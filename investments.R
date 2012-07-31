library("rmongodb")
library("plyr")
mongo <- mongo.create(db = "crunch")

# check connect
if (mongo.is.connected(mongo)) {
  print(mongo.count(mongo, "crunch.company"))
}

#####
# name
# field
# total_raised_amount
# funding_rounds[j]:
#     round_code
#     raised_amount
#     funded_year
#     investments:
#     who(company, financial_org, person
#     ).name
########

######
# для компаній,які отримали фінансування і бли засновані не раніше 2000 року
# отримати дані про раунди фінсування (тип раунду, залучена сума, джерело фінансування, рік)
#####

#######
# mongo query
# db.company.findOne(
# where  {"total_money_raised": {'$ne': "0$"}, "founded_year": {'$gte': 2000}}, 
# display fields
#  {  'name': true, // may be excluded 
#   'funding_rounds.funded_year': true
#   'funding_rounds.round_code':true, 
#   'funding_rounds.raised_amount': true,
#   'funding_rounds.investments.company.permalink': true, 
#   'funding_rounds.investments.person.permalink': true, 
#   'funding_rounds.investments.financial_org.permalink': true })
######
# db.company.findOne({"total_money_raised": {'$ne': "0$"}, "founded_year": {'$gte': 2000}}, {'name': true, 'funding_rounds.funded_year': true, 'funding_rounds.round_code':true, 'funding_rounds.raised_amount': true,'funding_rounds.investments.company.permalink': true, 'funding_rounds.investments.person.permalink': true, 'funding_rounds.investments.financial_org.permalink': true })
######

#####
# translate query to rmongodb:
#####
buf <- mongo.bson.buffer.create()
mongo.bson.buffer.start.object(buf, "total_money_raised")
mongo.bson.buffer.append(buf, "$ne", "$0")
mongo.bson.buffer.finish.object(buf)
mongo.bson.buffer.start.object(buf, "founded_year")
mongo.bson.buffer.append(buf, "$gte", 2000)
mongo.bson.buffer.finish.object(buf)
query <- mongo.bson.from.buffer(buf)
#####

#####
# translate fields  to display from mongo to rmongodb:
#####
buf <- mongo.bson.buffer.create()
mongo.bson.buffer.append(buf, "name", "true")
mongo.bson.buffer.append(buf, "category_code", "true")
mongo.bson.buffer.append(buf, "funding_rounds.round_code", "true")
mongo.bson.buffer.append(buf, "funding_rounds.funded_year", "true")
mongo.bson.buffer.append(buf, "funding_rounds.raised_amount", "true")
mongo.bson.buffer.append(buf, "funding_rounds.investments.company.permalink", "true")
mongo.bson.buffer.append(buf, "funding_rounds.investments.financial_org.permalink", "true")
mongo.bson.buffer.append(buf, "funding_rounds.investments.person.permalink", "true")
fields <- mongo.bson.from.buffer(buf)

########

count <- mongo.count(mongo, "crunch.company", query)

######## functions


unwind <- function(x){
# in this function we unrolled data about funding round and investors at this round:
  round <- x  
    
  code_type <- round$"round_code"   #extact round_code - character
  raised_amount <- round$"raised_amount" # extract raised_amount - numeric
  funded_year <- round$"funded_year"  # extract funded_year - numeric
  
  # catch uncleaned data
  if (length(code_type) == 0) {
    code_type <- "no code"
    print("code_type is null")
    } 
  if (length(raised_amount) == 0) {
    raised_amount <- 0
    print("raised_amount is null")
    }
  if (length(funded_year) == 0) {
    funded_year <- "unknown year"
    print("funded_year is null") 
    }
  
  number_investors <- length (round$"investments") # count number of investors
  # if we have no data about investors fill data by "NA"
  if (number_investors == 0) {type_of_investors <- as.data.frame(cbind(id ="NA", investor_type = "NA", investor_name ="NA"))}
  # expand data about investors
  else {investors <- round$"investments"# get list of investors
        # transform list of investor into useful dataframe
        investors1 <- ldply(investors, names) # get types of investors
        colnames(investors1) <- c("id", "investor_type")  
        get_investors_name <- function(x) x[[1]][[1]][[1]] ## get name of investor
        investors2 <- ldply(investors, get_investors_name)  
        colnames(investors2) <- c("id", "investor_name")
        type_of_investors <- merge(investors1, investors2, by = "id" ) 
  }
  # combine every record of investors with funding round information:
      unwind1 <- function(x) {cbind(code_type, raised_amount, funded_year, number_investors, x)} 
      round_df <- ddply(type_of_investors, .(id), .fun = unwind1)
  #######
  return(round_df)
}
############
# at list company_list we will collect all our unrolled data
company_list <- list()
j <- 1

#find fields of records in db "crunch", collection "company" that match the query
cursor <- mongo.find(mongo, "crunch.company", query=query, field=fields, limit = count)

#extract data
while (mongo.cursor.next(cursor)){
  b <- mongo.cursor.value(cursor)   
  name <- mongo.bson.value (b, "name") # extract name, character
  category_code <- mongo.bson.value (b, "category_code") # extarct category_code, character
  print(name) # we have fun when see how quick our data expands
  ## catch uncleaned data
  if (length(name) == 0) {print("name is null")}  
  if (length(category_code) == 0) {
    category_code <- "unknown category"
    print("code is null")
    }
  
  funding_rounds <- mongo.bson.value (b, "funding_rounds")  # extract funding rounds, list with nested list
  number_of_rounds <- length(funding_rounds) # count rounds
  
  # expand rounds
  if (number_of_rounds > 0) {
    # expand first round
      all_rounds <- unwind(funding_rounds[[1]]) 
        if (number_of_rounds > 1){
          # expand follow rounds 
        i <- 2
        for (i in 2:number_of_rounds){
          df_rounds <- unwind(funding_rounds[[i]])
          # combine rounds data
          all_rounds <- rbind(all_rounds, df_rounds)       
        }
      }
      
     add_cat_and_name <- function(x){cbind(name, category_code, x)} # add plane data for every round record
     all_rounds <- ddply(all_rounds, .(code_type, id), .fun = add_cat_and_name)  # apply adding category code and name to every round record
     company_list[[j]] <- all_rounds # add data to list
     j <- j + 1
     
  }
}

result <- ldply(company_list) # convert result list of dataframes to dataframe
write.csv(result, file = "crunchbasedata.csv")
################################################################################
