#with rmongodb

library("rmongodb")
mongo <- mongo.create(db = "crunch")
# check connect
if (mongo.is.connected(mongo)) {
  print(mongo.count(mongo, "crunch.company"))
}

#mastering query: select ... where "total_money_raised" != 0
buf <- mongo.bson.buffer.create()
mongo.bson.buffer.start.object(buf, "total_money_raised")
mongo.bson.buffer.append(buf, "$ne", "$0")
mongo.bson.buffer.finish.object(buf)
query <- mongo.bson.from.buffer(buf)

count <- mongo.count(mongo, "crunch.company", query)
count

# selecting fields to display
buf <- mongo.bson.buffer.create()
mongo.bson.buffer.append(buf, "funding_rounds.funded_year", "true")
mongo.bson.buffer.append(buf, "funding_rounds.round_code", "true")
mongo.bson.buffer.append(buf, "funding_rounds.raised_amount", "true")
mongo.bson.buffer.append(buf, "raised_currency_code", "true")
fields <- mongo.bson.from.buffer(buf)

#create empty list
list1 <- list()
comp_list <- list()

####### example
count <- mongo.count(mongo, ns, query)

cursor <- mongo.find(mongo, query)
list1$name[] <- vector("character", count)
age <- vector("numeric", count)
i <- 1
while (mongo.cursor.next(cursor)) {
  b <- mongo.cursor.value(cursor)
  name[i] <- mongo.bson.value(b, "name")
  age[i] <- mongo.bson.value(b, "age")
  i <- i + 1
}
df <- as.data.frame(list(name=name, age=age))
#######

#put result of query to list1

#while (mongo.cursor.next(cursor)){
#    list1 <- (mongo.bson.to.list(mongo.cursor.value(cursor)))   
#    xret <- as.data.frame(t(sapply(comp_list, rbind)))
#}
xret <- data.frame()

cursor <- mongo.find(mongo, "crunch.company", query=query, field=fields, limit = 93000)
while (mongo.cursor.next(cursor)){
  fun_rounds <- mongo.cursor.value(cursor)
  b <- mongo.bson.value(fun_rounds, "funding_rounds")
  xret1 <- as.data.frame(t(sapply(b[[1]], rbind)))
  xret <- rbind(xret, xret1)
#  bb <- b[[1]]
  
#  print(bb[['funded_year']])
#  print(bb[['round_code']])
#  print(bb[['raised_amount']])
 
}

funded <- xret
funded$round_code <- as.character(funded$round_code)
funded$funded_year <- as.numeric(as.character(funded$funded_year))
funded$raised_amount <- as.numeric(as.character(funded$raised_amount))

#funded$funded_year <- as.Date(funded$funded_year, "%Y")
funded <- funded[funded$funded_year >= 2000, ]
funded <- funded [funded$round_code != "post_ipo_debt", ]
funded <- funded [funded$round_code != "post_ipo_equity", ]

funded$raised_amount <- funded$raised_amount/1000

ag_funding_rounds <- aggregate(funded$raised_amount, by = list(funded$round_code, funded$funded_year), FUN = mean)
library("lattice")

library("ggplot2") 
xyplot(ag_funding_rounds$x~ag_funding_rounds$Group.2 | ag_funding_rounds$Group.1,
       type = "b", main ="Кількість інвестицій по раундах", xlab = "Рік", ylab = "Сума")

qplot(funded$funded_year, funded$raised_amount, geom = c("boxplot", "jitter"), colour = funded$round_code, na.rm =F)

summary(funded$raised_amount)
summary(funded$funded_year)
year()
str(funded)


mongo.disconnect(mongo)


##########
who invests, company, person or finansial org?
mongo.bson.buffer.append(buf, "funding_rounds.investments.company.permalink", "true")
mongo.bson.buffer.append(buf, "funding_rounds.investmens.financial_org.permalink", "true")
mongo.bson.buffer.append(buf, "funding_rounds.investmens.person.permalink", "true")

#########

# mastering test query: select ... where name == 'Facebook'
buf <- mongo.bson.buffer.create()
mongo.bson.buffer.append(buf, "name", "MagneticOne")
query <- mongo.bson.from.buffer(buf)


buf <- mongo.bson.buffer.create()
mongo.bson.buffer.append(buf, "funding_rounds.round_code", "angel")
query <- mongo.bson.from.buffer(buf)

#mastering query: select 'name '  where query
buf <- mongo.bson.buffer.create()
mongo.bson.buffer.append(buf, "name", "true")
fields <- mongo.bson.from.buffer(buf)

# test - findOne
b <- mongo.find.one(mongo, "crunch.company", query, fields)
if (!is.null(b))
  print(b)

bwplot(funded$raised_amount ~ funded$funded_year | funded$round_code, layout = c(11, 1))
boxplot(funded$raised_amount ~ funded$funded_year, outline = FALSE)
bwplot(funded$raised_amount ~ funded$funded_year, outline = FALSE)
library(lattice) 
attach(mtcars)
# create factors with value labels 
gear.f<-factor(gear,levels=c(3,4,5),
               labels=c("3gears","4gears","5gears")) 
cyl.f <-factor(cyl,levels=c(4,6,8),
               labels=c("4cyl","6cyl","8cyl")) 
# boxplots for each combination of two factors 
bwplot(cyl.f~mpg|gear.f,
       ylab="Cylinders", xlab="Miles per Gallon", 
       main="Mileage by Cylinders and Gears", 
       layout=(c(3,1))
       
       # scatterplots for each combination of two factors 
       xyplot(mpg~wt|cyl.f*gear.f, 
              main="Scatterplots by Cylinders and Gears", 
              ylab="Miles per Gallon", xlab="Car Weight")
       
