source("investments.R")

library("lattice")
library("ggplot2")
#invest_companies <- (read.csv("crunchbasedata.csv"))
invest_companies <- result
#### we have 3 detalization level. We start with deepest - 3. At this level we have all data about funding rounds and investors.

## сфери, куди вкладають інвестори
## розподіл по типах інвесторів
## на яких етапах
##
## очистка даних
##
## кількість інвесторів на етапах?
##
## тип івестора / сфера
## тип інветора/ етап
## роки вкладання / типи івесторів

## сфера - кільксть грошей

# grant, seed, angel - field, who
names(result)
summary(result$code_type)


str(invest_companies)

# cleaning
type_of_inv <- c("person", "financial_org", "company")
invest_companies <- invest_companies[invest_companies$investor_type %in% type_of_inv, ]

###### by type of investor
#invest_companies$number_investors <- as.factor(invest_companies$number_investors)
#barplot(table(invest_companies$investor_type))

investorsdf <- invest_companies[ ,c("investor_type", "investor_name")]
investorsdf <- unique(investorsdf)
barplot(table(investorsdf$investor_type), main = "Types of investors")

types_of_investors <- aggregate(invest_companies$investor_type, by = list(invest_companies$investor_type, invest_companies$category_code), FUN = length)
colnames(types_of_investors) <- c("type", "field", "count")

barchart(types_of_investors$type ~ types_of_investors$count | types_of_investors$field,
         xlab = "Count", main = "Funding by field")

##### by number of investor
boxplot(invest_companies$number_investors, outline = F)
barplot(table(as.factor(invest_companies$number_investors)))

str(invest_companies)

summary(invest_companies$category_code)
summary(invest_companies$code_type)
summary(invest_companies$number_investors)

## get number of funding for every fundutor, group by type of investor(насправді не потрібно, трік)
count_of_investors <- aggregate(invest_companies$investor_type, by = list(invest_companies$investor_name,invest_companies$investor_type), FUN = length)
colnames(count_of_investors) <- c("name", "type", "count")

## sorting count_of_investors by dec
Ord <- order(count_of_investors$count, decreasing = TRUE)
count_of_investors <- count_of_investors[Ord, ]


count_of_persons <- count_of_investors[count_of_investors$type == "person", ]
count_of_company <- count_of_investors[count_of_investors$type == "company", ]
count_of_finorg <-  count_of_investors[count_of_investors$type == "financial_org", ]

fields <- aggregate (invest_companies$investor_name, by = list(invest_companies$category_code, invest_companies$investor_type), FUN = length)

count_rounds <- function(x){length(unique(x))}
count_of_rounds_by_fields <- aggregate (invest_companies$name, by = list(invest_companies$code_type, invest_companies$category_code), FUN = count_rounds)
count_of_rounds <- aggregate(invest_companies$code_type, by = list(invest_companies$name), FUN = count_rounds)

### go to detalization level 2

invest_companies2 <- invest_companies[ , c("name","category_code", "code_type", "raised_amount", "funded_year", "number_investors")]
invest_companies2 <- unique(invest_companies2)


summary(as.factor(invest_companies2$funded_year))
### cleaning data step-by-step
invest_companies2 <- invest_companies2[invest_companies2$funded_year >= 2000, ]
invest_companies2 <- invest_companies2[invest_companies2$funded_year != "unknown year", ]
invest_companies2 <- invest_companies2[!is.na(invest_companies2$funded_year), ]

str(invest_companies2)

invest_companies2$funded_year <- as.factor(invest_companies2$funded_year)
## to thousands $
invest_companies2$raised_amount <- invest_companies2$raised_amount / 1000

boxplot(invest_companies2$raised_amount ~ invest_companies2$funded_year, outline= T,
        xlab = "Year", ylab = "Raised amount (in thousands $)",
        main = "Raised by years")
boxplot(invest_companies2$raised_amount ~ invest_companies2$funded_year, outline= F,
        xlab = "Year", ylab = "Raised amount (in thousands $)",
        main = "Raised by years")
        )



##plot by type of funding round:

boxplot(invest_companies2$raised_amount ~ invest_companies2$code_type, outline= T,
        xlab = "", ylab = "Raised amount (in thousands $)",
        main = "Raised by funding rounds", las = 3)

boxplot(invest_companies2$raised_amount ~ invest_companies2$code_type, outline= F,
        xlab = "", ylab = "Raised amount (in thousands $)",
        main = "Raised by funding rounds", las = 3)

##### valley of death ######
inestments1 <- c("angel", "seed", "grant")
invest_companies2vd <- invest_companies2[invest_companies2$code_type %in% inestments1 , ]

invest_companies2$code_type <- as.factor(as.character(invest_companies2$code_type))
boxplot(invest_companies2$raised_amount ~ invest_companies2$code_type, outline= F)
boxplot(invest_companies2$raised_amount ~ invest_companies2$category_code, outline= F)
#### early stage ######
inestments2 <- c("a", "b")
invest_companies2ab <- invest_companies2[invest_companies2$code_type %in% inestments2 , ]
#### later stage ######
inestments3 <- c("c", "d", "e", "f")
invest_companies2cd <- invest_companies2[invest_companies2$code_type %in% inestments3 , ]
#### ipo ####
# ??????
#############