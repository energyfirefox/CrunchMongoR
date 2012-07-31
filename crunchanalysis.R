library("RMongo")
mongo <- mongoDbConnect("crunch")
collections <- dbShowCollections(mongo)


#### get companies> which was founded 2000-2011
opened<- dbGetQueryForKeys(mongo, "company", "{'founded_year': {'$gte':2000, '$lt':2012}}", "{'name': 1, 'founded_year':1}", 0, 95000)
barplot(table(opened$founded_year), main = "Founded company", xlab = "Year", ylab = "Quantity" )

#### get previous + fieldsfor companies
openedfields <- dbGetQueryForKeys(mongo, "company", "{'founded_year': {'$gte':2000, '$lt':2012}}", "{'name': 1, 'founded_year':1, 'category_code':1}", 0, 95000)

#### aggegate by field ad year
fieldsbyyear <- aggregate(openedfields$name, by = list(openedfields$founded_year, openedfields$category_code), FUN = length)
names(fieldsbyyear) <- c("year", "field", "count")
fieldsbyyear <- fieldsbyyear[!(fieldsbyyear$field == ""), ]
fieldsbyyear$year <- as.factor(fieldsbyyear$year)
str(fieldsbyyear)
#### plot in lattice
library(lattice)
xyplot(fieldsbyyear$count ~ fieldsbyyear$year| fieldsbyyear$field, 
       type = "b",
       xlab = "Year", ylab = "Companies", main = "By Fields")

barchart(fieldsbyyear$year ~ fieldsbyyear$count| fieldsbyyear$field, 
         type = "b",
         xlab = "Companies", ylab = "Year", main = "By Fields")
#### plot in ggplot
library(ggplot2)
qplot(Group.1, X_id, data = fieldsbyyear, facets = Group.2 ~. , geom = "path")
qplot(Group.1, X_id, data = fieldsbyyear, colour = Group.2, geom = c("point", "smooth"))


############################################# end of research ###########################################################

###### test data
openedcompany<- dbGetQueryForKeys(mongo, "company", "{'founded_year': {'$gte':2000, '$lt':2012}}", "{'name': 1, 'founded_year':1, 'deadpooled_year':1, 'category_code':1,  'rank': 1 }" , 0, 95000)
closedcompany <- dbGetQueryForKeys(mongo, "company", "{'deadpooled_year': {'$gte':2000, '$lt':2012}}", "{'name': 1, 'founded_year':1, 'deadpooled_year':1, 'category_code':1,  'rank': 1 }" , 0, 95000)

str(openedcompany)

barplot(table(openedcompany$founded_year))
barplot(table(openedcompany$deadpooled_year))
barplot(table(closedcompany$founded_year))
barplot(table(closedcompany$deadpooled_year))

summary(openedcompany$deadpooled_year)
table(openedcompany$category_code)

webcompany <- openedcompany[ openedcompany$category_code == 'web' , c('name' , 'founded_year', 'deadpooled_year')]
barplot(table(webcompany$founded_year))
softwarecompany <- openedcompany[ openedcompany$category_code == 'software' , c('name' , 'founded_year', 'deadpooled_year')]
barplot(table(softwarecompany$founded_year))
ecomcompany <- openedcompany[ openedcompany$category_code == 'ecommerce' , c('name' , 'founded_year', 'deadpooled_year')]
barplot(table(ecomcompany$founded_year))



#display together all opened in 2000-2012 company
#dispaly months? (why?)
###############################################################

##### plot all fieldss in one plot
palette.lty <- function(lvl, colors) {
  nlevels <- length(unique(lvl))
  ncolors <- min(nlevels, length(colors))
  return (list(col=colors[1:ncolors],
               lty=sort(rep(1:ceiling(nlevels/ncolors), ncolors))[1:nlevels]))
}

palette <- palette.lty(fieldsbyyear$Group.2,
                       trellis.par.get("superpose.line")$col)


xyplot(X_id ~ Group.1, groups=Group.2, data=fieldsbyyear,
       panel=panel.superpose, panel.groups=panel.loess,
       lwd=2, alpha=0.9, col=palette$col, lty=palette$lty,
       key=list(columns=1, space="right",
                lines=list(lwd=2, alpha=0.9, col=palette$col, lty=palette$lty),
                title="Предметная область",
                text=list(unique(fieldsbyyear$Group.2)))
)

