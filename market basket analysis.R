#####Radmila Velickovic###
######market basket analysis#####
#reshaping data 

#select the columns you need
str(alc)
mba<-select(alc, ID,9:31)
View(mba)

#reshaping from wide to long form

library(reshape2)
mba1<-melt(mba, id.vars=c("ID"))
dim(mba1)
View(mba1)
table(mba1$value)

#mba now
#install.packages("arules")
library(arules)
#individual products that belong to each transaction are aggregated across records 
#into a single record as an array of products. 
i <- split(mba1$variable, mba1$ID)
View(i) 
head(i)

txn <- as(i, "transactions")
basket_rules <- apriori(txn, parameter = list(sup = 0.005, conf = 0.01, target="rules"))         
inspect(basket_rules)