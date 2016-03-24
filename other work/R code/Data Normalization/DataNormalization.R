


##rm(list=ls())

library(dplyr)

##read the sales data
salesData<-tbl_df(read.csv("../data/sales_data.csv"))

products<-unique(salesData$Product)

prd1<-salesData %>% filter(Retailer_country=='United States' &
                           Product_line=='Personal Accessories' & 
                           Product_type=='Watches' &
                           Product=='Mountain Man Analog')

## http://www.r-bloggers.com/r-tutorial-series-centering-variables-and-generating-z-scores-with-the-scale-function/
scRevenue<-scale(prd1$Revenue, center=T, scale=T)
scQuantity<-scale(prd1$Quantity, center=T, scale=T)
# mean(prd1$Quantity)
# sd(prd1$Quantity)


prd1<-cbind.data.frame(prd1,scRevenue)


