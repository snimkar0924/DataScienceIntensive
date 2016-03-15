
#rm(list=ls())
# Load the dplyr package
library(dplyr)

##data are stored in <root>/GIT-REPOS/DataScienceIntensive/data
dataDir <- "../../../data"
datFl<-file.path(dataDir,"WA_Retail-SalesMarketing_-ProfitCost.zip")

salesData<-read.csv(unz(datFl, "WA_Retail-SalesMarketing_-ProfitCost.csv"))
dim(salesData)
colnames(salesData)

salesData<-tbl_df(salesData)
salesData
glimpse(salesData)
attach(salesData)

dfHier<-salesData %>% filter(Retailer.country=='United States')
dfHier<-dfHier %>% group_by(Year,
                            Product.line,
                            Product.type,
                            Product)
##### select the product here....
df1<- dfHier %>% filter(Product=='TrailChef Cook Set')
df1<-arrange(df1,Unit.sale.price)

par(mfrow=c(2,2))
#2004
temp<-filter(df1, Year=='2004')
plot(x=temp$Unit.sale.price, y=temp$Quantity, type='h', col='blue',
     main="2004")
#2005
temp<-filter(df1, Year=='2005')
plot(x=temp$Unit.sale.price, y=temp$Quantity, type='h', col='blue',
     main="2005")
#2006
temp<-filter(df1, Year=='2006')
plot(x=temp$Unit.sale.price, y=temp$Quantity, type='h', col='blue',
     main="2006")
#2007
temp<-filter(df1, Year=='2007')
plot(x=temp$Unit.sale.price, y=temp$Quantity, type='h', col='blue',
     main="2007")
?plot
#unit sale price
inc<-0.5
fl<-floor(min(df1$Unit.sale.price))
cl<-ceiling(max(df1$Unit.sale.price))
breaks=seq(fl,cl, inc)
cuts<-cut(df1$Unit.sale.price, 
          breaks=breaks,
          include.lowest = TRUE)

par(mfrow=c(1,1))
plot(cuts)
tcuts<-table(cuts)
fcuts_sale_price<-as.data.frame(tcuts)
arrange(df1, df1$Unit.sale.price)


#unit price & discount
disc<-df1$Unit.price-df1$Unit.sale.price
mean(disc)
sd(disc) 
plot(x=disc, y=df1$Quantity, type='h', col='blue')
plot(x=disc, y=(df1$Unit.sale.price-df1$Unit.cost), type='h', col='green')

####### later
df1<-filter(salesData, Year=='2004' &
                       Retailer.country=='United States' &
                       Product.type == 'Lanterns')
dim(df1)
unique(df1$Product)


#sort by Quantity
df1<- df1 %>% group_by(Product) %>% arrange(Quantity)
#write.csv(df1,'Lanterns.csv')

par(mfrow=c(1,1))
plot(x=df1$Quantity, y=df1$Unit.sale.price, type='l', col='blue')


summary(salesData)

?cut
