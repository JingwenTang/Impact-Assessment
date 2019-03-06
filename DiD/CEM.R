rm(list = ls())
library(openxlsx)
library(MatchIt)
library(cem)
clx <-function(fm, dfcw, cluster){
  library(sandwich)
  library(lmtest)
  M <- length(unique(cluster))
  N <- length(cluster)
  dfc <- (M/(M-1))*((N-1)/(N-fm$rank))
  u <- apply(estfun(fm),2,function(x) tapply(x, cluster, sum))
  vcovCL <- dfc*sandwich(fm, meat=crossprod(u)/N)*dfcw
  coeftest(fm, vcovCL) }
intervention= as.data.frame(read.xlsx("/Users/tangjingwen/Dropbox/jingwen_tasks/infrastructure_characteristics/Market Details.xlsx",sheet = "Market Live Dates"))[3:160,]
intervention$X3 = tolower(as.character(intervention$X3))

temp = read.csv("/Users/tangjingwen/Dropbox/jingwen_tasks/CombinedArrivals/intervented change to organized.csv",header = F)
colnames(temp) = c("int","org")
temp$int = as.character(temp$int)
temp$org = as.character(temp$org)
inind = intervention$X3%in%temp$int
orind = match(intervention$X3[inind],temp$int)
intervention$X3[inind] = temp$org[orind]
intervented = tolower(as.character(intervention$X3))
intervented = c(intervented,"virtual")
interdate = as.Date(as.numeric(as.character(intervention$X4))-25569,origin = "1970-01-01")
interdate = as.Date(cut(interdate,breaks = "month"))
Product = c("Groundnut","Jowar(Sorghum)","Maize","Sunflower","Arhar (Tur-Red Gram)","Cotton","Green Gram (Moong)","Arecanut(Betelnut-Supari)","Bengal Gram(Gram)","Black Gram (Urd Beans)","Dry Chillies","Copra","Kulthi(Horse Gram)","Paddy(Dhan)")
wb = createWorkbook() 
for (p in 1:length(Product)){
product = Product[p]
marketdat = read.xlsx(paste("/Users/tangjingwen/Dropbox/jingwen_tasks/organized/data/",product,"/13-17 month/",product," eleventh data.xlsx",sep = ""),sheet = 1)
marketdat$treat = marketdat$state=="karnataka"
marketdat = na.omit(marketdat)
m.out <- matchit(treat ~ annual + total_ar , data = marketdat, method = "cem")
m.data <- match.data(m.out)
summary(m.data)
colnames(m.data)[1] = "date"

marketdat = m.data

temp = intervented%in%unique(marketdat$market)
intervented1 = intervented[temp]
interdate1 = interdate[temp]
marketdat$date = as.Date(marketdat$date-25569,origin = "1970-01-01")
for (i in 1:length(intervented1)){
  temp = marketdat[marketdat$market==intervented1[i],]
  marketdat[marketdat$market==intervented1[i]&(marketdat$date>=(interdate1[i]+0)),"inter"] = 1
  
}
d = marketdat
d = d[!is.na(d$Per.Capita.GSDP),]
d = d[!is.na(d$production),]
d = d[!is.na(d$yield),]
d = d[!is.na(d$amount),]
d = d[!is.na(d$inter),]
for (l in 20:32){
  d = d[!is.na(d[,l]),]
}
d$date = as.factor(d$date)
fit1 = lm(modal~ market+date+
            Per.Capita.GSDP+production+yield+
            amount+ rainfall+rainfall_lead1+rainfall_lead2+rainfall_lead3+rainfall_lead4
          +rainfall_lead5+rainfall_lead6+rainfall_lead7+rainfall_lead8+rainfall_lead9
          +rainfall_lead10+rainfall_lead11+rainfall_lead12+inter,
          data=d)

crse1 = clx(fit1,1, d$market)
crse1
write.csv(crse1,paste("/Users/tangjingwen/Dropbox/jingwen_tasks/organized/result/DiD/CEM/",product,".csv",sep = ""))

}
