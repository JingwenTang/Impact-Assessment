rm(list = ls())
library(openxlsx)
library(ggplot2)
dat = read.xlsx("/Users/tangjingwen/Dropbox/jingwen_tasks/organized/data/Black Gram (Urd Beans)/13-17 month/Black Gram (Urd Beans) eleventh data.xlsx")
dat$month = as.Date(dat$month-25569,origin = "1970-01-01")
inkar = dat[dat$state=="karnataka",]
notinkar = dat[dat$state!="karnataka",]
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
interdate = as.Date(as.numeric(as.character(intervention$X4))-25569,origin = "1970-01-01")
interdate = as.Date(cut(interdate,breaks = "month"))

ind = intervented%in%inkar$market
id = min(interdate[ind])

a = aggregate(notinkar$modal,by = list(notinkar$month),mean,na.rm = T)
ggplot(inkar,aes(x = month,y = modal))+
  geom_line(aes(group = market,color = market))+
  geom_vline(xintercept = id)+
  geom_line(data = a,aes(x = month,y = modal))


colnames(a) = c("month","modal")
ggplot(a,aes(x = month,y = modal))+
  geom_line()

dat = read.xlsx("/Users/tangjingwen/Dropbox/jingwen_tasks/organized/data/Paddy(Dhan)/13-17 month/Paddy(Dhan) eleventh data.xlsx")
dat$month = as.Date(dat$month-25569,origin = "1970-01-01")
inkar = dat[dat$state=="karnataka",]
ind = intervented%in%inkar$market
id = min(interdate[ind])

ggplot(inkar,aes(x = month,y = modal))+
  geom_line(aes(group = market,color = market))+
  geom_vline(xintercept = id)
