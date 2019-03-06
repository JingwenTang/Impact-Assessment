rm(list = ls())
library(openxlsx)
library(lubridate)

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
intervented2 = intervented
interdate2 = interdate


notr = data.frame(date = as.Date(as.Date("2014-01-01"):as.Date("2017-12-31"),origin = "1970-01-01"))
count = 0

for (l in 1:nrow(notr)){
  temp = sum(notr$date[l]==interdate2)
  count = count + temp
  notr[l,"no.inter"] = count
  tempmar = intervented2[which(notr$date[l]==interdate2)]

}
notr$month = cut(notr$date,br = "month")
notr$no.inter = as.numeric(notr$no.inter)
notr$month = as.Date(notr$month)
ggplot(notr,aes(x = month,y = no.inter))+
  geom_line()+
  ggtitle("Plot of number of markets active in Karnataka")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.text = element_text(size=20),axis.title = element_text(size=20),plot.title = element_text(size = 20))
