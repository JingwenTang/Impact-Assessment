library(openxlsx)
library(ggplot2)
library(reshape2)
Product = c("Groundnut","Jowar(Sorghum)","Maize","Sunflower","Arhar (Tur-Red Gram)","Cotton","Green Gram (Moong)","Arecanut(Betelnut-Supari)","Bengal Gram(Gram)","Black Gram (Urd Beans)","Dry Chillies","Copra","Kulthi(Horse Gram)","Paddy(Dhan)")
allnumber = c()
number = c()
percentage = c()
allpercentage = c()
for (p in 1:length(Product)){
  alldat = read.xlsx(paste("/Users/tangjingwen/Dropbox/jingwen_tasks/organized/data/",Product[p],"/13-17 month all/data details.xlsx",sep = ""),sheet = 1)
  dat = read.xlsx(paste("/Users/tangjingwen/Dropbox/jingwen_tasks/organized/data/",Product[p],"/13-17 month/data details.xlsx",sep=""),sheet = 1)
  allnumber = rbind(allnumber,alldat[10,3:4])
  allpercentage = rbind(allpercentage,alldat[10,9:10])
  number = rbind(number,dat[10,3:4])
  percentage = rbind(percentage,dat[10,9:10])
}

colnames(allnumber) = c("treated","untreated")
colnames(number) = c("treated","untreated")
colnames(allpercentage) = c("treated","untreated")
colnames(percentage) = c("treated","untreated")
allnumber$commodity = Product
number$commodity = Product
allpercentage$commodity = Product
percentage$commodity = Product
allnumber = melt(allnumber,id.vars = "commodity")
number = melt(number,id.vars = "commodity")
allpercentage = melt(allpercentage,id.vars = "commodity")
percentage = melt(percentage,id.vars = "commodity")
colnames(allnumber) = c("commodity","treated","market_numbers")
colnames(number) = c("commodity","treated","market_numbers")
colnames(percentage) = c("commodity","treated","percentage")
colnames(allpercentage) = c("commodity","treated","percentage")
setwd("/Users/tangjingwen/Dropbox/jingwen_tasks/organized/plots/DiD data counting")
pdf("number of markets.pdf",width = 13,height = 4)
ggplot(allnumber, aes(commodity, market_numbers,fill = treated)) + 
  geom_bar(position = 'dodge', stat="identity") +
  geom_text(aes(label=market_numbers), position = position_dodge(width = 0.9), vjust = -0.25,size=3)+
  theme(text = element_text(size=8,face = "bold"))
dev.off()
pdf("portion of markets.pdf",width = 13,height = 4)
ggplot(allpercentage, aes(commodity, percentage,fill = treated)) + 
  geom_bar(position = 'dodge', stat="identity") +
  geom_text(aes(label=paste(round(percentage*100,3),"%",sep = "")), position = position_dodge(width = 0.9), vjust = -0.25,size=3)+
  theme(text = element_text(size=8,face = "bold"))
dev.off()
pdf("number of markets top 90 percent selected.pdf",width = 13,height = 4)
ggplot(number, aes(commodity, market_numbers,fill = treated)) + 
  geom_bar(position = 'dodge', stat="identity") +
  geom_text(aes(label=market_numbers), position = position_dodge(width = 0.9), vjust = -0.25,size=3)+
  theme(text = element_text(size=8,face = "bold"))
dev.off()
pdf("portion of markets top 90 percent selected.pdf",width = 13,height = 4)
ggplot(percentage, aes(commodity,percentage,fill = treated)) + 
  geom_bar(position = 'dodge', stat="identity") +
  geom_text(aes(label=paste(round(percentage*100,3),"%",sep = "")), position = position_dodge(width = 0.9), vjust = -0.25,size=3)+
  theme(text = element_text(size=8,face = "bold"))
dev.off()




write.xlsx(number,"/Users/tangjingwen/Dropbox/jingwen_tasks/organized/data/number_markets.xlsx")
a = read.xlsx("/Users/tangjingwen/Dropbox/jingwen_tasks/organized/result/DiD/top_90_result_summary.xlsx",sheet = 1)
ne = cbind(number$commodity[1:14],number$market_numbers[1:14],number$market_numbers[15:28])
colnames(ne) = c("commodity","treated markets","untreated markets")
library(plyr)
ne = as.data.frame(ne)
a = join(a,ne,by = "commodity",type = "left")

write.xlsx(a,"/Users/tangjingwen/Dropbox/jingwen_tasks/organized/result/DiD/top_90_result_summary.xlsx")

a = c()
r = c()
library(lubridate)
for (p in 1:length(Product)){
  dat = read.xlsx(paste("/Users/tangjingwen/Dropbox/jingwen_tasks/organized/data/",Product[p],"/13-17 month/",Product[p]," eleventh data.xlsx",sep=""),sheet = 1)
  dat$month = as.Date(dat$month-25569,origin = "1970-01-01")
  dat = dat[dat$state=="karnataka",]
  dat = dat[,c(1,2,3,6)]
  dat$year = year(dat$month)
  arrivals = aggregate(dat$amount,by = list(dat$year),sum,na.rm = T)
  arrivalsm = mean(arrivals$x)
  dat$revenue = dat$amount*dat$modal
  revenue = aggregate(dat$revenue,by = list(dat$year),sum,na.rm = T)
  revenuem = mean(revenue$x)
  a = c(a,arrivalsm)
  r = c(r,revenuem)
}
dat = cbind(Product,round(a,0))
dat = cbind(dat,round(r,0))
colnames(dat) = c("commodity","karnataka arrivals","karnataka revenue")
dat = as.data.frame(dat)
a = read.xlsx("/Users/tangjingwen/Dropbox/jingwen_tasks/organized/result/DiD/top_90_result_summary.xlsx")
a = join(a,dat,by = "commodity",type = "left")
write.xlsx(a,"/Users/tangjingwen/Dropbox/jingwen_tasks/organized/result/DiD/top_90_result_summary.xlsx")


rm(list = ls())
library(openxlsx)
library(ggplot2)
library(reshape2)
Product = c("Groundnut","Jowar(Sorghum)","Maize","Sunflower","Arhar (Tur-Red Gram)","Cotton","Green Gram (Moong)","Arecanut(Betelnut-Supari)","Bengal Gram(Gram)","Black Gram (Urd Beans)","Dry Chillies","Copra","Kulthi(Horse Gram)","Paddy(Dhan)")
allnumber = c()
number = c()
percentage = c()
allpercentage = c()
for (p in 1:length(Product)){
  dat = read.xlsx(paste("/Users/tangjingwen/Dropbox/jingwen_tasks/organized/data/",Product[p],"/13-17 month/",Product[p]," eleventh data.xlsx",sep=""),sheet = 1)
  if ("sri har gobindpur"%in%dat$market) print(Product[p])
  dat1 = dat[dat$state=="karnataka",]
  dat2 = dat[dat$state!="karnataka",]
  s1 = sum(dat1$amount,na.rm = T)
  s2 = sum(dat2$amount,na.rm = T)
  number = rbind(number,c(s1,s2))
  percentage = rbind(percentage,s1/(s1+s2))
}

colnames(number) = c("treated","untreated")
colnames(percentage) = c("treated amount portion")
number = as.data.frame(number)
percentage = as.data.frame(percentage)
number$commodity = Product
percentage$commodity = Product
number = melt(number,id.vars = "commodity")
#percentage = melt(percentage,id.vars = "commodity")
colnames(number) = c("commodity","treated","quantity")
colnames(percentage) = c("treated_quantity_percentage","commodity")
a = read.xlsx("/Users/tangjingwen/Dropbox/jingwen_tasks/organized/result/DiD/top_90_result_summary.xlsx")
#a = join(a,number,by = "commodity",type = "left")
a = join(a,percentage,by = "commodity",type = "left")
write.xlsx(a,"/Users/tangjingwen/Dropbox/jingwen_tasks/organized/result/DiD/top_90_result_summary.xlsx")

setwd("/Users/tangjingwen/Dropbox/jingwen_tasks/organized/plots/DiD data counting")

pdf("arrival quantities top 90 percent selected.pdf",width = 13,height = 4)
ggplot(number, aes(commodity, quantity,fill = treated)) + 
  geom_bar(position = 'dodge', stat="identity") +
  geom_text(aes(label=round(quantity,0)), position = position_dodge(width = 0.9), vjust = -0.25,size=3)+
  theme(text = element_text(size=8,face = "bold"))
dev.off()
pdf("portion of arrival quantity in treated markets top 90 percent selected.pdf",width = 13,height = 4)
ggplot(percentage, aes(commodity,treated_quantity_percentage)) + 
  geom_bar(position = 'dodge', stat="identity") +
  geom_text(aes(label=paste(round(treated_quantity_percentage*100,3),"%",sep = "")), position = position_dodge(width = 0.9), vjust = -0.25,size=3)+
  theme(text = element_text(size=8,face = "bold"))
dev.off()

p=14
dat = read.xlsx(paste("/Users/tangjingwen/Dropbox/jingwen_tasks/organized/data/",Product[p],"/13-17 month/",Product[p]," eleventh data.xlsx",sep=""),sheet = 1)
dat$month = as.Date(dat$month-25569,origin = "1970-01-01")

