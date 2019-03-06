rm(list = ls())
setwd("/Users/tangjingwen/Dropbox/jingwen_tasks/organized/plots/10-17")
library(ggplot2)
library(scales)
library(openxlsx)
library(reshape2)
library(plyr)
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

intervention = intervention[,c(3,4)]
colnames(intervention) = c("market","date")
intervention$date = as.Date(as.numeric(intervention$date)-25569,origin = "1970-01-01")

Product = c("Groundnut","Jowar(Sorghum)","Maize","Sunflower","Arhar (Tur-Red Gram)","Cotton","Green Gram (Moong)","Arecanut(Betelnut-Supari)","Bengal Gram(Gram)","Black Gram (Urd Beans)","Dry Chillies","Copra","Kulthi(Horse Gram)","Paddy(Dhan)")
wb = createWorkbook() 
for (p in 1:length(Product)){
  product = Product[p]
  dat = c()
  for (i in 2010:2017){
    filename = paste("/Users/tangjingwen/Dropbox/jingwen_tasks/Organized Data/",i,"_Data/",product,".xlsx",sep = "")
    print(i)
    mydf <- read.xlsx(filename, sheet = 1, startRow = 1, colNames = F)
    marketrow = (1:nrow(mydf))[!is.na(mydf$X1)]
    concluderow = (1:nrow(mydf))[(is.na(mydf$X2))&(is.na(mydf$X1))]
    for (j in 1:(length(marketrow)-1)){
      mydf[(marketrow[j]+1):(marketrow[j+1]-2),"X1"] = mydf[marketrow[j],"X1"]
    }
    mydf[(marketrow[length(marketrow)]+1):(nrow(mydf)-1),"X1"] = mydf[marketrow[length(marketrow)],"X1"]
    mydf = mydf[-c(marketrow,concluderow),]
    mydf$X3 = as.numeric(mydf$X3)
    for (j in 5:7){
      mydf[,j] = as.numeric(mydf[,j])
    }
    dat = rbind(dat,mydf)
  }
  colnames(dat) = c("market","date","amount","type","minp","maxp","modal")
  dat$market = tolower(dat$market)
  dat$date = as.Date(dat$date,format = "%d/%m/%Y")
  dat$date = as.Date(cut(dat$date,breaks = "week"))
  dat = dat[!is.na(dat$modal),]
  dat = dat[!is.na(dat$amount),]
  
  dat$amount = as.numeric(as.character(dat$amount))
  dat$modal = as.numeric(as.character(dat$modal))
  newd = c()
  for (i in 1:length(unique(dat$market))){
    temp = dat[dat$market==unique(dat$market)[i],]
    temp$amount[((temp$amount<quantile(temp$amount,probs = c(0.01,0.95),na.rm = T)[1])|(temp$amount>quantile(temp$amount,probs = c(0.01,0.95),na.rm = T)[2]))] = -100
    temp$modal[((temp$modal<quantile(temp$modal,probs = c(0.01,0.99),na.rm = T)[1])|(temp$modal>quantile(temp$modal,probs = c(0.01,0.99),na.rm = T)[2]))] =-100
    temp = temp[temp$amount!=-100,]
    temp = temp[temp$modal!=-100,]
    #print(nrow(temp))
    newd = rbind(newd,temp)
  }
  dat = newd
  ###there are markets have na in all days of a year so that it will be organized to an empty row in the dat and we need to remove them
  dat = dat[!is.na(dat$market),]

  dat$treat = dat$market%in%intervented
  wm = ddply(dat,.(date,treat),summarise, wmean = mean(modal))
  wm$treat[wm$treat == T] = "treated"
  wm$treat[wm$treat == F] = "untreated"
  interday = min(as.Date(intervention[intervention$market%in%dat$market,"date"]))
  
  
  addWorksheet(wb,product)
  
  
  writeData(wb,sheet = product,wm)
  
  
  pdf(paste(product," 10-17.pdf"),width = 14,height = 8)
  print(ggplot(data = wm)+
    geom_line(aes(x = date,y = wmean,group = treat,color = treat))+
    geom_vline(xintercept = interday))
  dev.off()
  
}


saveWorkbook(wb,paste("/Users/tangjingwen/Dropbox/jingwen_tasks/organized/result/DiD/10-17 all.xlsx",sep = ""),overwrite = T)



