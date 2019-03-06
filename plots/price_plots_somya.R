#this file prints out the dates on which first, half, and last market was implemented

rm(list = ls())
library(readr)
library(openxlsx)
library(plyr)
library(dplyr)
library(Synth)
library(ggplot2)
library(graphics)
library(qpcR)
library(MASS)
library(ROCR)
library(plyr)
library(stargazer)
library(miceadds)
library(multiwayvcov)
library(radiant.data)
library(stringi)
library(lubridate)
Timemode = c("10-17 week all") #the time unit and lags chosen for the model in synthetic control
br="month"


Product = c("Groundnut","Maize","Arhar (Tur-Red Gram)","Cotton","Green Gram (Moong)","Paddy(Dhan)") 
#Product = c("Green Gram (Moong)") 
  
  intervention= as.data.frame(read.xlsx("/Users/somyasinghvi/Dropbox (MIT)/jingwen_tasks/infrastructure_characteristics/Market Details.xlsx",sheet = "Market Live Dates"))[3:160,]
  intervention$X3 = tolower(as.character(intervention$X3))
  temp = read.csv("/Users/somyasinghvi/Dropbox (MIT)/jingwen_tasks/CombinedArrivals/intervented change to organized.csv",header = F)
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
  
  
  date_strt=c()
  quantity_strt=c()
  for (p in 1:length(Product)){
    
    re = c()
    ree=c()
    product = Product[p]
    #dir.create(paste("/Users/tangjingwen/Dropbox/jingwen_tasks/DiD/",fmode,"/",product,sep = ""))
    
    #features = Features[[p]]
    
    #predictors = features
    
    
    
    
    #market modal price monthly
    marketdat = read.xlsx(paste("/Users/somyasinghvi/Dropbox (MIT)/jingwen_tasks/organized/data/",product,"/","10-17 week all","/",product," eleventh data.xlsx",sep = ""),sheet = 1)
    colnames(marketdat)[1] = "date"
    marketdat = marketdat[marketdat$market!="virtual",]
    temp = intervented%in%unique(marketdat$market)
    intervented1 = intervented[temp]
    interdate1 = interdate[temp]
    
    marketdat = marketdat[!is.na(marketdat$modal),]
    #marketdat = marketdat[!is.na(marketdat$amount),]
    #####to remove the data points when only with treated or untreated markets
    marketdat$treat = (marketdat$state=="karnataka")
    marketdat$treat = as.factor(marketdat$treat)
    newmarketdat = c()
    da = sort(unique(marketdat$date))
    for (l in 1:length(da)){
      temp = marketdat[marketdat$date == da[l],]
      if (length(unique(temp$treat))==2){
        newmarketdat = rbind(newmarketdat,temp)
      }
    }
    marketdat = newmarketdat
    marketdat$treat = (marketdat$state=="karnataka")
    marketdat$date = as.Date(marketdat$date-25569,origin = "1970-01-01")
    dat = marketdat
    
    wm = ddply(dat,.(date,treat),summarise, wmean = weighted.mean(modal,amount))
    wm$treat[wm$treat == T] = "treated"
    wm$treat[wm$treat == F] = "untreated"
    interday = as.Date(intervention[intervention$market%in%marketdat$market,"date"])
    
    d1 = min(interday)
    d3 = max(interday)
    tr = marketdat[marketdat$market%in%intervented,]
    s = sum(tr$amount)*0.5
    temp = aggregate(tr$amount,by = list(tr$market),sum,na.rm = T)
    colnames(temp) = c("market","amount")
    temp = join(temp,intervention,by = "market",type = "left")
    temp = temp[order(temp$date,decreasing = F),]
    for (l in 1:nrow(temp)){
      if (sum(temp$amount[1:l])>=s) {
        d2 = temp[l,"date"]
        break
      }
    }
    
    
   # marketdat%>%group_by(treat)%>%dplyr::summarise(tot_quant=sum(amount))
  #  date_strt[p]=paste(product,d1,d2,d3,sep=" ")
    
    quant_sum=marketdat[marketdat$date>as.Date('2014-02-01'),]%>%dplyr::group_by(treat)%>%summarize(tot_quant=sum(amount))
    tot_quant=quant_sum[quant_sum$treat==TRUE,]$tot_quant
    quantity_strt[p]=paste(product,tot_quant)
  }
  
  #write.xlsx(wm, paste("/Users/somyasinghvi/Dropbox (MIT)/jingwen_tasks/organized/plots/",product,"/","10-17 week all","/",product,sep = ""),sheet = 1, sheetName="Sheet1")
  
  
#marketdat%>%filter(date>as.Date(2015-01-01))%>%group_by(treat)%>%summarize(tot_quant=sum(amount))



    
    
    

