###this file is used to get the data of all commodities and choose markets we are going to do synthetic control on
###the data we get from this file can be used for both synthetic control and difference in differences
#90 5-14
#########1-8need rerun 6 model
rm(list = ls())
library(readr)
library(openxlsx)
library(plyr)
library(dplyr)
library(qpcR)
library(lubridate)
fMode = c("full","sub","sub2") #the size of the features 
f = list() #the features names
f[[1]]= c("annual","total_ar","lit_t","male_saw","annmai","lit_ru","ptmrkt","lroad","agrl_t")
f[[2]] = c("annual","total_ar","lit_t","male_saw")
f[[3]] = c("annual","total_ar","lit_ru","ptmrkt","agrl_t")
for (size in 3:3){
  fmode = fMode[size]
  f = f[[size]]
  dir.create(paste("/Users/tangjingwen/Dropbox/jingwen_tasks/SCM/vdsa chosen features weighetd wpi marketwise/no combined/",fmode,sep = ""))
  #the names of the commodities 
  Product = c("Groundnut","Jowar(Sorghum)","Maize","Sunflower","Arhar (Tur-Red Gram)","Cotton","Green Gram (Moong)","Arecanut(Betelnut-Supari)","Bengal Gram(Gram)","Black Gram (Urd Beans)","Dry Chillies","Copra","Kulthi(Horse Gram)","Paddy(Dhan)")
  #the names of the commodities in wpi file
  Wpiproduct = c("Groundnut Seed","Jowar","Maize","Sunflower","Arhar","Cotton Seed","Moong","Betelnut/Arecanut","Gram","All commodities","Chillies (Dry)","Copra (Coconut)","All commodities","All commodities")
  Model = c("10-17 month","10-17 month all","13-17 month common types","13-17 month all common types",
            "10-17 week","10-17 week all","13-17 week common types","13-17 week all common types","13-17 month without vdsa features","13-17 month all without vdsa features") #the time unit and lags chosen for the model in synthetic control
  Begin = c(2010,2010,2013,2013,2010,2010,2013,2013,2013,2013) 
  Over = c(2017,2017,2017,2017,2017,2017,2017,2017,2017,2017)
  Br = c("month","month","month","month","week","week","week","week","month","month")  #how to combine dauly data to the time unit data we want
  early = as.Date(c("2010-01-01","2010-01-01","2013-01-01","2013-01-01","2010-01-01","2010-01-01","2013-01-01","2013-01-01","2013-01-01","2013-01-01"))#set the time threshold according to the wpi data we have
  late = as.Date(c("2018-01-01","2018-01-01","2018-01-01","2018-01-01","2018-01-01","2018-01-01","2018-01-01","2018-01-01","2018-01-01","2018-01-01"))
  Lagbr  = c("month","3 months","3 months","6 months")#how to get the lags data in synthetic control
  Commontype = c(FALSE,FALSE,TRUE,TRUE,FALSE,FALSE,TRUE,TRUE,FALSE,FALSE)
  Top = c(TRUE,FALSE,TRUE,FALSE,TRUE,FALSE,TRUE,FALSE,TRUE,FALSE)
  feat = c(TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,FALSE,FALSE)
  
  setpre = c(1,3,1,2)#corresponding to the lags to chosen the columns indices of the data frame in synthetic control
  
  dat1 = read.xlsx("/Users/tangjingwen/Dropbox/jingwen_tasks/SCM/vdsa chosen features weighetd wpi marketwise/district features.xlsx",sheet = 1)
  for (j in 2:(ncol(dat1))){
    dat1[,j] = as.numeric(as.character(dat1[,j]))
  }
  #to get the features by fraction
  dat1[,2:9] = dat1[,2:9]/dat1[,31]
  dat1[,15:25] = dat1[,15:25]/dat1[,14]
  dat1[,27:30] = dat1[,27:30]/dat1[,31]
  dat1[,61:62] = dat1[,61:62]/dat1[,31]
  
  
  Features = list()
  for (j in 1:length(Product)){
    Features[[j]] = f
  }
  DAT1 = dat1
  #to get interventioninformation
  intervention= as.data.frame(read.xlsx("/Users/tangjingwen/Dropbox/jingwen_tasks/infrastructure_characteristics/Market Details.xlsx",sheet = "Market Live Dates"))[3:160,]
  intervention$X3 = tolower(as.character(intervention$X3))
  #because the names of the markest are not unified between the organized data and intervention data so we unify the two
  temp = read.csv("/Users/tangjingwen/Dropbox/jingwen_tasks/CombinedArrivals/intervented change to organized.csv",header = F)
  colnames(temp) = c("int","org")
  temp$int = as.character(temp$int)
  temp$org = as.character(temp$org)
  inind = intervention$X3%in%temp$int
  orind = match(intervention$X3[inind],temp$int)
  intervention$X3[inind] = temp$org[orind]
  intervented = tolower(as.character(intervention$X3))
  intervented = c(intervented,"virtual")
  
  #inkar = read.xlsx("/Users/tangjingwen/Dropbox/jingwen_tasks/SCM/market_districts.xlsx",sheet = 1)
  #for (l in 2:5){
  #inkar[,l] = tolower(inkar[,l])
  #}
  #inkarmar = c()
  #for (l in 1:nrow(inkar)){
  #if ("karnataka"%in%inkar[l,2:5]) inkarmar = c(inkarmar,inkar[l,1])
  #}
  #sort(intervented[!intervented%in%inkarmar])
  #sort(inkarmar[!inkarmar%in%intervented])
  #write.csv(sort(inkarmar[!inkarmar%in%intervented]),"/Users/tangjingwen/Dropbox/jingwen_tasks/SCM/inkarnataka.csv")
  
  for (p in c(7)){
    try({
      
      product = Product[p]
      dir.create(paste("/Users/tangjingwen/Dropbox/jingwen_tasks/organized/data/",product,sep = ""))
      print(product)
      features = Features[[p]]
      dat1 = DAT1[,c("market",features)]
      ####dat1 = na.omit(dat1)#because that the features don't change over time so in case that it will be NA for all time periods we just remove them
      dat1 = as.data.frame(dat1)
      predictors = features
      #create output tables
      balance_tables = c()
      
      choice = seq(from = 0.05,to = 1, by = 0.05)
      overall_effect = matrix(0,length(choice),(2*length(Model)+1))
      overall_effect = as.data.frame(overall_effect)
      colnames(overall_effect) = c("portion_of_markets",Model,Model)
      overall_effect$portion_of_markets = choice
      
      for (m in c(6)){
        dir.create(paste("/Users/tangjingwen/Dropbox/jingwen_tasks/organized/data/",product,"/",Model[m],sep = ""))
        setwd(paste("/Users/tangjingwen/Dropbox/jingwen_tasks/organized/data/",product,"/",Model[m],sep = ""))
        Balance = rep(Model[m],(length(features)))
        Market_info = c()
        Data_info = matrix(0,11,10)
        Data_info = as.data.frame(Data_info)
        colnames(Data_info) = c("state","total markets","treated markets","untreated markets",
                                "total records","treated markets records","untreated markets records",
                                "total amount portion","treated amount portion","untreated amount portion")
        Data_info$state = c("weird and inkarnataka markets removed",
                            "keep the common types data",
                            "outliers of each market each year removed",
                            "90% of markets for treated and untreated respectively",
                            "after combined with features",
                            "weighted mean price of unit time",
                            "combine with district and state",
                            "combine with production data",
                            "comnine with rainfall data",
                            "combine with state GDP",
                            "remove missing values in modal price")   #seven performance on data leading to seven formats of data
        
        ##read organized data year by year
        dat = c()
        for (i in Begin[m]:Over[m]){
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
        
        
        ##choose the data of time period we have wpi on
        ##remove the unknown markets and weird markets in control
        dat = dat[dat$date>=early[m],]
        dat = dat[dat$date<late[m],]
        inkar = as.character(read.csv("/Users/tangjingwen/Dropbox/jingwen_tasks/SCM/inkarnataka.csv",header = F)$V1)
        dat = dat[!dat$market%in%inkar,]
        weird = as.character(read.csv("/Users/tangjingwen/Dropbox/jingwen_tasks/SCM/weird control markets.csv",header = F)$V1)
        dat = dat[!dat$market%in%weird,]
        if(product=="Groundnut"){
          dat = dat[dat$market != "gondal",]
        }
        
        
        
        
        
        write.xlsx(dat,paste("/Users/tangjingwen/Dropbox/jingwen_tasks/organized/data/",product,"/",Model[m],"/",product," first data.xlsx",sep = ""))
        
        ##chunk like this is to get the data information
        h = dat
        h = h[!h$market=="virtual",]
        h1 = h[h$market%in%intervented,]
        h2 = h[!h$market%in%intervented,]
        originaltable = aggregate(h$amount,by = list(h$market),sum,na.rm = T)
        colnames(originaltable) = c("market","amount")
        originaltable = originaltable[order(originaltable$amount,decreasing = T),]
        write.xlsx(originaltable,paste("/Users/tangjingwen/Dropbox/jingwen_tasks/organized/data/",product,"/",Model[m],"/original table.xlsx",sep = ""))
        totalamount = sum(h$amount,na.rm = T)
        treatedamount = sum(h1$amount,na.rm = T)
        untreatedamount = sum(h2$amount,na.rm = T)
        Data_info[1,2] = length(unique(h$market))
        Data_info[1,3] = length(unique(h1$market))
        Data_info[1,4] = length(unique(h2$market))
        Data_info[1,5] = nrow(h)
        Data_info[1,6] = nrow(h1)
        Data_info[1,7] = nrow(h2)
        Data_info[1,8] = sum(originaltable[originaltable$market%in%unique(h$market),"amount"],na.rm = T)/totalamount
        Data_info[1,9] = sum(originaltable[originaltable$market%in%unique(h1$market),"amount"],na.rm = T)/treatedamount
        Data_info[1,10] = sum(originaltable[originaltable$market%in%unique(h2$market),"amount"],na.rm = T)/untreatedamount
        
        if (Commontype[m]){
          dat = dat[dat$type!="Other",]
          temp = dat[dat$market%in%intervented,]
          types = unique(temp$type)
          dat = dat[dat$type%in%types,]
        }
        
        
        write.xlsx(dat,paste("/Users/tangjingwen/Dropbox/jingwen_tasks/organized/data/",product,"/",Model[m],"/",product," second data.xlsx",sep = ""))
        
        
        h = dat
        h = h[!h$market=="virtual",]
        h1 = h[h$market%in%intervented,]
        h2 = h[!h$market%in%intervented,]
        Data_info[2,2] = length(unique(h$market))
        Data_info[2,3] = length(unique(h1$market))
        Data_info[2,4] = length(unique(h2$market))
        Data_info[2,5] = nrow(h)
        Data_info[2,6] = nrow(h1)
        Data_info[2,7] = nrow(h2)
        Data_info[2,8] = sum(originaltable[originaltable$market%in%unique(h$market),"amount"],na.rm = T)/totalamount
        Data_info[2,9] = sum(originaltable[originaltable$market%in%unique(h1$market),"amount"],na.rm = T)/treatedamount
        Data_info[2,10] = sum(originaltable[originaltable$market%in%unique(h2$market),"amount"],na.rm = T)/untreatedamount
        
        
        
        dat$year = year(dat$date)
        ###in case all days of a market don't have amount records,anyway we need to rank  them by order so the na in amount won't be chosen
        
        #keep 1% - 99% of modal price for each market each year
        #keep 1% - 95% of amount for each market each year
        dat$amount = as.numeric(as.character(dat$amount))
        dat$modal = as.numeric(as.character(dat$modal))
        newd = c()
        for (i in 1:length(unique(dat$market))){
          temp = dat[dat$market==unique(dat$market)[i],]
          temp$amount[((temp$amount<quantile(temp$amount,probs = c(0.01,0.95),na.rm = T)[1])|(temp$amount>quantile(temp$amount,probs = c(0.01,0.95),na.rm = T)[2]))] = -100
          temp$modal[((temp$modal<quantile(temp$modal,probs = c(0.01,0.99),na.rm = T)[1])|(temp$modal>quantile(temp$modal,probs = c(0.01,0.99),na.rm = T)[2]))] =-100
          temp = temp[temp$amount!=-100,]
          temp = temp[temp$modal!=-100,]
          print(nrow(temp))
          newd = rbind(newd,temp)
        }
        dat = newd[,-ncol(newd)]
        ###there are markets have na in all days of a year so that it will be organized to an empty row in the dat and we need to remove them
        dat = dat[!is.na(dat$market),]
        
        
        
        
        write.xlsx(dat,paste("/Users/tangjingwen/Dropbox/jingwen_tasks/organized/data/",product,"/",Model[m],"/",product," third data.xlsx",sep = ""))
        
        h = dat
        h = h[!h$market=="virtual",]
        h1 = h[h$market%in%intervented,]
        h2 = h[!h$market%in%intervented,]
        Data_info[3,2] = length(unique(h$market))
        Data_info[3,3] = length(unique(h1$market))
        Data_info[3,4] = length(unique(h2$market))
        Data_info[3,5] = nrow(h)
        Data_info[3,6] = nrow(h1)
        Data_info[3,7] = nrow(h2)
        Data_info[3,8] = sum(originaltable[originaltable$market%in%unique(h$market),"amount"],na.rm = T)/totalamount
        Data_info[3,9] = sum(originaltable[originaltable$market%in%unique(h1$market),"amount"],na.rm = T)/treatedamount
        Data_info[3,10] = sum(originaltable[originaltable$market%in%unique(h2$market),"amount"],na.rm = T)/untreatedamount
        
        if (Top[m]){
          ##choose top 90% markets according to markets total amount
          a1 = dat
          
          
          amountrm1 = aggregate(a1$amount,by = list(a1$market),sum,na.rm = T)
          amountrm1 = amountrm1[order(amountrm1$x,decreasing = T),]
          thres1 = sum(amountrm1[,"x"],na.rm = T)*0.9
          for (i in 1:nrow(amountrm1)){
            if (sum(amountrm1[1:i,"x"],na.rm = T)>thres1) break
          }
          arm = amountrm1[i:nrow(amountrm1),"Group.1"]
          
          
          
          
          
          dat = dat[!dat$market%in%arm,]
        }
        
        
        
        
        dat = dat[,c(1:3,5:7)]
        colnames(dat) = c("market","date","amount","minp","maxp","modal")
        dat$market = tolower(dat$market)
        dat$amount = as.numeric(as.character(dat$amount))
        dat$minp = as.numeric(as.character(dat$minp))
        dat$maxp = as.numeric(as.character(dat$maxp))
        dat$modal = as.numeric(as.character(dat$modal))
        
        
        write.xlsx(dat,paste("/Users/tangjingwen/Dropbox/jingwen_tasks/organized/data/",product,"/",Model[m],"/",product," forth data.xlsx",sep = ""))
        
        
        h = dat
        h = h[!h$market=="virtual",]
        h1 = h[h$market%in%intervented,]
        h2 = h[!h$market%in%intervented,]
        Data_info[4,2] = length(unique(h$market))
        Data_info[4,3] = length(unique(h1$market))
        Data_info[4,4] = length(unique(h2$market))
        Data_info[4,5] = nrow(h)
        Data_info[4,6] = nrow(h1)
        Data_info[4,7] = nrow(h2)
        Data_info[4,8] = sum(originaltable[originaltable$market%in%unique(h$market),"amount"],na.rm = T)/totalamount
        Data_info[4,9] = sum(originaltable[originaltable$market%in%unique(h1$market),"amount"],na.rm = T)/treatedamount
        Data_info[4,10] = sum(originaltable[originaltable$market%in%unique(h2$market),"amount"],na.rm = T)/untreatedamount
        
        
        
        ###################################################################################
        #combine organized data(about price and amount) with district features data (about all features)
        lagbr = Lagbr[m]
        br = Br[m]
        
        filename = paste("/Users/tangjingwen/Dropbox/jingwen_tasks/organized/data/",product,"/",Model[m],"/",product," forth data.xlsx",sep = "")
        dat = read.xlsx(filename,sheet = 1)
        
        #dat1_ = na.omit(dat1_)##########because we do not need them in the DiD
        if (feat[m]){
          dat = join(dat,dat1,by = "market",type = "inner")
          dat = dat[(dat$market)%in%(dat1$market),]
          
        }
        
        
        write.xlsx(dat,paste("/Users/tangjingwen/Dropbox/jingwen_tasks/organized/data/",product,"/",Model[m],"/",product," fifth data.xlsx",sep = ""))
        
        
        
        ################to take the vdsa chosen features weighetd wpi marketwise here except amount and price
        h = dat
        h = h[!h$market=="virtual",]
        h1 = h[h$market%in%intervented,]
        h2 = h[!h$market%in%intervented,]
        Data_info[5,2] = length(unique(h$market))
        Data_info[5,3] = length(unique(h1$market))
        Data_info[5,4] = length(unique(h2$market))
        Data_info[5,5] = nrow(h)
        Data_info[5,6] = nrow(h1)
        Data_info[5,7] = nrow(h2)
        Data_info[5,8] = sum(originaltable[originaltable$market%in%unique(h$market),"amount"],na.rm = T)/totalamount
        Data_info[5,9] = sum(originaltable[originaltable$market%in%unique(h1$market),"amount"],na.rm = T)/treatedamount
        Data_info[5,10] = sum(originaltable[originaltable$market%in%unique(h2$market),"amount"],na.rm = T)/untreatedamount
        
        
        
        ##to calculate for the virtual market
        dat_1 = dat[dat$market%in%intervented,c("market","amount")]
        dat_1 = na.omit(dat_1)
        weight = aggregate(dat_1$amount,list(dat_1$market),mean)
        weight$x = weight$x/sum(weight$x,na.rm = T)
        colnames(weight) = c("market","weight")
        
        ##to deal with the price with wpi: price/wpi*100
        
        dat$date <- as.Date(dat$date-25569,origin = "1970-01-01")
        
        
        wpi = read.xlsx("/Users/tangjingwen/Dropbox/jingwen_tasks/infrastructure_characteristics/monthly_index-2.xlsx",sheet = 1)
        wpi = wpi[wpi$COMM_NAME == Wpiproduct[p],][,4:72]
        wpimonth = seq(as.Date("2012/04/01"), as.Date("2017/12/01"), by = "month")
        WPI = data.frame(month = wpimonth,wpi = as.numeric(as.character(wpi)))
        
        dat$month  <- as.Date(cut(dat$date,
                                  breaks = "month"))
        #newd = join(dat,WPI,by = "month",type = "left")
        #newd$wpi = as.numeric(as.character(newd$wpi))
        #newd$modal = as.numeric(as.character(newd$modal))
        #newd$modal = (newd$modal)/(newd$wpi)
        #newd$modal = newd$modal*100
        #dat = newd[,1:(ncol(newd)-2)]
        
        
        
        
        
        
        ##break the data to the unit time we need for synthetic control
        ##for amount use the average of each month
        ##for other variate use volume weighted average
        
        dat$month <- as.Date(cut(dat$date,
                                 breaks = br))
        dat = dat[order(as.Date(dat$date, format="%d/%m/%Y")),]
        for (j in 3:(ncol(dat)-1)){
          dat[,j] = as.numeric(as.character(dat[,j]))
        }
        #dat = na.omit(dat)
        #func = function(x) aggregate(x, list(dat$month,dat$market), mean)
        #newdat = apply(dat[,3:(ncol(dat)-1)],2,func)
        #newdat = as.data.frame(newdat)
        tempdat = dat[!is.na(dat$amount),]
        tempdat = tempdat[!is.na(dat$modal),]
        newdat = as.data.frame(aggregate(tempdat$amount, list(as.Date(tempdat$month),tempdat$market), mean,na.rm = T))
        colnames(newdat) = c("month","market","amount")
        #tempdat = na.omit(dat)
        
        temp = ddply(tempdat,.(month,market),summarise, minp = weighted.mean(minp,amount,na.rm = T))
        newdat = join(newdat,temp,by = c("month","market"))
        temp = ddply(tempdat,.(month,market),summarise, maxp = weighted.mean(maxp,amount,na.rm = T))
        newdat = join(newdat,temp,by = c("month","market"))
        temp = ddply(tempdat,.(month,market),summarise, modal = weighted.mean(modal,amount,na.rm = T))
        newdat = join(newdat,temp,by = c("month","market"))
        
        if (feat[m]){
          for (l in 1:length(features)){
            temp = aggregate(tempdat[,(l+6)],by = list(tempdat$month,tempdat$market),mean,na.rm = T)
            colnames(temp)=c("month","market",features[l])
            newdat = join(newdat,temp,by = c("month","market"),type = "left")
          }
          
          
          
          
          library(plyr)
          
          
          d = newdat
          
          d$month = as.Date(d$month,format="%d/%m/%Y")
          d_1 = d[d$market%in%intervented,]
          d_2 = d[!d$market%in%intervented,]
          d_11 = c()
          for (j in 3:ncol(d_1)){
            d_1[,j] = as.numeric(as.character(d_1[,j]))
          }
          for (i in 1:length(unique(d_1$month))){
            print(i)
            Temp = d_1[d_1$month==unique(d_1$month)[i],]
            inter = intersect(weight$market,Temp$market)
            Temp = Temp[order(Temp$market),]
            weight = weight[order(weight$market),]
            temp = Temp[Temp$market%in%inter,]
            wei = weight[weight$market%in%inter,"weight"]
            func = function(x) sum(wei*x,na.rm = T)
            Temp[(nrow(Temp)+1),3:6] = as.numeric(as.character(apply(temp[,3:6],2,func)))
            Temp[nrow(Temp),7:ncol(temp)] = as.numeric(as.character(apply(temp[,7:ncol(temp)],2,sum)))
            Temp[nrow(Temp),1] = unique(d_1$month)[i]
            Temp[nrow(Temp),2] = "virtual"
            #temp = rbind(temp,c(unique(d_1$month)[i],,as.numeric(apply(temp[,3:ncol(temp)],2,f))))
            d_11 = rbind(d_11,Temp)
          }
          d = rbind(d_11,d_2)
          
          newdat = d
        }
        dat = newdat
        write.xlsx(dat,paste("/Users/tangjingwen/Dropbox/jingwen_tasks/organized/data/",product,"/",Model[m],"/",product," sixth data.xlsx",sep = ""),sheet=1)
        
        
        h = dat
        h = h[!h$market=="virtual",]
        h1 = h[h$market%in%intervented,]
        h2 = h[!h$market%in%intervented,]
        Data_info[6,2] = length(unique(h$market))
        Data_info[6,3] = length(unique(h1$market))
        Data_info[6,4] = length(unique(h2$market))
        Data_info[6,5] = nrow(h)
        Data_info[6,6] = nrow(h1)
        Data_info[6,7] = nrow(h2)
        Data_info[6,8] = sum(originaltable[originaltable$market%in%unique(h$market),"amount"],na.rm = T)/totalamount
        Data_info[6,9] = sum(originaltable[originaltable$market%in%unique(h1$market),"amount"],na.rm = T)/treatedamount
        Data_info[6,10] = sum(originaltable[originaltable$market%in%unique(h2$market),"amount"],na.rm = T)/untreatedamount
        
        
        ###############################################################
        
        #add state and districts for marketdat
        ms = read.xlsx("/Users/tangjingwen/Dropbox/jingwen_tasks/SCM/market_districts.xlsx",sheet=1)
        
        colnames(ms)[1:4] = c("market","dis1","dis2","state")
        ms$market = tolower(ms$market)
        ms$state = tolower(ms$state)
        ms$dis1 = tolower(ms$dis1)
        ms$dis2 = tolower(ms$dis2)
        MS = ms
        ms = ms[,c(1:3)]
        #remove records which are "non returned" from google API
        #ms = ms[(ms$dis1!="none returned")&(ms$dis1!="zero_results"),]
        d = dat
        d = join(d,ms,by = "market",type = "inner")
        
        dat = d
        
        write.xlsx(dat,paste("/Users/tangjingwen/Dropbox/jingwen_tasks/organized/data/",product,"/",Model[m],"/",product," seventh data.xlsx",sep = ""),sheet=1)
        
        h = dat
        h = h[!h$market=="virtual",]
        h1 = h[h$market%in%intervented,]
        h2 = h[!h$market%in%intervented,]
        Data_info[7,2] = length(unique(h$market))
        Data_info[7,3] = length(unique(h1$market))
        Data_info[7,4] = length(unique(h2$market))
        Data_info[7,5] = nrow(h)
        Data_info[7,6] = nrow(h1)
        Data_info[7,7] = nrow(h2)
        Data_info[7,8] = sum(originaltable[originaltable$market%in%unique(h$market),"amount"],na.rm = T)/totalamount
        Data_info[7,9] = sum(originaltable[originaltable$market%in%unique(h1$market),"amount"],na.rm = T)/treatedamount
        Data_info[7,10] = sum(originaltable[originaltable$market%in%unique(h2$market),"amount"],na.rm = T)/untreatedamount
        
        
        ####################################################################################
        #to combine with the production data
        print(product)
        setwd(paste("/Users/tangjingwen/Dropbox/jingwen_tasks/organized/data/",product,"/",Model[m],sep = ""))
        dat = read.xlsx(paste("/Users/tangjingwen/Dropbox/jingwen_tasks/organized/data/",product,"/",Model[m],"/",product," seventh data.xlsx",sep = ""),sheet=1)
        dat$month = as.Date(dat$month-25569,origin = "1970-01-01")
        dat$year = year(dat$month)
        production = read.xlsx("/Users/tangjingwen/Dropbox/jingwen_tasks/state features/production.xlsx",sheet = 1)#the production information include all 14 commodities
        production = production[production$product == product,]#select the production information of the certain product
        #production = production[production$season]
        temp = read.xlsx("/Users/tangjingwen/Dropbox/jingwen_tasks/SCM/production to organized districts.xlsx",sheet = 1,colNames = F)
        for (l in 1:nrow(temp)){
          if(sum(production$district%in%temp[l,-1])!=0){
            add = production[production$district%in%temp[l,-1],] 
            add$district = temp[l,1]
            production = rbind(production,add)
            #production = production[!production$district%in%temp[l,-1],]
          }
        }
        colnames(dat)[colnames(dat)=="dis1"] = "district"
        #write.xlsx(unique(dat$district)[!unique(dat$district)%in%unique(production$district)],paste("/Users/tangjingwen/Dropbox/jingwen_tasks/organized/data/",product,"/",Model[m],"/",product," lost districts from production.xlsx",sep = ""),sheet=1)
        dat_1 = join(dat,production,by = c("district","year"),type = "inner")
        #if the market cannot find its corresponding district within the column dis1, then look into dis2 column
        dat_remain = dat[!dat$district%in%production$district,]
        colnames(dat_remain)[colnames(dat_remain)=="district"] = "dis1"
        colnames(dat_remain)[colnames(dat_remain)=="dis2"] = "district"
        dat_2 = join(dat_remain,production,by = c("district","year"),type = "inner")
        dat_1 = dat_1[,!colnames(dat_1)%in%c("dis1","dis2")]
        dat_2 = dat_2[,!colnames(dat_2)%in%c("dis1","dis2")]
        dat = rbind(dat_1,dat_2)
        
        
        
        
        
        
        write.xlsx(dat,paste("/Users/tangjingwen/Dropbox/jingwen_tasks/organized/data/",product,"/",Model[m],"/",product," eighth data.xlsx",sep = ""),sheet=1)
        
        
        h = dat
        h = h[!h$market=="virtual",]
        h1 = h[h$market%in%intervented,]
        h2 = h[!h$market%in%intervented,]
        Data_info[8,2] = length(unique(h$market))
        Data_info[8,3] = length(unique(h1$market))
        Data_info[8,4] = length(unique(h2$market))
        Data_info[8,5] = nrow(h)
        Data_info[8,6] = nrow(h1)
        Data_info[8,7] = nrow(h2)
        Data_info[8,8] = sum(originaltable[originaltable$market%in%unique(h$market),"amount"],na.rm = T)/totalamount
        Data_info[8,9] = sum(originaltable[originaltable$market%in%unique(h1$market),"amount"],na.rm = T)/treatedamount
        Data_info[8,10] = sum(originaltable[originaltable$market%in%unique(h2$market),"amount"],na.rm = T)/untreatedamount
        
        ####################################################################################
        #to combine with the rainfall data
        print(product)
        setwd(paste("/Users/tangjingwen/Dropbox/jingwen_tasks/organized/data/",product,"/",Model[m],sep = ""))
        dat = read.xlsx(paste("/Users/tangjingwen/Dropbox/jingwen_tasks/organized/data/",product,"/",Model[m],"/",product," eighth data.xlsx",sep = ""),sheet=1)
        dat$month = as.Date(dat$month-25569,origin = "1970-01-01")
        
        colnames(dat)[1] = "week"
        dat$month = as.Date(cut(dat$week,breaks = "month"))
        
        rainfall = read.xlsx("/Users/tangjingwen/Dropbox/jingwen_tasks/infrastructure_characteristics/rainfall.xlsx",sheet = 1)#the rainfall information include all 14 commodities
        rainfall$month = as.Date(rainfall$month)
        temp = read.xlsx("/Users/tangjingwen/Dropbox/jingwen_tasks/SCM/rainfall to organized districts.xlsx",sheet = 1,colNames = F)
        for (l in 1:nrow(temp)){
          if(sum(rainfall$district%in%temp[l,-1])!=0){
            add = rainfall[rainfall$district%in%temp[l,-1],] 
            add$district = temp[l,1]
            rainfall = rbind(rainfall,add)
            #rainfall = rainfall[!rainfall$district%in%temp[l,-1],]
          }
        }
        #write.xlsx(unique(dat$district)[!unique(dat$district)%in%unique(rainfall$district)],paste("/Users/tangjingwen/Dropbox/jingwen_tasks/organized/data/",product,"/",Model[m],"/",product," lost districts from rainfall.xlsx",sep = ""),sheet=1)
        dat = dat[dat$district%in%unique(rainfall$district),]
        temp = matrix(nrow = nrow(dat),ncol = 13)
        colnames(temp) = c("rainfall","rainfall_lead1","rainfall_lead2","rainfall_lead3","rainfall_lead4"
                           ,"rainfall_lead5","rainfall_lead6","rainfall_lead7"
                           ,"rainfall_lead8","rainfall_lead9","rainfall_lead10","rainfall_lead11","rainfall_lead12")
        dat = cbind(dat,temp)
        for (l in 1:nrow(dat)){
          print(l)
          dis = dat[l,"district"]
          mon = dat[l,"month"]
          for (w in 0:12){
            if (length(rainfall[(rainfall$district==dis)&(rainfall$month==(mon %m+% months(-w))),"rainfall"])>0){
              dat[l,(ncol(dat)-13+w+1)] = rainfall[(rainfall$district==dis)&(rainfall$month==(mon %m+% months(-w))),"rainfall"]
            }
            
          }
        }
        
        
        dat = join(dat,WPI,by = "month")
        
        
        
        write.xlsx(dat,paste("/Users/tangjingwen/Dropbox/jingwen_tasks/organized/data/",product,"/",Model[m],"/",product," ninth data.xlsx",sep = ""),sheet=1)
        
        
        h = dat
        h = h[!h$market=="virtual",]
        h1 = h[h$market%in%intervented,]
        h2 = h[!h$market%in%intervented,]
        Data_info[9,2] = length(unique(h$market))
        Data_info[9,3] = length(unique(h1$market))
        Data_info[9,4] = length(unique(h2$market))
        Data_info[9,5] = nrow(h)
        Data_info[9,6] = nrow(h1)
        Data_info[9,7] = nrow(h2)
        Data_info[9,8] = sum(originaltable[originaltable$market%in%unique(h$market),"amount"],na.rm = T)/totalamount
        Data_info[9,9] = sum(originaltable[originaltable$market%in%unique(h1$market),"amount"],na.rm = T)/treatedamount
        Data_info[9,10] = sum(originaltable[originaltable$market%in%unique(h2$market),"amount"],na.rm = T)/untreatedamount
        
        ##########################################################
        #to combine with state level GDP
        d = dat
        sf = read.xlsx("/Users/tangjingwen/Dropbox/jingwen_tasks/state features/full state features.xlsx",sheet = 1)
        temp = read.xlsx("/Users/tangjingwen/Dropbox/jingwen_tasks/SCM/sf_to_organized_states.xlsx",sheet = 1,colNames = F)
        for (l in 1:nrow(temp)){
          sf$state[sf$state%in%temp[l,-1]] = temp[l,1]
        }
        write.xlsx(unique(d$state)[!unique(d$state)%in%unique(sf$state)],paste("/Users/tangjingwen/Dropbox/jingwen_tasks/organized/data/",product,"/",Model[m],"/",product," lost states from GDP.xlsx",sep = ""),sheet=1)
        
        newd = join(d,sf,by = c("state","year"),type = "inner")
        newd = as.data.frame(newd)
        
        dat = newd
        write.xlsx(dat,paste("/Users/tangjingwen/Dropbox/jingwen_tasks/organized/data/",product,"/",Model[m],"/",product," tenth data.xlsx",sep = ""),sheet=1)
        
        
        h = dat
        h = h[!h$market=="virtual",]
        h1 = h[h$market%in%intervented,]
        h2 = h[!h$market%in%intervented,]
        Data_info[10,2] = length(unique(h$market))
        Data_info[10,3] = length(unique(h1$market))
        Data_info[10,4] = length(unique(h2$market))
        Data_info[10,5] = nrow(h)
        Data_info[10,6] = nrow(h1)
        Data_info[10,7] = nrow(h2)
        Data_info[10,8] = sum(originaltable[originaltable$market%in%unique(h$market),"amount"],na.rm = T)/totalamount
        Data_info[10,9] = sum(originaltable[originaltable$market%in%unique(h1$market),"amount"],na.rm = T)/treatedamount
        Data_info[10,10] = sum(originaltable[originaltable$market%in%unique(h2$market),"amount"],na.rm = T)/untreatedamount
        
        
        ###########################################################
        
        #to remove the missing values in modal price to do synthetic control
        dat = dat[!is.na(dat$modal),]
        write.xlsx(dat,paste("/Users/tangjingwen/Dropbox/jingwen_tasks/organized/data/",product,"/",Model[m],"/",product," eleventh data.xlsx",sep = ""),sheet=1)
        
        
        h = dat
        h = h[!h$market=="virtual",]
        h1 = h[h$market%in%intervented,]
        h2 = h[!h$market%in%intervented,]
        Data_info[11,2] = length(unique(h$market))
        Data_info[11,3] = length(unique(h1$market))
        Data_info[11,4] = length(unique(h2$market))
        Data_info[11,5] = nrow(h)
        Data_info[11,6] = nrow(h1)
        Data_info[11,7] = nrow(h2)
        Data_info[11,8] = sum(originaltable[originaltable$market%in%unique(h$market),"amount"],na.rm = T)/totalamount
        Data_info[11,9] = sum(originaltable[originaltable$market%in%unique(h1$market),"amount"],na.rm = T)/treatedamount
        Data_info[11,10] = sum(originaltable[originaltable$market%in%unique(h2$market),"amount"],na.rm = T)/untreatedamount
        
        
        
        a = dat
        a1 = a[a$market%in%intervented,]
        a2 = a[!a$market%in%intervented,]
        ##set the treated markets in data now as the treated markets we are going to do synthetc control on
        write.xlsx(sort(unique(dat$market))[sort(unique(dat$market))%in%intervented],paste("/Users/tangjingwen/Dropbox/jingwen_tasks/SCM/vdsa chosen features weighetd wpi marketwise/no combined/",fmode,"/",Product[p],"/treat.xlsx",sep = ""))
        write.xlsx(Data_info,paste("/Users/tangjingwen/Dropbox/jingwen_tasks/organized/data/",Product[p],"/",Model[m],"/data details.xlsx",sep = ""))
        
      }
      
    })
  }#product
}


##########below is abandoned codes
##insert 226

dat$year = year(dat$date)
#to choose top 90% days in a year for a market 
dat = dat[!is.na(dat$amount),]
newd = c()
for (i in 1:length(unique(dat$market))){
  Temp = dat[dat$market==unique(dat$market)[i],]
  for (j in Begin[m]:Over[m]){
    temp = Temp[Temp$year == j,]
    temp = temp[order(temp$amount,decreasing = T),]
    thres = sum(temp$amount,na.rm = T)*0.9
    for (l in 1:nrow(temp)){
      if (sum(temp$amount[1:l],na.rm = T)>thres) break
    }
    temp = temp[1:l,]
    newd = rbind(newd,temp)
  }
}

dat = newd[,-ncol(newd)]
###there are markets have na in all days of a year so that it will be organized to an empty row in the dat and we need to remove them
dat = dat[!is.na(dat$market),]


##insert 89

treat = as.character(read.xlsx(paste("/Users/tangjingwen/Dropbox/jingwen_tasks/SCM/vdsa chosen features weighetd wpi marketwise/no combined/",fmode,"/",Product[p],"/treat.xlsx",sep = ""),sheet = 1,colNames = F)$X1)
# c("challakere","chitradurga","davangere","gadag","koppal","laxmeshwar","madhugiri","mundaragi","raichur","savanur","sira","hubli (amaragol)","bellary","yadgir","virtual")
subintervented = c(treat[treat%in%intervented])
#poolmarkets_amountportion = matrix(0,length(subintervented)*length(Model),3)
#poolmarkets_amountportion = as.data.frame(poolmarkets_amountportion)
#poolmarkets_amountportion[,1] = rep(subintervented,each = length(Model))
#poolmarkets_amountportion[,2] = rep(Model,length(subintervented))
#colnames(poolmarkets_amountportion)[-(1:2)] = c("portionbeforefeatures")

gaps_analysis = matrix(0,length(subintervented)*length(Model),16+length(predictors))
temp = c("mean of gaps before","t value","p value","mean of gaps after","t value","p value","mean of relative gaps before","t value","p value","mean of relative gaps after","t value","p value","RMSPE_before","RMSPE_after",predictors)
gaps_analysis = as.data.frame(gaps_analysis)
gaps_analysis[,1] = rep(subintervented,each = length(Model))
gaps_analysis[,2] = rep(Model,length(subintervented))
colnames(gaps_analysis)[-(1:2)] = temp



####insert 529
d = dat
for (l in 1:nrow(d)){
  if (d[l,"state"]=="india"|is.na(d[l,"state"])){
    d[l,"state"] = MS[MS$market==d[l,"market"],3]
  }
}
d = d[!is.na(d$state),]
for (l in 1:nrow(d)){
  if (d[l,"state"]%in%c("hagaribommanahalli","hubballi")){
    d[l,"state"] = "karnataka"
  }
  if (d[l,"state"]%in%c("bhildi","jagdish nagar")){
    d[l,"state"] = "gujarat"
  }
  if (d[l,"state"]=="jadcherla"){
    d[l,"state"] = "telangana"
  }
  if (d[l,"state"]=="purani basti"){
    d[l,"state"] = "rajasthan"
  }
  if (d[l,"state"]%in%c("sampath nagar","ramanujapuram")){
    d[l,"state"] = "tamil nadu"
  }
  if (d[l,"state"]=="risia bazaar"){
    d[l,"state"] = "uttar pradesh"
  }
  
}
dat = d


if(sum(production$district%in%temp[l,-1])!=0){
  add = production[production$district%in%temp[l,-1],] 
  add$district = temp[l,1]
  production = rbind(production,add)
  #production = production[!production$district%in%temp[l,-1],]
}