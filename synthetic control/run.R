
rm(list = ls())
library(readr)
library(openxlsx)
library(plyr)
library(dplyr)
library(Synth)
library(ggplot2)
library(graphics)
library(qpcR)
fMode = c("full","sub","sub2") #means the set of features
f = list() #the names of the features
f[[1]]= c("annual","total_ar","lit_t","male_saw","annmai","lit_ru","ptmrkt","lroad","agrl_t")
f[[2]] = c("annual","total_ar","lit_t","male_saw")
f[[3]] = c("annual","total_ar","lit_t","agrl_t","ptmrkt")
for (size in 3:3){
  
  fmode = fMode[size]
  f = f[[size]]
  dir.create(paste("/Users/tangjingwen/Dropbox/jingwen_tasks/organized/result/synthetic control/",fmode,sep = ""))
  #Product is the products list
  Product = c("Groundnut","Jowar(Sorghum)","Maize","Sunflower","Arhar (Tur-Red Gram)","Cotton","Green Gram (Moong)","Arecanut(Betelnut-Supari)","Bengal Gram(Gram)","Black Gram (Urd Beans)","Dry Chillies","Copra","Kulthi(Horse Gram)","Paddy(Dhan)")
  #Wpiproduct is the names of the commodities in the wpi file to read their wpi from 2012.4 to 2017.12
  Wpiproduct = c("Groundnut Seed","Jowar","Maize","Sunflower","Arhar","Cotton Seed","Moong","Betelnut/Arecanut","Gram","All commodities","Chillies (Dry)","Copra (Coconut)","All commodities","All commodities")
  Model = c("01-17 mon-mon","01-17 mon-qua","01-17 qua-qua","01-17 qua-half")
  Begin = c(2001,2001,2001,2001)
  Over = c(2017,2017,2017,2017)
  Br = c("month","month","3 months","3 months")
  early = as.Date(c("2001-01-01","2001-01-01","2001-01-01","2001-01-01"))
  late = as.Date(c("2018-01-01","2018-01-01","2018-01-01","2018-01-01"))
  Lagbr  = c("month","3 months","3 months","6 months")
  
  setpre = c(1,3,1,2)
  
  
  dat1 = read.xlsx("/Users/tangjingwen/Dropbox/jingwen_tasks/SCM/vdsa chosen features weighetd wpi marketwise/district features.xlsx",sheet = 1)
  for (j in 2:(ncol(dat1))){
    dat1[,j] = as.numeric(as.character(dat1[,j]))
  }
  
  dat1[,2:9] = dat1[,2:9]/dat1[,31]
  dat1[,15:25] = dat1[,15:25]/dat1[,14]
  dat1[,27:30] = dat1[,27:30]/dat1[,31]
  dat1[,61:62] = dat1[,61:62]/dat1[,31]
  
  
  Features = list()
  Features[[1]] = c("gnut_tq",f)
  Features[[2]] = c("sorg_tq",f)
  Features[[3]] = c("maiz_tq",f)
  Features[[4]] = c("sunf_tq",f)
  for (j in 1:length(Product)){
    Features[[j]] = f
  }
  DAT1 = dat1
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
  #2,4,6,7,8,12,10,11,
  for (p in 4:4){
    
    product = Product[p]
    dir.create(paste("/Users/tangjingwen/Dropbox/jingwen_tasks/organized/result/synthetic control/",fmode,"/",product,sep = ""))
    
    features = Features[[p]]
    dat1 = DAT1[,c("market",features)]
    ####dat1 = na.omit(dat1)#because that the features don't change over time so in case that it will be NA for all time periods we just remove them
    dat1 = as.data.frame(dat1)
    predictors = features
    
    treat = as.character(as.array(read.csv(paste("/Users/tangjingwen/Dropbox/jingwen_tasks/SCM/vdsa chosen features weighetd wpi marketwise/",fmode,"/",Product[p],"/treat.csv",sep = ""),header = F)$V1))
    # c("challakere","chitradurga","davangere","gadag","koppal","laxmeshwar","madhugiri","mundaragi","raichur","savanur","sira","hubli (amaragol)","bellary","yadgir","virtual")
    subintervented = c(treat[treat%in%intervented],"virtual")
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
    
    balance_tables = c()
    
    choice = seq(from = 0.05,to = 1, by = 0.05)
    overall_effect = matrix(0,length(choice),(2*length(Model)+1))
    overall_effect = as.data.frame(overall_effect)
    colnames(overall_effect) = c("portion_of_markets",Model,Model)
    overall_effect$portion_of_markets = choice
    
    for (m in 3:3){
      dir.create(paste("/Users/tangjingwen/Dropbox/jingwen_tasks/organized/result/synthetic control/",fmode,"/",product,"/",Model[m],sep = ""))
      setwd(paste("/Users/tangjingwen/Dropbox/jingwen_tasks/organized/result/synthetic control/",fmode,"/",product,"/",Model[m],sep = ""))
      Balance = rep(Model[m],(length(features)))
      Market_info = c()
      Data_info = matrix(0,7,10)
      Data_info = as.data.frame(Data_info)
      colnames(Data_info) = c("state","total markets","treated markets","untreated markets",
                              "total records","treated markets records","untreated markets records",
                              "total amount portion","treated amount portion","untreated amount portion")
      Data_info$state = c("all organized data (2012.04-2017.12 and unknown karnataka markets removed and replacing 1% outliers of price and amount with NA)",
                    "90% of markets for treated and untreated respectively",
                    "after combined with features","after adjusted with wpi","weighted mean price of unit time",
                    "remove missing values in modal price","combine with state")
      dat = c()
      for (i in Begin[m]:Over[m]){
        filename = paste("/Users/tangjingwen/Dropbox/jingwen_tasks/Organized Data/",i,"_Data/",product,".xlsx",sep = "")
        print(i)
        mydf <- read.xlsx(filename, sheet = 1, startRow = 1, colNames = F)
        marketrow = (1:nrow(mydf))[!is.na(mydf$X1)]
        concluderow = (1:nrow(mydf))[is.na(mydf$X2)]
        for (j in 1:(length(marketrow)-1)){
          mydf[(marketrow[j]+1):(marketrow[j+1]-2),"X1"] = mydf[marketrow[j],"X1"]
        }
        mydf[(marketrow[length(marketrow)]+1):(nrow(mydf)-1),"X1"] = mydf[marketrow[length(marketrow)],"X1"]
        mydf = mydf[-c(marketrow,concluderow),]
        mydf$X3 = as.numeric(as.character(mydf$X3))
        for (j in 5:7){
          mydf[,j] = as.numeric(as.character(mydf[,j]))
          temp = mean(mydf[,j],na.rm = T)
          mydf[,j][is.na(mydf[,j])]=temp
        }
        dat = rbind(dat,mydf)
      }
      colnames(dat) = c("market","date","amount","type","minp","maxp","modal")
      dat$market = tolower(dat$market)
      dat$date = as.Date(dat$date,format = "%d/%m/%Y")
      
      if(p %in% c(1,3,6,9)){
        ad = read.csv(paste("/Users/tangjingwen/Dropbox/jingwen_tasks/CombinedArrivals/",product,".csv",sep = ""))
        ad$Market = tolower(ad$Market)
        ad$Variety = tolower(ad$Variety)
        
        temp = read.csv("/Users/tangjingwen/Dropbox/jingwen_tasks/CombinedArrivals/combined change to organized.csv",header = F)
        colnames(temp) = c("ad","org")
        temp$ad = as.character(temp$ad)
        temp$org = as.character(temp$org)
        inind = ad$Market%in%temp$ad
        orind = match(ad$Market[inind],temp$ad)
        ad$Market[inind] = temp$org[orind]
        
        
        ad$Date = as.Date(ad$Date,format = "%d/%m/%Y")
        ad = ad[,c(1,2,5,3,7,8,9)]
        colnames(ad) = c("market","date","amount","type","minp","maxp","modal")
        addat = rbind(ad,dat)
        addat = addat[!duplicated(addat[c("market","date","type")]),]
        dat = addat
      }
      
      
      
      dat = dat[dat$date>=early[m],]
      dat = dat[dat$date<late[m],]
      inkar = as.character(read.csv("/Users/tangjingwen/Dropbox/jingwen_tasks/SCM/inkarnataka.csv",header = F)$V1)
      dat = dat[!dat$market%in%inkar,]
      
      
      
      
      for (j in c(3,6)){
        dat[,j] = as.numeric(as.character(dat[,j]))
        dat[,j][dat[,j]<quantile(dat[,j],probs = c(0.01,0.99),na.rm = T)[1]|dat[,j]>quantile(dat[,j],probs = c(0.01,0.99),na.rm = T)[2]] = NA
      }
      
      h = dat
      h = h[!h$market=="virtual",]
      h1 = h[h$market%in%intervented,]
      h2 = h[!h$market%in%intervented,]
      originaltable = aggregate(h$amount,by = list(h$market),sum,na.rm = T)
      colnames(originaltable) = c("market","amount")
      originaltable = originaltable[order(originaltable$amount,decreasing = T),]
      write.xlsx(originaltable[originaltable$market%in%intervented,],paste("/Users/tangjingwen/Dropbox/jingwen_tasks/organized/result/synthetic control/",fmode,"/",product,"/",Model[m],"original table.xlsx",sep = ""))
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
      
      a = dat
      a1 = a[a$market%in%intervented,]
      a2 = a[!a$market%in%intervented,]
      
      amountrm1 = aggregate(a1$amount,by = list(a1$market),sum,na.rm = T)
      amountrm1 = amountrm1[order(amountrm1$x,decreasing = T),]
      thres1 = sum(amountrm1[,"x"],na.rm = T)*0.9
      for (i in 1:nrow(amountrm1)){
        if (sum(amountrm1[1:i,"x"],na.rm = T)>thres1) break
      }
      arm1 = amountrm1[i:nrow(amountrm1),"Group.1"]
      
      amountrm2 = aggregate(a2$amount,by = list(a2$market),sum,na.rm = T)
      amountrm2 = amountrm2[order(amountrm2$x,decreasing = T),]
      thres2 = sum(amountrm2[,"x"],na.rm = T)*0.9
      for (i in 1:nrow(amountrm2)){
        if (sum(amountrm2[1:i,"x"],na.rm = T)>thres2) break
      }
      arm2 = amountrm2[i:nrow(amountrm2),"Group.1"]
      
      arm = c(arm1,arm2)
      
      
      
      dat = dat[!dat$market%in%arm,]
      
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
      
      
      
      dat = dat[,c(1:3,5:7)]
      colnames(dat) = c("market","date","amount","minp","maxp","modal")
      dat$market = tolower(dat$market)
      dat$amount = as.numeric(as.character(dat$amount))
      dat$minp = as.numeric(as.character(dat$minp))
      dat$maxp = as.numeric(as.character(dat$maxp))
      dat$modal = as.numeric(as.character(dat$modal))
      write.xlsx(dat,paste("/Users/tangjingwen/Dropbox/jingwen_tasks/organized/result/synthetic control/",fmode,"/",product,"/",Model[m],"/common type organized dat.xlsx",sep = ""))
      
      ###################################################################################
      #rm(list = ls())
      
      lagbr = Lagbr[m]
      br = Br[m]
      
      filename = paste("/Users/tangjingwen/Dropbox/jingwen_tasks/organized/result/synthetic control/",fmode,"/",product,"/",Model[m],"/common type organized dat.xlsx",sep = "")
      dat = read.xlsx(filename,sheet = 1)
      
      #datbeforefeatures = dat[!dat$market%in%intervented,]
      
      unique(dat$market)[!unique(dat$market)%in%(DAT1$market)]
      dat1 = dat1[dat1$market%in%dat$market,]
      dat1 = na.omit(dat1)
      
      dat = join(dat,dat1,by = "market",type = "left",match = "first")
      dat = dat[(dat$market)%in%(dat1$market),]
      
      
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
      
      
      
      
      dat_1 = dat[dat$market%in%intervented,c("market","amount")]
      dat_1 = na.omit(dat_1)
      weight = aggregate(dat_1$amount,list(dat_1$market),mean)
      weight$x = weight$x/sum(weight$x,na.rm = T)
      colnames(weight) = c("market","weight")
      
      
      
      dat$date <- as.Date(dat$date-25569,origin = "1970-01-01")
      
      #wpi = read.xlsx("/Users/tangjingwen/Dropbox/jingwen_tasks/infrastructure_characteristics/monthly_index-2.xlsx",sheet = 1)
      
      #wpi = wpi[wpi$COMM_NAME == Wpiproduct[p],][,4:72]
      #wpimonth = seq(as.Date("2012/04/01"), as.Date("2017/12/01"), by = "month")
      #WPI = data.frame(month = wpimonth,wpi = as.numeric(as.character(wpi)))
      
      dat$month  <- as.Date(cut(dat$date,
                                breaks = "month"))
      #newd = join(dat,WPI,by = "month",type = "left")
      #newd$wpi = as.numeric(as.character(newd$wpi))
      #newd$modal = as.numeric(as.character(newd$modal))
      #newd$modal = (newd$modal)/(newd$wpi)
      #newd$modal = newd$modal*100
      #dat = newd[,1:(ncol(newd)-2)]
      
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
      for (l in 1:length(features)){
        temp = aggregate(tempdat[,(l+6)],by = list(tempdat$month,tempdat$market),mean,na.rm = T)
        colnames(temp)=c("month","market",features[l])
        newdat = join(newdat,temp,by = c("month","market"),type = "left")
      }
      
      
      h = newdat
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
      
      
      
      library(plyr)
      
      
      d = newdat
      
      d$month = as.Date(d$month,format="%d/%m/%Y")
      d_1 = d[d$market%in%subintervented,]
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
      ################to take the vdsa chosen features weighetd wpi marketwise here except amount and price
      
      
      
      
      write.xlsx(d,paste("/Users/tangjingwen/Dropbox/jingwen_tasks/organized/result/synthetic control/",fmode,"/",product,"/",Model[m],"/common type price features.xlsx",sep = ""))
      
      #####################################################################################
      
      print(product)
      setwd(paste("/Users/tangjingwen/Dropbox/jingwen_tasks/organized/result/synthetic control/",fmode,"/",product,"/",Model[m],sep = ""))
      dat = read.xlsx(paste("/Users/tangjingwen/Dropbox/jingwen_tasks/organized/result/synthetic control/",fmode,"/",product,"/",Model[m],"/common type price features.xlsx",sep = ""),sheet=1)
      #wide <- reshape(dat, v.names = c("modal"), idvar = "market",
      #timevar = "month", direction = "wide")
      dat = dat[!is.na(dat$modal),]
      
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
      
      #######combine with state
      ms = read.xlsx("/Users/tangjingwen/Dropbox/jingwen_tasks/SCM/market_districts.xlsx",sheet=1)
      
      colnames(ms)[1:4] = c("market","dis1","dis2","state")
      ms$market = tolower(ms$market)
      ms$state = tolower(ms$state)
      ms$dis1 = tolower(ms$dis1)
      ms$dis2 = tolower(ms$dis2)
      MS = ms
      ms = ms[,c(1,4)]
      #remove records which are "non returned" from google API
      #ms = ms[(ms$dis1!="none returned")&(ms$dis1!="zero_results"),]
      d = dat
      d = join(d,ms,by = "market",type = "inner")
      
      dat = d
      
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
      
      
      
      
      
      temp = dat[dat$market%in%intervented,]
      Dat = aggregate(temp$amount,by = list(temp$month),mean,na.rm=T)
      colnames(Dat) = c("month","amount")
      for (l in c(4:11)){
        name = colnames(temp)[l]
        colnames(temp)[l] = "target"
        newdat = as.data.frame(ddply(temp,.(month),summarise, wmean = weighted.mean(target,amount,na.rm = T)))
        colnames(newdat) = c("month",name)
        Dat = join(Dat,newdat,by = c("month"),type = "left")
        colnames(temp)[l] = name
        
      }
      Dat$market = "virtual"
      dat = rbind(dat,Dat)
      a = dat
      a1 = a[a$market%in%intervented,]
      a2 = a[!a$market%in%intervented,]
      
      
      
      for (k in 1:length(subintervented)){
        print(length(unique(a[a$market==subintervented[k],"month"])))
      }
      Sigma_reci = c()
      Sigma_reci2 = c()
      Gaps = list()
      success = c()
      for (j in (length(subintervented)):(length(subintervented))){
        
        try({
          i = (1:length(subintervented))[subintervented==subintervented[j]]
          print(j)
          print(subintervented[i])
          #marketmonth = unique(a[a$market==subintervented[i],"month"])
          #BELOW WE CHOOSE 90% AMOUNT MONTHS TO GET MORE POOL MARKETS FOR MONTHLY SYNTHTIC CONTROL
          
          temp = a[a$market==subintervented[i],]
          temp = temp[order(temp$amount,decreasing = T),]
          tempthres = sum(temp$amount,na.rm = T)*0.22
          for (l in 1:nrow(temp)){
            if (sum(temp$amount[1:l],na.rm = T)>tempthres) break
          }
          temp = temp[1:(l-1),]
          marketmonth = unique(temp$month)
          
          Nonin = unique(a2$market)
          nonin = c()
          for (nm in 1:length(Nonin)){
            temp = unique(a2[a2$market==Nonin[nm],"month"])
            if (sum(marketmonth%in%temp)==length(marketmonth)){
              print(Nonin[nm])
              nonin = c(nonin,Nonin[nm])
            }
          }
          a11 = a1[a1$market==subintervented[i],]  
          a11 = a11[a11$month%in%marketmonth,]
          a22 = a2[a2$market%in%nonin,]  
          a22 = a22[a22$month%in%marketmonth,]
          dat = rbind(a11,a22)
          dat$month <- as.Date(dat$month-25569,origin = "1970-01-01")
          dat$lag <- as.Date(cut(dat$month,
                                 breaks = lagbr))
          dat = dat[order(as.Date(dat$month)),]
          for (l in 3:(ncol(dat)-1)){
            dat[,l] = as.numeric(as.character(dat[,l]))
          }
          #dat = na.omit(dat)
          newdat = as.data.frame(aggregate(dat$modal, list(as.Date(dat$lag),dat$market), mean,na.rm = T))
          
          colnames(newdat) = c("lag","market","modal")
          wide <- reshape(newdat, v.names = c("modal"), idvar = "market",timevar = "lag", direction = "wide")
          dat = join(dat,wide,by = "market",type = "left",match = "first")
          
          dat = dat[,!duplicated(colnames(dat))]
          dat = dat[,-((1:ncol(dat))[colnames(dat)=="lag"])]
          
          
          
          
          market = unique(dat$market)
          dat$marketno = 0
          for (l in 1:length(market)){
            dat[dat$market==market[l],"marketno"] = l
          }
          uniquemonth = unique(as.numeric(dat$month))+25569
          dat = dat[order(dat$month),]
          dat$Month = rep(1:length(uniquemonth),each = length(market))
          datab = data.frame(day = sort(uniquemonth),month = (1:length(uniquemonth)))
          for (l in 3:ncol(dat)){
            dat[,l] = as.numeric(as.character(dat[,l]))
          }
          Dat = dat
          
          dat = dat[!dat$market%in%intervented,]
          
          dat_in = Dat[Dat$market%in%intervented,]
          #nonin = unique(dat$market)
          #portionbeforefeatures = sum(datbeforefeatures[datbeforefeatures$market%in%nonin,"amount"],na.rm = T)/sum(datbeforefeatures$amount,na.rm = T)
          
          #treat = as.data.frame(read.xlsx(paste("/Users/tangjingwen/Dropbox/jingwen_tasks/SCM/vdsa chosen features weighetd wpi marketwise/",product,#"/treat",Model[m],".xlsx",sep = ""),colNames  = F))
          #treat = treat$X1
          #subintervented = c(treat[treat%in%intervented])
          
          #w = as.data.frame(w)
          #w[,1] = nonin
          #v = as.data.frame(v)
          #v[,1] = colnames(dat)[6:(ncol(dat)-2)]
          #colnames(w)[-1] = subintervented
          #colnames(v)[-1] = subintervented
          setwd(paste("/Users/tangjingwen/Dropbox/jingwen_tasks/organized/result/synthetic control/",fmode,"/",product,"/",Model[m],sep = ""))
          inter_date = c()
          for (l in 1:nrow(intervention)){
            temp  = as.numeric(intervention[l,"X4"])-25569
            temp = as.Date(as.numeric(as.character(intervention[l,"X4"]))-25569,origin = "1970-01-01")
            temp <- as.numeric(as.Date(cut(temp,breaks = br)))+25569
            temp = datab$month[which.min(abs((datab$day)-temp))]
            inter_date = c(inter_date,temp)
          }
          for (t in 1:(length(unique(dat$month))-1)){
            print(t)
            already = tolower(as.character(intervention[inter_date<=t,"X3"]))
            notyet = tolower(as.character(intervention[inter_date>t,"X3"]))
            if (sum(dat_in[dat_in$market%in%already,"amount"],na.rm = T)>sum(dat_in[dat_in$market%in%notyet,"amount"],na.rm = T))
            {
              already = tolower(as.character(intervention[inter_date<=(t+1),"X3"]))
              notyet = tolower(as.character(intervention[inter_date>(t+1),"X3"]))
              if (sum(dat_in[dat_in$market%in%already,"amount"],na.rm = T)>sum(dat_in[dat_in$market%in%notyet,"amount"],na.rm = T))
              {
                subin = t
                print(t)
                break
              }
            }
          }
          temp = min(inter_date)
          inter_date = c(inter_date,temp)
          interinfo =data.frame(market = intervented,inter_date = inter_date)
          subinter = data.frame(market = subintervented)
          interinfo = join(subinter,interinfo,by = "market",type = "left")
          temp = min(interinfo$inter_date)
          interinfo[market=="virtual","inter_date"]=temp
          inter_date = interinfo$inter_date
          ################subintervention = datab[datab$day==subintervention,"month"]
          d = as.data.frame(rbind(dat,Dat[Dat$market==as.character(subintervented[i]),]))
          no = Dat[Dat$market==as.character(subintervented[i]),"marketno"][1]
          subintervention = inter_date[i]
          t0 = 1:(subintervention-1)
          
          dataprep.out <-
            dataprep(foo = d,
                     predictors = colnames(d)[c(7:((length(predictors)+5)+floor((subintervention-1)/setpre[m])))] ,
                     predictors.op = "mean" ,
                     time.predictors.prior = (min(d$Month)):(subintervention-1) ,
                     dependent = "modal",
                     unit.variable = "marketno",
                     unit.names.variable = "market",
                     time.variable = "Month",
                     treatment.identifier = as.character(subintervented[i]),
                     controls.identifier = nonin,
                     time.optimize.ssr = (min(d$Month)):(subintervention-1),
                     time.plot = (min(d$Month)):(max(d$Month))
            )
          synth.out <- synth(data.prep.obj = dataprep.out,method = "BFGS")
          synth.tables <- synth.tab(dataprep.res = dataprep.out,
                                    synth.res = synth.out)
          Gaps = c(Gaps,list((dataprep.out$Y1plot - (dataprep.out$Y0plot %*% synth.out$solution.w))[subintervention:(max(d$Month))]))
          nowGaps = (dataprep.out$Y1plot - (dataprep.out$Y0plot %*% synth.out$solution.w))[subintervention:(max(d$Month))]
          Sigma_reci = c(Sigma_reci,1/(sqrt(sum(nowGaps^2,na.rm = T)/length(nowGaps))))
          Sigma_reci2 = c(Sigma_reci2,1/((sqrt(sum(nowGaps^2,na.rm = T)/length(nowGaps)))*length(nowGaps)))
          success = c(success,subintervented[i])
          gaps <- dataprep.out$Y1plot - (dataprep.out$Y0plot %*% synth.out$solution.w)
          ##################################
          ##################################3
          ###################################
          balance = cbind(features,synth.tables$tab.pred[1:length(features),])
          colnames(balance)[1] = subintervented[i]
          Balance = cbind(Balance,balance)
          colnames(Balance)[1] = "Mode"
          
          rgaps = gaps/dataprep.out$Y1plot
          tgb = t.test(gaps[(min(d$Month)):(subintervention-1)])
          tga = t.test(gaps[subintervention:(max(d$Month))])
          trb = t.test(rgaps[(min(d$Month)):(subintervention-1)])
          tra = t.test(rgaps[subintervention:(max(d$Month))])
          m1 = mean(gaps[(min(d$Month)):(subintervention-1)])
          m2 = mean(gaps[subintervention:(max(d$Month))])
          m3 = mean(rgaps[(min(d$Month)):(subintervention-1)])
          m4 = mean(rgaps[subintervention:(max(d$Month))])
          gaps1 = gaps[t0]
          gaps2 = gaps[-t0]
          RMSPE1 = (sum(gaps1^2,na.rm = T)/length(gaps1))^0.5
          RMSPE2 = (sum(gaps2^2,na.rm = T)/length(gaps2))^0.5
          RMSPE = (sum(gaps^2,na.rm = T)/length(gaps))^0.5
          synth.tables <- synth.tab(dataprep.res = dataprep.out,
                                    synth.res = synth.out)
          diff = synth.tables$tab.pred[,1] - synth.tables$tab.pred[,2]#actual-synthetic
          gaps_analysis[((j-1)*length(Model)+m),][-(1:2)] = c(m1,tgb$statistic,tgb$p.value,m2,tga$statistic,tga$p.value,m3,trb$statistic,trb$p.value,m4,tra$statistic,tra$p.value,RMSPE1,RMSPE2,diff[1:(length(predictors))])
          #poolmarkets_amountportion[((j-1)*length(Model)+m),][-(1:2)] = c(portionbeforefeatures)
          #w[,(j+1)] = synth.tables$tab.w[,1]
          #v[,(j+1)] = c(as.numeric(synth.tables$tab.v),rep(0,(nrow(v)-length(as.numeric(synth.tables$tab.v)))))
          #temp = colnames(d)[6:(ncol(d)-2)]
          #temp = cbind(temp,synth.tables$tab.pred)
          #write.xlsx(temp,paste(subintervented[i],"table1.xlsx",sep = " "))
          pdf(paste("path plot5 of", subintervented[i],Model[m],".pdf",sep = " "),8,4)
          path.plot(synth.res = synth.out,
                    dataprep.res = dataprep.out,
                    Ylab = "modal price",
                    Xlab = Model[m],
                    Legend = c(as.character(subintervented[i]),paste("synthetic",as.character(subintervented[i]),sep = " ")),
                    Legend.position = "bottomright",
                    Main = subintervented[i]
          )
          abline(v=subintervention,lty = "dotted",lwd = 1.5)
          dev.off()
          pdf(paste("gap plot5 of", subintervented[i],Model[m],".pdf",sep = " "),8,4)
          gaps.plot(synth.res = synth.out,
                    dataprep.res = dataprep.out,
                    Ylab = "gap in modal price",
                    Xlab = Model[m],
                    Main = subintervented[i]
          )
          abline(v=subintervention,lty = "dotted",lwd = 1.5)
          dev.off()
          
        })
        finalportion = sum(originaltable[originaltable$market%in%nonin,"amount"],na.rm = T)/untreatedamount
        Market_info = qpcR:::cbind.na(Market_info,c(subintervented[i],nonin,length(nonin),finalportion))
      }
      Balance = rbind(colnames(Balance),Balance)
      balance_tables = qpcR:::rbind.na(balance_tables,Balance)

      cal_overall = function(p){
        temptable = originaltable[originaltable$market%in%success,]
        temptable = temptable[order(temptable$amount,decreasing = T),]
        tempthreshold = sum(temptable$amount,na.rm = T)*p
        for (l in 1:nrow(temptable)){
          if (sum(temptable[1:l,"amount"],na.rm = T)>=tempthreshold) break
        }
        chosenmar = temptable[1:l,"market"]
        chosenind = match(chosenmar,success)
        Sum = c()
        for (ll in chosenind){
          temparray = Gaps[[ll]]
          Sum = c(Sum,sum(temparray,na.rm = T))
        }
        outcome1 = (sum(Sigma_reci[chosenind]*Sum,na.rm = T))/sum(Sigma_reci[chosenind],na.rm = T)
        outcome2 = (sum(Sigma_reci2[chosenind]*Sum,na.rm = T))/sum(Sigma_reci[chosenind],na.rm = T)
        return(c(outcome1,outcome2))
      }
      for (o in 1:length(choice)){
        overall_effect[o,(m+1)] = cal_overall(choice[o])[1]
        overall_effect[o,(m+5)] = cal_overall(choice[o])[2]
      }
      
      
      
      data_details = createWorkbook()
      addWorksheet(data_details,"Market_info")
      addWorksheet(data_details,"Data_info")
      writeData(data_details,sheet = "Market_info",Market_info)
      writeData(data_details,sheet = "Data_info",Data_info)
      saveWorkbook(data_details, "data_details1.xlsx", overwrite = TRUE)
    }#a model ends
    
    gaps_analysis$type = 0
    gaps_analysis[(gaps_analysis[,7]>0)&(gaps_analysis[,8]<=0.05),"type"] = "+ *"
    gaps_analysis[(gaps_analysis[,7]>0)&(gaps_analysis[,8]>0.05),"type"] = "+"
    gaps_analysis[(gaps_analysis[,7]<0)&(gaps_analysis[,8]<=0.05),"type"] = "- *"
    gaps_analysis[(gaps_analysis[,7]<0)&(gaps_analysis[,8]>0.05),"type"] = "-"
    gaps_analysis2 = c("model","positive_significant","poitive_insignificant","negative_insignificant","negative_significant")
    for (l in 1:length(unique(gaps_analysis$V2))){
      temp = gaps_analysis[gaps_analysis$V2==(unique(gaps_analysis$V2))[l],"type"]
      positive_significant = sum(temp=="+ *")
      positive_insignificant = sum(temp=="+")
      negative_significant = sum(temp=="- *")
      negative_insignificant = sum(temp=="-")
      temp = c((unique(gaps_analysis$V2))[l],positive_significant,positive_insignificant,negative_insignificant,negative_significant)
      gaps_analysis2 = rbind(gaps_analysis2,temp)
    }
    setwd(paste("/Users/tangjingwen/Dropbox/jingwen_tasks/organized/result/synthetic control/",fmode,"/",product,sep = ""))
    wb <- createWorkbook()
    addWorksheet(wb,"overall_effect")
    addWorksheet(wb,"gaps_analysis")
    addWorksheet(wb,"gaps_analysis2")
    addWorksheet(wb,"balance_tables")
    writeData(wb,sheet = "overall_effect",overall_effect)
    writeData(wb,sheet = "gaps_analysis",gaps_analysis)
    writeData(wb,sheet = "gaps_analysis2",gaps_analysis2)
    writeData(wb,sheet = "balance_tables",balance_tables)
    saveWorkbook(wb, "result tables1.xlsx", overwrite = TRUE)
    }
  
  

}




bal = read.xlsx("/Users/tangjingwen/Dropbox/jingwen_tasks/organized/result/synthetic control/sub2/Groundnut/result tables.xlsx",sheet = "balance_tables")
no = (ncol(bal)-1)/4

for (k in 1:3){
  for (i in c(2:6,8:12,14:18,20:24)){
    temp=0
    for (j in 1:no){
      aa = as.numeric(bal[i,(4*j-2+k)])
      temp = temp + aa
    }
    temp = temp/no
    bal[i,4*no+1+k] = temp
  }
  
}
colnames(bal)[(ncol(bal)-2):ncol(bal)] = c("Treated","Synthetic","Sample Mean")
newbal = bal[,c(1,2,(ncol(bal)-2):ncol(bal))]
addWorksheet(wb,"overall_balance")
writeData(wb,sheet = "overall_balance",newbal)
saveWorkbook(wb, "result tables1.xlsx", overwrite = TRUE)
