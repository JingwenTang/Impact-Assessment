rm(list = ls())
library(openxlsx)
library(plyr)
library(ggplot2)
library(graphics)
dir.create("E:/Dropbox/jingwen_tasks/average plots/market wise")
Fo = c("pure average 2013-2017 wpi","market average 2013-2017 wpi","market average weighted 2013-2017 wpi")
for (fo in 3:3){
  print(Fo[fo])
  Product = c("Groundnut","Jowar(Sorghum)","Maize","Sunflower","Arhar (Tur-Red Gram)","Cotton","Green Gram (Moong)","Arecanut(Betelnut-Supari)","Bengal Gram(Gram)","Black Gram (Urd Beans)","Dry Chillies","Copra","Kulthi(Horse Gram)","Paddy(Dhan)")
  Wpiproduct = c("Groundnut Seed","Jowar","Maize","Sunflower","Arhar","Cotton Seed","Moong","Betelnut/Arecanut","Gram","All commodities","Chillies (Dry)","Copra (Coconut)","All commodities","All commodities")
  outputmodal = matrix(0,length(Product),5)
  outputmodal[,1] = Product
  colnames(outputmodal) = c("product","untreated before","treated before","untreated after","treated after")
  for (p in 2:length(Product)){
    try({
      product = Product[p]
      print(p)
      filename = paste("E:/Dropbox/jingwen_tasks/SCM/vdsa chosen features weighetd wpi marketwise/no combined/sub2/",product,"/13-17 week/",product," first data.xlsx",sep = "")
      dat = read.xlsx(filename,sheet = 1)
      
      #datbeforefeatures = dat[!dat$market%in%intervented,]
      treat = as.character(read.xlsx(paste("E:/Dropbox/jingwen_tasks/SCM/vdsa chosen features weighetd wpi marketwise/no combined/sub2/",product,"/treat.xlsx",sep = ""),colNames = F)$X1)
      subintervented = treat      
      
      
      intervention= as.data.frame(read.xlsx("E:/Dropbox/jingwen_tasks/infrastructure_characteristics/Market Details.xlsx",sheet = "Market Live Dates"))[3:160,]
      intervention$X3 = tolower(as.character(intervention$X3))
      
      temp = read.csv("E:/Dropbox/jingwen_tasks/CombinedArrivals/intervented change to organized.csv",header = F)
      colnames(temp) = c("int","org")
      temp$int = as.character(temp$int)
      temp$org = as.character(temp$org)
      inind = intervention$X3%in%temp$int
      orind = match(intervention$X3[inind],temp$int)
      intervention$X3[inind] = temp$org[orind]
      intervented = tolower(as.character(intervention$X3))
      intervented = c(intervented,"virtual")
      
      control = c()
      
      filename = paste("E:/Dropbox/jingwen_tasks/SCM/vdsa chosen features weighetd wpi marketwise/no combined/sub2/",product,"/13-17 week/",product," first data.xlsx",sep = "")
      dat7 = read.xlsx(filename,sheet = 1)
      control = unique(dat7$market)[!unique(dat7$market)%in%intervented]
      
      dat = dat[(dat$market%in%treat)|(dat$market%in%control),]
      
      
      #colnames(dat)[1] = "date"
      dat$date <- as.Date(dat$date-25569,origin = "1970-01-01")
      
      wpi = read.xlsx("E:/Dropbox/jingwen_tasks/infrastructure_characteristics/monthly_index-2.xlsx",sheet = 1)
      wpi = wpi[wpi$COMM_NAME == Wpiproduct[p],][,4:72]
      wpimonth = seq(as.Date("2012/04/01"), as.Date("2017/12/01"), by = "month")
      WPI = data.frame(month = wpimonth,wpi = as.numeric(as.character(wpi)))
      
      dat$month  <- as.Date(cut(dat$date,
                                breaks = "month"))
      newd = join(dat,WPI,by = "month",type = "left")
      newd$wpi = as.numeric(as.character(newd$wpi))
      newd$modal = as.numeric(as.character(newd$modal))
      newd$modal = (newd$modal)/(newd$wpi)
      newd$modal = newd$modal*100
      dat = newd[,1:(ncol(newd)-1)]
      
      market = unique(dat$market)
      dat$marketno = 0
      for (i in 1:length(market)){
        dat[dat$market==market[i],"marketno"] = i
      }
      uniquemonth = unique(dat$month)
      dat = dat[order(dat$month),]
      dat$Month = as.numeric(as.factor(dat$month))
      datab = data.frame(day = sort(uniquemonth),month = (1:length(uniquemonth)))
      intervention= as.data.frame(read.xlsx("E:/Dropbox/jingwen_tasks/infrastructure_characteristics/Market Details.xlsx",sheet = "Market Live Dates"))[3:160,]
      intervented = tolower(as.character(intervention$X3))
      intervented = c(intervented)
      dat = na.omit(dat)
      dat_un = dat[!dat$market%in%intervented,]
      dat_in = dat[dat$market%in%intervented,]
      subintervention = c()
      for (i in 1:nrow(intervention)){
        temp  = as.numeric(intervention[i,"X4"])-25569
        temp = as.Date(as.numeric(as.character(intervention[i,"X4"]))-25569,origin = "1970-01-01")
        #temp <- as.numeric(as.Date(cut(temp,breaks = "3 months")))+25569
        temp = datab$month[which.min(abs((datab$day)-temp))]
        subintervention = c(subintervention,temp)
      }
      for (t in 1:(length(unique(dat$month))-1)){
        print(t)
        already = tolower(as.character(intervention[subintervention<=t,"X3"]))
        notyet = tolower(as.character(intervention[subintervention>t,"X3"]))
        if (sum(dat_in[dat_in$market%in%already,"amount"],na.rm = T)>sum(dat_in[dat_in$market%in%notyet,"amount"],na.rm = T))
        {
          already = tolower(as.character(intervention[subintervention<=(t+1),"X3"]))
          notyet = tolower(as.character(intervention[subintervention>(t+1),"X3"]))
          if (sum(dat_in[dat_in$market%in%already,"amount"],na.rm = T)>sum(dat_in[dat_in$market%in%notyet,"amount"],na.rm = T))
          {
            subin = t
            print(t)
            break
          }
        }
      }
      subintervention = subin
      
      
      if (fo ==1){
        #previous
        wu1 = dat_un[dat_un$Month<subintervention,"amount"]/sum(dat_un[dat_un$Month<subintervention,"amount"])
        wu2 = dat_un[dat_un$Month>=subintervention,"amount"]/sum(dat_un[dat_un$Month>=subintervention,"amount"])
        wi1 = dat_in[dat_in$Month<subintervention,"amount"]/sum(dat_in[dat_in$Month<subintervention,"amount"])
        wi2 = dat_in[dat_in$Month>=subintervention,"amount"]/sum(dat_in[dat_in$Month>=subintervention,"amount"])
        dat_un1 = sum(wu1*dat_un[dat_un$Month<subintervention,"modal"],na.rm = T)
        dat_un2 = sum(wu2*dat_un[dat_un$Month>=subintervention,"modal"],na.rm = T)
        dat_in1 = sum(wi1*dat_in[dat_in$Month<subintervention,"modal"],na.rm = T)
        dat_in2 = sum(wi2*dat_in[dat_in$Month>=subintervention,"modal"],na.rm = T)
        outputmodal[p,][-1] = c(dat_un1,dat_in1,dat_un2,dat_in2)
        Before_UMP = c(dat_in1,dat_un1)
        After_UMP = c(dat_in2,dat_un2)
      }
      
      if (fo == 2){
        dat_un1 = dat_un[dat_un$Month<subintervention,]
        dat_un2 = dat_un[dat_un$Month>=subintervention,]
        dat_in1 = dat_in[dat_in$Month<subintervention,]
        dat_in2 = dat_in[dat_in$Month>=subintervention,]
        #unweighetd
        pu1 = aggregate(dat_un1$modal,by = list(dat_un1$market),mean)
        pu1 = mean(pu1$x)
        pu2 = aggregate(dat_un2$modal,by = list(dat_un2$market),mean)
        pu2 = mean(pu2$x)
        pi1 = aggregate(dat_in1$modal,by = list(dat_in1$market),mean)
        pi1 = mean(pi1$x)
        pi2 = aggregate(dat_in2$modal,by = list(dat_in2$market),mean)
        pi2 = mean(pi2$x)
        outputmodal[p,][-1] = c(pu1,pi1,pu2,pi2)
        Before_UMP = c(pi1,pu1)
        After_UMP = c(pi2,pu2)
      }
      
      if (fo == 3){
        
        dat_un1 = dat_un[dat_un$Month<subintervention,]
        dat_un2 = dat_un[dat_un$Month>=subintervention,]
        dat_in1 = dat_in[dat_in$Month<subintervention,]
        dat_in2 = dat_in[dat_in$Month>=subintervention,]
        #weighted
        pu1 = ddply(dat_un1,.(market),summarise, mean = weighted.mean(modal,amount))
        pu2 = ddply(dat_un2,.(market),summarise, mean = weighted.mean(modal,amount))
        pi1 = ddply(dat_in1,.(market),summarise, mean = weighted.mean(modal,amount))
        pi2 = ddply(dat_in2,.(market),summarise, mean = weighted.mean(modal,amount))
        colnames(pu1) = c("market","modal")
        colnames(pu2) = c("market","modal")
        colnames(pi1) = c("market","modal")
        colnames(pi2) = c("market","modal")
        au1 = aggregate(dat_un1$amount,by = list(dat_un1$market),sum)
        au2 = aggregate(dat_un2$amount,by = list(dat_un2$market),sum)
        ai1 = aggregate(dat_in1$amount,by = list(dat_in1$market),sum)
        ai2 = aggregate(dat_in2$amount,by = list(dat_in2$market),sum)
        colnames(au1) = c("market","amount")
        colnames(au2) = c("market","amount")
        colnames(ai1) = c("market","amount")
        colnames(ai2) = c("market","amount")
        cu1 = join(pu1,au1,by = "market")
        cu2 = join(pu2,au2,by = "market")
        ci1 = join(pi1,ai1,by = "market")
        ci2 = join(pi2,ai2,by = "market")
        pu1 = weighted.mean(cu1$modal,cu1$amount)
        pu2 = weighted.mean(cu2$modal,cu2$amount)
        pi1 = weighted.mean(ci1$modal,ci1$amount)
        pi2 = weighted.mean(ci2$modal,ci2$amount)
        outputmodal[p,][-1] = c(pu1,pi1,pu2,pi2)
        Before_UMP = c(pi1,pu1)
        After_UMP = c(pi2,pu2)
      }
      
      
      df = data.frame(Before_UMP = Before_UMP,After_UMP = After_UMP)
      rownames(df) = c("Karnataka","Rest")
      df = as.matrix(df)
      dir.create(paste("E:/Dropbox/jingwen_tasks/average plots/outlier marketwise/",Fo[fo],sep = ""))
      setwd(paste("E:/Dropbox/jingwen_tasks/average plots/outlier marketwise/",Fo[fo],sep = ""))
      pdf(paste(product,".pdf",sep = ""),4,4)
      barplot(df,space = c(0.5,4),ylim = c(0,(max(max(df))+1000)),
              main = paste("Modal Price of ",product,sep = ""),
              xlab = "",ylab = "",
              col = c("darkslategray4","darkorange3"),
              legend.text = rownames(df),beside = T,
              args.legend = list(x = "top",cex = 0.7,bty = "n"),
              cex.axis=0.65,cex.names=0.65,cex.main = 0.8)
      dev.off()
    })

  }
  setwd(paste("E:/Dropbox/jingwen_tasks/average plots/outlier marketwise/",Fo[fo],sep = ""))
  write.xlsx(outputmodal,"average of modal prices.xlsx")
}



###insert 27 to get the names of the control markets we use for this commodity in the synthetic control
temp = read.xlsx(paste("E:/Dropbox/jingwen_tasks/SCM/vdsa chosen features weighetd wpi marketwise/no combined/sub2/",product,"/12-17 mon-qua/data_details1.xlsx",sep = ""),sheet="Market_info")
temp = temp[-1,]
nonin = c()
for (l in 1:ncol(temp)){
  num = sum(!is.na(temp[,l]))-2
  nonin = c(nonin,temp[1:num,l])
}
control = unique(nonin)
