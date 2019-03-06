#5-10 14size2
rm(list = ls())
library(readr)
library(openxlsx)
library(plyr)
library(dplyr)
library(Synth)
library(ggplot2)
library(graphics)
library(qpcR)
fMode = c("full","sub","sub2")
f = list()
f[[1]]= c("annual","total_ar","lit_t","male_saw","annmai","lit_ru","ptmrkt","lroad","agrl_t")
f[[2]] = c("annual","total_ar","lit_t","male_saw")
f[[3]] = c("annual","total_ar","lit_t","agrl_t","ptmrkt")
for (size in 3:3){
  fmode = fMode[size]
  f = f[[size]]
  dir.create(paste("E:/Dropbox/jingwen_tasks/SCM/vdsa chosen features weighetd wpi marketwise/placebo/",fmode,sep = ""))
  Product = c("Groundnut","Jowar(Sorghum)","Maize","Sunflower","Arhar (Tur-Red Gram)","Cotton","Green Gram (Moong)","Arecanut(Betelnut-Supari)","Bengal Gram(Gram)","Black Gram (Urd Beans)","Dry Chillies","Copra","Kulthi(Horse Gram)","Paddy(Dhan)")
  Wpiproduct = c("Groundnut Seed","Jowar","Maize","Sunflower","Arhar","Cotton Seed","Moong","Betelnut/Arecanut","Gram","All commodities","Chillies (Dry)","Copra (Coconut)","All commodities","All commodities")
  Model = c("12-17 mon-mon","12-17 mon-qua","12-17 qua-qua","12-17 qua-half")
  Begin = c(2012,2012,2012,2012)
  Over = c(2017,2017,2017,2017)
  Br = c("month","month","3 months","3 months")
  early = as.Date(c("2012-04-01","2012-04-01","2012-04-01","2012-04-01"))
  late = as.Date(c("2018-01-01","2018-01-01","2018-01-01","2018-01-01"))
  Lagbr  = c("month","3 months","3 months","6 months")
  
  setpre = c(1,3,1,2)
  
  dat1 = read.xlsx("E:/Dropbox/jingwen_tasks/SCM/vdsa chosen features weighetd wpi marketwise/district features.xlsx",sheet = 1)
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

  for (p in 1:length(Product)){
 subintervented = as.character(as.array(read.csv(paste("E:/Dropbox/jingwen_tasks/SCM/vdsa chosen features weighetd wpi marketwise/no combined/",fmode,"/",Product[p],"/treat.csv",sep = ""),header = F)$V1))
 product = Product[p]
 dir.create(paste("E:/Dropbox/jingwen_tasks/SCM/vdsa chosen features weighetd wpi marketwise/placebo/",fmode,"/",product,sep = ""))
 
 features = Features[[p]]
 dat1 = DAT1[,c("market",features)]
 ####dat1 = na.omit(dat1)#because that the features don't change over time so in case that it will be NA for all time periods we just remove them
 dat1 = as.data.frame(dat1)
 predictors = features
 for (s in 1:length(subintervented)){
    try({
      dir.create(paste("E:/Dropbox/jingwen_tasks/SCM/vdsa chosen features weighetd wpi marketwise/placebo/",fmode,"/",product,"/",subintervented[s],sep = ""))
      
      
            #treat = as.character(as.array(read.csv(paste("E:/Dropbox/jingwen_tasks/SCM/vdsa chosen features weighetd wpi marketwise/",fmode,"/",Product[p],"/treat.csv",sep = ""),header = F)$V1))
        #subintervented = c(treat[treat%in%intervented])
        #treat = as.character(as.array(read.csv(paste("E:/Dropbox/jingwen_tasks/SCM/vdsa chosen features weighetd wpi marketwise/placebo/",fmode,"/",Product[p],"/treat.csv",sep = ""),header = F)$V1))
        notintervented = read.xlsx(paste("E:/Dropbox/jingwen_tasks/SCM/vdsa chosen features weighetd wpi marketwise/placebo/",fmode,"/",product,"/treat.xlsx",sep = ""),colNames = F)[,1]
        gaps_analysis = matrix(0,length(notintervented)*length(Model),16+length(predictors))
        temp = c("mean of gaps before","t value","p value","mean of gaps after","t value","p value","mean of relative gaps before","t value","p value","mean of relative gaps after","t value","p value","RMSPE_before","RMSPE_after",predictors)
        gaps_analysis = as.data.frame(gaps_analysis)
        gaps_analysis[,1] = rep(notintervented,each = length(Model))
        gaps_analysis[,2] = rep(Model,length(notintervented))
        colnames(gaps_analysis)[-(1:2)] = temp
        
        balance_tables = c()
        
        choice = seq(from = 0.05,to = 1, by = 0.05)
        overall_effect = matrix(0,length(choice),(2*length(Model)+1))
        overall_effect = as.data.frame(overall_effect)
        colnames(overall_effect) = c("portion_of_markets",Model,Model)
        overall_effect$portion_of_markets = choice
        
        for (m in 2:2){
          dir.create(paste("E:/Dropbox/jingwen_tasks/SCM/vdsa chosen features weighetd wpi marketwise/placebo/",fmode,"/",product,"/",subintervented[s],"/",Model[m],sep = ""))
          setwd(paste("E:/Dropbox/jingwen_tasks/SCM/vdsa chosen features weighetd wpi marketwise/placebo/",fmode,"/",product,"/",subintervented[s],"/",Model[m],sep = ""))
          Balance = rep(Model[m],(length(features)))
          Market_info = c()
          Data_info = matrix(0,6,10)
          Data_info = as.data.frame(Data_info)
          colnames(Data_info) = c("state","total markets","treated markets","untreated markets",
                                  "total records","treated markets records","untreated markets records",
                                  "total amount portion","treated amount portion","untreated amount portion")
          Data_info$state = c("all organized data (2012.04-2017.12 and unknown karnataka markets removed and replacing 1% outliers of price and amount with NA)",
                              "90% of markets for treated and untreated respectively",
                              "after combined with features","after adjusted with wpi","weighted mean price of unit time",
                              "remove missing values in modal price")
          dat = c()
          for (i in Begin[m]:Over[m]){
            filename = paste("E:/Dropbox/jingwen_tasks/Organized Data/",i,"_Data/",product,".xlsx",sep = "")
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
          
          
          dat = dat[dat$date>=early[m],]
          dat = dat[dat$date<late[m],]
          inkar = as.character(read.csv("E:/Dropbox/jingwen_tasks/SCM/inkarnataka.csv",header = F)$V1)
          dat = dat[!dat$market%in%inkar,]
          weird = as.character(read.csv("E:/Dropbox/jingwen_tasks/SCM/weird control markets.csv",header = F)$V1)
          dat = dat[!dat$market%in%weird,]
          
          
          
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
          write.xlsx(originaltable[!originaltable$market%in%intervented,],paste("E:/Dropbox/jingwen_tasks/SCM/vdsa chosen features weighetd wpi marketwise/",fmode,"/",product,"/",Model[m],"original table.xlsx",sep = ""))
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
          
          
          lagbr = Lagbr[m]
          br = Br[m]
          
          #####################################################################################
          
          print(product)
          setwd(paste("E:/Dropbox/jingwen_tasks/SCM/vdsa chosen features weighetd wpi marketwise/placebo/",fmode,"/",product,"/",Model[m],sep = ""))
          dat = read.xlsx(paste("E:/Dropbox/jingwen_tasks/SCM/vdsa chosen features weighetd wpi marketwise/",fmode,"/",product,"/",Model[m],"/common type price features.xlsx",sep = ""),sheet=1)
          #wide <- reshape(dat, v.names = c("modal"), idvar = "market",
          #timevar = "month", direction = "wide")
          dat = dat[!is.na(dat$modal),]
          
          inkar = as.character(read.csv("E:/Dropbox/jingwen_tasks/SCM/inkarnataka.csv",header = F)$V1)
          dat = dat[!dat$market%in%inkar,]
          weird = as.character(read.csv("E:/Dropbox/jingwen_tasks/SCM/weird control markets.csv",header = F)$V1)
          dat = dat[!dat$market%in%weird,]
          
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
          
          
          
          a = dat
          a1 = a[a$market%in%intervented,]
          a2 = a[!a$market%in%intervented,]
          
          
          for (k in 1:length(notintervented)){
            print(length(unique(a[a$market==notintervented[k],"month"])))
          }
          Sigma_reci = c()
          Sigma_reci2 = c()
          Gaps = list()
          success = c()
          
          
          
          
          for (j in 1:length(notintervented)){
            
            try({
              i = (1:length(notintervented))[notintervented==notintervented[j]]
              print(j)
              print(notintervented[i])
              #marketmonth = unique(a[a$market==notintervented[i],"month"])
              #BELOW WE CHOOSE 90% AMOUNT MONTHS TO GET MORE POOL MARKETS FOR MONTHLY SYNTHTIC CONTROL
              
              temp = a[a$market==notintervented[i],]
              temp = temp[order(temp$amount,decreasing = T),]
              
              marketmonth = unique(temp$month)
              
              Nonin = unique(a2$market)[!unique(a2$market)==notintervented[i]]
              nonin = c()
              for (nm in 1:length(Nonin)){
                temp = unique(a2[a2$market==Nonin[nm],"month"])
                if (sum(marketmonth%in%temp)==length(marketmonth)){
                  print(Nonin[nm])
                  nonin = c(nonin,Nonin[nm])
                }
              }
              a11 = a2[a2$market==notintervented[i],]  
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
              
              setwd(paste("E:/Dropbox/jingwen_tasks/SCM/vdsa chosen features weighetd wpi marketwise/placebo/",fmode,"/",product,"/",subintervented[s],"/",Model[m],sep = ""))
              inter_date = c()
              colnames(intervention) = c("number","code","market","date")
              #intervention = intervention[intervention$market%in%unique(a1$market),]
              for (l in 1:nrow(intervention)){
                temp  = as.numeric(intervention[l,"date"])-25569
                temp = as.Date(as.numeric(as.character(intervention[l,"date"]))-25569,origin = "1970-01-01")
                temp <- as.numeric(as.Date(cut(temp,breaks = br)))+25569
                temp = datab$month[which.min(abs((datab$day)-temp))]
                inter_date = c(inter_date,temp)
              }
              interinfo =data.frame(market = intervented[intervented!="virtual"],inter_date = inter_date)
              subinter = data.frame(market = subintervented)
              interinfo = join(subinter,interinfo,by = "market",type = "left")
              subintervention = interinfo[interinfo$market==subintervented[s],"inter_date"]
              ################subintervention = datab[datab$day==subintervention,"month"]
              d = as.data.frame(dat)
              no = Dat[Dat$market==as.character(notintervented[i]),"marketno"][1]
              
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
                         treatment.identifier = as.character(notintervented[i]),
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
              success = c(success,notintervented[i])
              gaps <- dataprep.out$Y1plot - (dataprep.out$Y0plot %*% synth.out$solution.w)
              ##################################
              ##################################3
              ###################################
              balance = cbind(features,synth.tables$tab.pred[1:length(features),])
              colnames(balance)[1] = notintervented[i]
              Balance = cbind(Balance,balance)
              colnames(Balance)[1] = "Mode"
              
              rgaps = gaps/(dataprep.out$Y0plot %*% synth.out$solution.w)
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
              #write.xlsx(temp,paste(notintervented[i],"table1.xlsx",sep = " "))
              pdf(paste("path plot5 of", notintervented[i],Model[m],".pdf",sep = " "),8,4)
              path.plot(synth.res = synth.out,
                        dataprep.res = dataprep.out,
                        Ylab = "modal price",
                        Xlab = Model[m],
                        Legend = c(as.character(notintervented[i]),paste("synthetic",as.character(notintervented[i]),sep = " ")),
                        Legend.position = "bottomright",
                        Main = notintervented[i]
              )
              abline(v=subintervention,lty = "dotted",lwd = 1.5)
              dev.off()
              pdf(paste("gap plot5 of", notintervented[i],Model[m],".pdf",sep = " "),8,4)
              gaps.plot(synth.res = synth.out,
                        dataprep.res = dataprep.out,
                        Ylab = "gap in modal price",
                        Xlab = Model[m],
                        Main = notintervented[i]
              )
              abline(v=subintervention,lty = "dotted",lwd = 1.5)
              dev.off()
              
            })
            
            finalportion = sum(originaltable[originaltable$market%in%nonin,"amount"],na.rm = T)/untreatedamount
            Market_info = qpcR:::cbind.na(Market_info,c(notintervented[i],nonin,length(nonin),finalportion))
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
          saveWorkbook(data_details, "data_details.xlsx", overwrite = TRUE)
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
        setwd(paste("E:/Dropbox/jingwen_tasks/SCM/vdsa chosen features weighetd wpi marketwise/placebo/",fmode,"/",product,"/",subintervented[s],sep = ""))
        wb <- createWorkbook()
        addWorksheet(wb,"overall_effect")
        addWorksheet(wb,"gaps_analysis")
        addWorksheet(wb,"gaps_analysis2")
        addWorksheet(wb,"balance_tables")
        writeData(wb,sheet = "overall_effect",overall_effect)
        writeData(wb,sheet = "gaps_analysis",gaps_analysis)
        writeData(wb,sheet = "gaps_analysis2",gaps_analysis2)
        writeData(wb,sheet = "balance_tables",balance_tables)
        saveWorkbook(wb, paste(subintervented[s],"result tables.xlsx",sep = ""), overwrite = TRUE)
    })
      }
      
      treated = read.xlsx("E:/Dropbox/jingwen_tasks/SCM/vdsa chosen features weighetd wpi marketwise/no combined/sub2/Groundnut/result tables1.xlsx",sheet = "gaps_analysis")
      
      untreated = read.xlsx(paste("E:/Dropbox/jingwen_tasks/SCM/vdsa chosen features weighetd wpi marketwise/placebo/",fmode,"/",subintervented[s],"/",product,"/",subintervented[s],"result tables.xlsx",sep = ""),sheet = "gaps_analysis")
      colnames(treated)[1:2] = c("market","model")
      treated = treated[treated$market==subintervented[s],]
      colnames(untreated)[1:2] = c("market","model")
      treated = treated[treated$model=="12-17 mon-qua",c(1,15,16)]
      untreated = untreated[untreated$model=="12-17 mon-qua",c(1,15,16)]
      treated$ratio = treated$RMSPE_after/treated$RMSPE_before
      untreated$ratio = untreated$RMSPE_after/untreated$RMSPE_before
      treated$treat = "treated"
      untreated$treat = "untreated"
      treated = treated[order(treated$ratio,decreasing = T),]
      untreated = untreated[order(untreated$ratio,decreasing = T),]
      dat = rbind(treated,untreated)
      dat$treat = factor(dat$treat)
      qt = 1-ecdf(dat$ratio)(dat[dat$market==subintervented[s],"ratio"])
      
      
      pdf(paste("E:/Dropbox/jingwen_tasks/SCM/vdsa chosen features weighetd wpi marketwise/placebo/",fmode,"/",product,"/",subintervented[s],"/",subintervented[s]," dot chart.pdf",sep = ""), width=8, height=4)
      print(ggplot(dat, aes(ratio, reorder(market,ratio)))+
              geom_point(aes(colour = treat))+
              annotate("text", x=max(dat$ratio-1), y=2, label= paste("quantile:",round(qt,digits = 3),sep = ""),size=3.3))
      dev.off()
    
    

    
  }
  
  
  }


