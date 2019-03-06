rm(list = ls())
setwd("E:/Dropbox/jingwen_tasks/SCM/vdsa chosen features weighetd wpi marketwise/no combined/missing values check amount based on modal")
library(openxlsx)
library(plyr)
library(lubridate)
library(ggplot2)
library(RColorBrewer)
library(wesanderson)
Product = c("Groundnut","Jowar(Sorghum)","Maize","Sunflower","Arhar (Tur-Red Gram)","Cotton","Green Gram (Moong)","Arecanut(Betelnut-Supari)","Bengal Gram(Gram)","Black Gram (Urd Beans)","Dry Chillies","Copra","Kulthi(Horse Gram)","Paddy(Dhan)")
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
for (p in 1:1){
  product = Product[p]
    ##check with missing valus of amount just based on all the dates we have
  d = read.xlsx(paste("E:/Dropbox/jingwen_tasks/SCM/vdsa chosen features weighetd wpi marketwise/no combined/sub2/",product,"/12-17 mon-qua marketwise daily/fifth data.xlsx",sep = ""),sheet=1)
  d = d[!d$market=="virtual",]
  d = d[,c(1:3,6)]
  #colnames(d)[1] = "date"
  d$date = as.Date(d$date-25569,origin = "1970-01-01")
  d1 = aggregate(d$amount,by = list(d$market,d$date),mean,na.rm = T)
  colnames(d1) = c("market","date","amount")
  d1$amount[!is.finite(d1$amount)] = NA
  d = d1
  #check after choosing 90% markets
  D = data.frame(date = rep(as.Date("2013-01-01"):as.Date("2017-12-31"),length(unique(d$market))),market = rep(unique(d$market),each = length(as.Date("2013-01-01"):as.Date("2017-12-31"))))
  D = join(D,d,by = c("date","market"))
  D$date = as.Date(D$date,origin = "1970-01-01")
  D$na = is.na(D$amount)
  D$day = 1
  
  D$month = as.Date(cut(D$date,br = "month"))
  D$week = as.Date(cut(D$date,br = "week"))
  monthcheck1 = aggregate(D$na,by = list(D$month),sum,na.rm = T)
  monthcheck2 = aggregate(D$day,by = list(D$month),sum,na.rm = T)
  colnames(monthcheck1) = c("month","no.na")
  colnames(monthcheck2) = c("month","no.day")
  monthcheck = join(monthcheck1,monthcheck2,by = "month")
  monthcheck$missing_portion = monthcheck$no.na/monthcheck$no.day
  
  weekcheck1 = aggregate(D$na,by = list(D$week),sum,na.rm = T)
  weekcheck2 = aggregate(D$day,by = list(D$week),sum,na.rm = T)
  colnames(weekcheck1) = c("week","no.na")
  colnames(weekcheck2) = c("week","no.day")
  weekcheck = join(weekcheck1,weekcheck2,by = "week")
  weekcheck$missing_portion = weekcheck$no.na/weekcheck$no.day
  
  quantity_missing_portion_monthly_ts = ts(monthcheck$missing_portion,frequency = 12,start = c(2013,1))
  quantity_missing_portion_weekly_ts = ts(weekcheck$missing_portion,frequency = 52,start = c(2013,1))
  
  pdf(paste(product,"quantity_missing_portion_monthly_ts.pdf"),width = 8,height = 4)
  plot.ts(quantity_missing_portion_monthly_ts)
  dev.off()
  pdf(paste(product,"quantity_missing_portion_weekly_ts.pdf"),width = 8,height = 4)
  plot.ts(quantity_missing_portion_weekly_ts)
  dev.off()
  
  ###check the missing distribution for modal price
  d = read.xlsx(paste("E:/Dropbox/jingwen_tasks/SCM/vdsa chosen features weighetd wpi marketwise/no combined/sub2/",product,"/12-17 mon-qua marketwise daily/fifth data.xlsx",sep = ""),sheet=1)
  d = d[!d$market=="virtual",]
  d = d[,c(1:3,6)]
  #colnames(d)[1] = "date"
  d$date = as.Date(d$date-25569,origin = "1970-01-01")
  d1 = aggregate(d$modal,by = list(d$market,d$date),mean,na.rm = T)
  colnames(d1) = c("market","date","modal")
  d1$modal[!is.finite(d1$modal)] = NA
  d = d1
  #check after choosing 90% markets
  D = data.frame(date = rep(as.Date("2013-01-01"):as.Date("2017-12-31"),length(unique(d$market))),market = rep(unique(d$market),each = length(as.Date("2013-01-01"):as.Date("2017-12-31"))))
  D = join(D,d,by = c("date","market"))
  D$date = as.Date(D$date,origin = "1970-01-01")
  D$na = is.na(D$modal)
  D$day = 1
  
  D$month = as.Date(cut(D$date,br = "month"))
  D$week = as.Date(cut(D$date,br = "week"))
  monthcheck1 = aggregate(D$na,by = list(D$month),sum,na.rm = T)
  monthcheck2 = aggregate(D$day,by = list(D$month),sum,na.rm = T)
  colnames(monthcheck1) = c("month","no.na")
  colnames(monthcheck2) = c("month","no.day")
  monthcheck = join(monthcheck1,monthcheck2,by = "month")
  monthcheck$missing_portion = monthcheck$no.na/monthcheck$no.day
  
  weekcheck1 = aggregate(D$na,by = list(D$week),sum,na.rm = T)
  weekcheck2 = aggregate(D$day,by = list(D$week),sum,na.rm = T)
  colnames(weekcheck1) = c("week","no.na")
  colnames(weekcheck2) = c("week","no.day")
  weekcheck = join(weekcheck1,weekcheck2,by = "week")
  weekcheck$missing_portion = weekcheck$no.na/weekcheck$no.day
  
  modal_missing_portion_monthly_ts = ts(monthcheck$missing_portion,frequency = 12,start = c(2013,1))
  modal_missing_portion_weekly_ts = ts(weekcheck$missing_portion,frequency = 52,start = c(2013,1))
  
  pdf(paste(product,"modal_missing_portion_monthly_ts.pdf"),width = 8,height = 4)
  plot.ts(modal_missing_portion_monthly_ts)
  dev.off()
  pdf(paste(product,"modal_missing_portion_weekly_ts.pdf"),width = 8,height = 4)
  plot.ts(modal_missing_portion_weekly_ts)
  dev.off()
  
  
  
  
  
  marketcheck = aggregate(D$na,by = list(D$market),sum,na.rm = T)
  colnames(marketcheck) = c("market","fraction.missing_days")
  marketcheck$fraction.missing_days = marketcheck$fraction.missing_days/length(as.Date("2013-01-01"):as.Date("2017-12-31"))
  market_check = ggplot(data = marketcheck,aes(x= market,y = fraction.missing_days))+
    geom_bar(stat = "identity",width = 0.5)+
    theme(axis.text.x=element_blank())
  dat = marketcheck
  dat$treat = dat$market%in%intervented
  treatcheck = aggregate(dat$fraction.missing_days,by = list(dat$treat),mean)
  colnames(treatcheck) = c("treat","fraction.missing_days")
  plt_treat_fraction = ggplot(data = treatcheck,aes(x=reorder(treat,-fraction.missing_days),y=fraction.missing_days))+
    geom_bar(stat = "identity",width =0.3)+
    ylim(c(0,(max(treatcheck$fraction.missing_days)+0.1)))+
    geom_text(size = 3,aes(x=treatcheck$treat,y=treatcheck$fraction.missing_days,label=round(treatcheck$fraction.missing_days,3)),vjust = -1)+
    theme(axis.text=element_text(size=10))+
    xlab("treat")
  
  
  #
  d = read.xlsx(paste("E:/Dropbox/jingwen_tasks/SCM/vdsa chosen features weighetd wpi marketwise/no combined/sub2/",product,"/12-17 mon-qua marketwise daily/fifth data.xlsx",sep = ""),sheet=1)
  d = d[!d$market=="virtual",]
  d = d[,c(1:3,6)]
  #colnames(d)[1] = "date"
  d$date = as.Date(d$date-25569,origin = "1970-01-01")
  d$date = as.Date(cut(d$date,br = "month"))
  d$na = (!is.na(d$modal))&(is.na(d$amount))
  d$na1 = !is.na(d$modal)
  d1 = aggregate(d$na1,by = list(d$market,d$date),sum,na.rm = T)
  colnames(d1) = c("market","date","modal")
  d = aggregate(d$na,by = list(d$market,d$date),sum,na.rm = T)
  colnames(d) = c("market","date","na")
  d = join(d,d1,by = c("market","date"),type = "left")
  Dat = d
  Dat$month = month(Dat$date)
  
  
  ### first check the missing values distribution with months
  ### we use data from 2012-04-01 to 2017-12-01  
  ### so we remove 2012 and start from 2013
  #dat = Dat[Dat$date>="2013-01-01",]
  monthcheck = aggregate(Dat$na,by = list(Dat$month),sum)
  colnames(monthcheck) = c("month","number_of_NA")
  monthcheck = as.data.frame(monthcheck)
  plt_month = ggplot(data = monthcheck,aes(x=month,y=number_of_NA))+
    geom_bar(stat = "identity")+
    ylim(c(0,(max(monthcheck$number_of_NA)+500)))+
    scale_x_continuous(breaks=1:12)+
    geom_text(size = 3,aes(x=1:12,y=monthcheck$number_of_NA,label=monthcheck$number_of_NA),vjust = -1)
  pdf(paste("E:/Dropbox/jingwen_tasks/SCM/vdsa chosen features weighetd wpi marketwise/no combined/missing values check amount based on modal/",product," month check.pdf",sep = ""),width = 6,height = 4)
  print(plt_month)
  dev.off()
  
  ### next check the missing values ditribution among states
  ### the data is from 2012-04-01 to 2017-12-01
  dat = Dat
  ms = read.xlsx("E:/Dropbox/jingwen_tasks/SCM/market_districts.xlsx",sheet=1)
  colnames(ms)[1:4] = c("market","dis1","dis2","state")
  ms$market = tolower(ms$market)
  ms$state = tolower(ms$state)
  ms$dis2 = tolower(ms$dis2)
  MS = ms
  ms = ms[,c(1,4)]
  dat = join(dat,ms,by = "market",type = "left")
  for (l in 1:nrow(dat)){
    if (!is.na(dat[l,"state"])){
      if (dat[l,"state"]=="india"){
        dat[l,"state"] = MS[MS$market==dat[l,"market"],3]
      }
      if (dat[l,"state"]%in%c("hagaribommanahalli","hubballi")){
        dat[l,"state"] = "karnataka"
      }
      if (dat[l,"state"]%in%c("bhildi","jagdish nagar")){
        dat[l,"state"] = "gujarat"
      }
      if (dat[l,"state"]%in%c("jadcherla","secunderabad")){
        dat[l,"state"] = "telangana"
      }
      if (dat[l,"state"]=="purani basti"){
        dat[l,"state"] = "rajasthan"
      }
      if (dat[l,"state"]%in%c("sampath nagar","ramanujapuram","puducherry")){
        dat[l,"state"] = "tamil nadu"
      }
      if (dat[l,"state"]=="risia bazaar"){
        dat[l,"state"] = "uttar pradesh"
      }
      if (dat[l,"state"]=="b block"){
        dat[l,"state"] = "haryana"
      }
    }
  }
  
  #plot the time trend of each state with the variable being the number of NAs per market in that state
  dat$missing_portion = dat$na/dat$modal
  timecheck = aggregate(dat$missing_portion,by = list(dat$date,dat$state),mean)
  colnames(timecheck) = c("date","state","missing_portion")
  timecheck$date = as.Date(timecheck$date)
  plt_time = ggplot(timecheck, aes(date, missing_portion, colour=state)) + 
    geom_line(lwd = 0.7,alpha = 0.75) + 
    scale_colour_brewer(palette = "Set1")+
    scale_x_date(date_breaks = "12 months", date_labels = "%Y-%m-%d")
  pdf(paste("E:/Dropbox/jingwen_tasks/SCM/vdsa chosen features weighetd wpi marketwise/no combined/missing values check amount based on modal/",product," time check in state.pdf",sep = ""),width = 8,height = 4)
  print(plt_time)
  dev.off()
  
  #plot the numbe of records with modal without amount and separate the bar by markets
  statecheck = aggregate(dat$na,by = list(dat$market,dat$state),sum)
  colnames(statecheck) = c("market","state","number_of_NA")
  statecheck = as.data.frame(statecheck)
  statecheck = statecheck[statecheck$number_of_NA!=0,]
  qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
  col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
  plt_state = ggplot(statecheck, aes(x = state, y = number_of_NA, fill = market, label = number_of_NA)) +
    geom_bar(stat = "identity") +
    geom_text(size = 3, position = position_stack(vjust = 0.5))+
    scale_fill_manual(values = sample(col_vector, nrow(statecheck),replace = T))
  pdf(paste("E:/Dropbox/jingwen_tasks/SCM/vdsa chosen features weighetd wpi marketwise/no combined/missing values check amount based on modal/",product," state check in number.pdf",sep = ""),width = 16,height = 8)
  print(plt_state)
  dev.off()
  
  
  #draw a barplot based on missing values in a state per market
  permarket = aggregate(statecheck$number_of_NA,by = list(statecheck$state),mean)
  colnames(permarket) = c("state","number_of_NA_per_market")
  plt_state_per_market = ggplot(data = permarket,aes(x=reorder(state,-number_of_NA_per_market),y=number_of_NA_per_market))+
    geom_bar(stat = "identity")+
    ylim(c(0,(max(permarket$number_of_NA_per_market)+5)))+
    geom_text(size = 3,aes(x=permarket$state,y=permarket$number_of_NA_per_market,label=round(permarket$number_of_NA_per_market,3)),vjust = -1)+
    theme(axis.text=element_text(size=6))+
    xlab("state")
  pdf(paste("E:/Dropbox/jingwen_tasks/SCM/vdsa chosen features weighetd wpi marketwise/no combined/missing values check amount based on modal/",product," state check per market.pdf",sep = ""),width = 6,height = 4)
  print(plt_state_per_market)
  dev.off()
  
  #fraction of each state 
  #fraction means the number of records with modal prices without amount divided by records with modal prices
  #for each market and average over states
  dat$missing_portion = dat$na/dat$modal
  statecheck = aggregate(dat$missing_portion,by = list(dat$state),mean)
  colnames(statecheck) = c("state","missing_portion")
    plt_state_fraction = ggplot(data = statecheck,aes(x=reorder(state,-missing_portion),y=missing_portion))+
    geom_bar(stat = "identity")+
    ylim(c(0,(max(statecheck$missing_portion)+0.1)))+
    geom_text(size = 3,aes(x=statecheck$state,y=statecheck$missing_portion,label=round(statecheck$missing_portion,3)),vjust = -1)+
    theme(axis.text=element_text(size=6))+
    xlab("state")
  pdf(paste("E:/Dropbox/jingwen_tasks/SCM/vdsa chosen features weighetd wpi marketwise/no combined/missing values check amount based on modal/",product," state check in fraction.pdf",sep = ""),width = 6,height = 4)
  print(plt_state_fraction)
  dev.off()
  
  #check with whether treated or not
  dat$treat = dat$market%in%intervented
  treatcheck = aggregate(dat$missing_portion,by = list(dat$treat),mean)
  colnames(treatcheck) = c("treat","missing_portion")
  plt_treat_fraction = ggplot(data = treatcheck,aes(x=reorder(treat,-missing_portion),y=missing_portion))+
    geom_bar(stat = "identity",width =0.3)+
    ylim(c(0,(max(treatcheck$missing_portion)+0.1)))+
    geom_text(size = 3,aes(x=treatcheck$treat,y=treatcheck$missing_portion,label=round(treatcheck$missing_portion,3)),vjust = -1)+
    theme(axis.text=element_text(size=10))+
    xlab("treat")
  pdf(paste("E:/Dropbox/jingwen_tasks/SCM/vdsa chosen features weighetd wpi marketwise/no combined/missing values check amount based on modal/",product," treat check in fraction.pdf",sep = ""),width = 6,height = 4)
  print(plt_treat_fraction)
  dev.off()
  
  #check for every month

  datecheck = aggregate(dat$missing_portion,by = list(dat$date),mean)
  colnames(datecheck) = c("date","missing_portion")
  plt_date_fraction = ggplot(data = datecheck,aes(x=date,y=missing_portion))+
    geom_bar(stat = "identity",width = 20)+
    ylim(c(0,(max(datecheck$missing_portion)+0.1)))+
    theme(axis.text=element_text(size=10))+
    xlab("month")
  pdf(paste("E:/Dropbox/jingwen_tasks/SCM/vdsa chosen features weighetd wpi marketwise/no combined/missing values check amount based on modal/",product," date check in fraction.pdf",sep = ""),width = 6,height = 4)
  print(plt_date_fraction)
  dev.off()
  
  
  
  ###check the trend of quantity of states 
  dat = read.xlsx(paste("E:/Dropbox/jingwen_tasks/SCM/vdsa chosen features weighetd wpi marketwise/no combined/sub2/",product,"/12-17 mon-qua marketwise daily/seventh data.xlsx",sep = ""),sheet=1)
  colnames(dat)[1] = "date"
  dat$date = as.Date(dat$date-25569,origin = "1970-01-01")
  ms = read.xlsx("E:/Dropbox/jingwen_tasks/SCM/market_districts.xlsx",sheet=1)
  colnames(ms)[1:4] = c("market","dis1","dis2","state")
  ms$market = tolower(ms$market)
  ms$state = tolower(ms$state)
  ms$dis2 = tolower(ms$dis2)
  MS = ms
  ms = ms[,c(1,4)]
  dat = join(dat,ms,by = "market",type = "left")
  dat = na.omit(dat)
  for (l in 1:nrow(dat)){
    if (dat[l,"state"]=="india"){
      dat[l,"state"] = MS[MS$market==dat[l,"market"],3]
    }
    if (dat[l,"state"]%in%c("hagaribommanahalli","hubballi")){
      dat[l,"state"] = "karnataka"
    }
    if (dat[l,"state"]%in%c("bhildi","jagdish nagar")){
      dat[l,"state"] = "gujarat"
    }
    if (dat[l,"state"]=="jadcherla"){
      dat[l,"state"] = "telangana"
    }
    if (dat[l,"state"]=="purani basti"){
      dat[l,"state"] = "rajasthan"
    }
    if (dat[l,"state"]%in%c("sampath nagar","ramanujapuram")){
      dat[l,"state"] = "tamil nadu"
    }
    if (dat[l,"state"]=="risia bazaar"){
      dat[l,"state"] = "uttar pradesh"
    }
    
  }
  #plot for krnataka and other states with two levels
  quan = aggregate(dat$amount,by = list(dat$state,dat$date),sum,na.rm = T)
  colnames(quan) = c("state","month","amount")
  quan$state = c("other_states","karnataka")[as.numeric(quan$state=="karnataka")+1]
  quan = aggregate(quan$amount,by = list(quan$state,quan$month),mean,na.rm = T)
  colnames(quan) = c("state","month","amount")
  mp = ggplot(data = quan,aes(x = month,y = amount,color = state))+
    geom_line(aes(group = state))+
    scale_color_brewer(type = 'seq', palette = "Set1")
  pdf(paste(product,"quantity trend.pdf"),width = 8,height = 4)
  print(mp)
  dev.off()
  
  #plot state swise
  quan = aggregate(dat$amount,by = list(dat$state,dat$date),sum,na.rm = T)
  colnames(quan) = c("state","month","amount")
  mp = ggplot(data = quan,aes(x = month,y = amount,color = state))+
    geom_line(aes(group = state))+
    scale_color_brewer(type = 'seq', palette = "Set1")
  pdf(paste(product,"quantity trend statewise.pdf"),width = 8,height = 4)
  print(mp)
  dev.off()
}




