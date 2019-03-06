rm(list = ls())
library(readr)
library(openxlsx)
library(plyr)
library(dplyr)
library(ggplot2)
library(reshape2)
library(ggrepel)
fMode = c("full","sub","sub2")
f = list()
f[[1]]= c("annual","total_ar","lit_t","male_saw","annmai","lit_ru","ptmrkt","lroad","agrl_t")
f[[2]] = c("annual","total_ar","lit_t","male_saw")
f[[3]] = c("annual","total_ar","lit_t","agrl_t","ptmrkt")
for (size in 3:3){
  fmode = fMode[size]
  f = f[[size]]
  dir.create(paste("E:/Dropbox/jingwen_tasks/SCM/vdsa chosen features weighetd wpi marketwise/",fmode,sep = ""))
  Product = c("Groundnut","Jowar(Sorghum)","Maize","Sunflower","Arhar (Tur-Red Gram)","Cotton","Green Gram (Moong)","Arecanut(Betelnut-Supari)","Bengal Gram(Gram)","Black Gram (Urd Beans)","Dry Chillies","Copra","Kulthi(Horse Gram)","Paddy(Dhan)")
  Wpiproduct = c("Groundnut Seed","Jowar","Maize","Sunflower","Arhar","Cotton Seed","Moong","Betelnut/Arecanut","Gram","All commodities","Chillies (Dry)","Copra (Coconut)","All commodities","All commodities")
  Model = c("week","month","quarter")
  Begin = c(2012,2012,2012,2012)
  Over = c(2017,2017,2017,2017)
  Br = c("week","month","quarter")
  early = as.Date(c("2012-04-01","2012-04-01","2012-04-01","2012-04-01"))
  late = as.Date(c("2018-01-01","2018-01-01","2018-01-01","2018-01-01"))
  Lagbr  = c("month","3 months","3 months","6 months")
  
  setpre = c(1,3,1,2)
  Features = list()
  Features[[1]] = c("gnut_tq",f)
  Features[[2]] = c("sorg_tq",f)
  Features[[3]] = c("maiz_tq",f)
  Features[[4]] = c("sunf_tq",f)
  for (j in 1:length(Product)){
    Features[[j]] = f
  }
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

  colnames(intervention) = c("number","code","market","date")

    
   for (p in c(1,3,14)){
  #for (p in 1:length(Product)){
      product = Product[p]
      dir.create(paste("E:/Dropbox/jingwen_tasks/SCM/vdsa chosen features weighetd wpi marketwise/placebo/",fmode,sep = ""))
      dir.create(paste("E:/Dropbox/jingwen_tasks/SCM/vdsa chosen features weighetd wpi marketwise/placebo/",fmode,"/",product,sep = ""))
      for (m in 2:2){
        temp = read.xlsx(paste("E:/Dropbox/jingwen_tasks/SCM/vdsa chosen features weighetd wpi marketwise/no combined/",fmode,"/",product,"/12-17 mon-qua/data_details1.xlsx",sep = ""),sheet="Market_info")
        temp = temp[-1,]
        nonin = c()
        for (l in 1:ncol(temp)){
          num = sum(!is.na(temp[,l]))-2
          nonin = c(nonin,temp[1:num,l])
        }
        nonin = unique(nonin)
        #write.xlsx(nonin,paste("E:/Dropbox/jingwen_tasks/SCM/vdsa chosen features weighetd wpi marketwise/placebo/",fmode,"/",product,"/treat.xlsx",sep = ""))
        inter = as.character(read.xlsx(paste("E:/Dropbox/jingwen_tasks/SCM/vdsa chosen features weighetd wpi marketwise/no combined/sub2/",product,"/treat.xlsx",sep = ""),colNames = F)$X1)
        intermar = data.frame(market=inter)
        interinfo = join(intermar,intervention,by = "market")[,c(1,4)]
        interinfo$date = as.Date(as.numeric(interinfo$date)-25569,origin = "1970-01-01")
        interinfo$date = cut(interinfo$date,br=Br[m])
        interinfo$date = as.Date(interinfo$date)
        amount = read.xlsx(paste("E:/Dropbox/jingwen_tasks/SCM/vdsa chosen features weighetd wpi marketwise/no combined/sub2/",product,"/13-17 month/original table.xlsx",sep = ""),sheet = 1)
        interinfo = join(interinfo,amount,by = "market",type = "left")
        dateinfo = aggregate(interinfo$amount,by = list(interinfo$date),sum,na.rm = T)
        colnames(dateinfo) = c("date","amount")
        dateinfo = dateinfo[order(dateinfo$date,decreasing = F),]
        for (l in 1:nrow(dateinfo)){
          dateinfo$portion[l] = sum(dateinfo$amount[1:l],na.rm = T)/sum(dateinfo$amount,na.rm = T)
        }
        
        intercount = table(interinfo$date)
        intercount = as.data.frame(intercount)
        colnames(intercount) = c("date","freq")
        intercount$date = as.Date(intercount$date)
        if (Model[m]=="week"){
          dat = read.xlsx(paste("E:/Dropbox/jingwen_tasks/SCM/vdsa chosen features weighetd wpi marketwise/no combined/sub2/",product,"/11-17 week common types/seventh data.xlsx",sep = ""),sheet = 1)
          #dat = read.xlsx(paste("E:/Dropbox/jingwen_tasks/DiD/full/",product,"/12-17 week/common type price features.xlsx",sep = ""),sheet = 1)
        }
        if (Model[m]=="month"){
          print(product)
          dat = read.xlsx(paste("E:/Dropbox/jingwen_tasks/SCM/vdsa chosen features weighetd wpi marketwise/no combined/sub2/",product,"/13-17 month/seventh data.xlsx",sep = ""),sheet = 1)
          
          #dat = read.xlsx(paste("E:/Dropbox/jingwen_tasks/SCM/vdsa chosen features weighetd wpi marketwise/no combined/sub2/",product,"/12-17 mon-qua outliers marketwise/common type price features.xlsx",sep = ""),sheet=1)
        }
        if (Model[m]=="quarter"){
          print(product)
          dat = read.xlsx(paste("E:/Dropbox/jingwen_tasks/SCM/vdsa chosen features weighetd wpi marketwise/no combined/sub2/",product,"/12-17 qua-qua/common type price features.xlsx",sep = ""),sheet = 1)
        }
        datin = dat[dat$market%in%inter,]
        datno = dat[dat$market%in%nonin,]
        intdat = ddply(datin,.(month),summarise, modal = weighted.mean(modal,amount,na.rm = T))
        notdat = ddply(datno,.(month),summarise, modal = weighted.mean(modal,amount,na.rm = T))
        colnames(intdat)[1] = "date"
        colnames(notdat)[1] = "date"
        d = join(intdat,notdat,by = "date",type = "inner")
        colnames(d) = c("date","karnataka","other_states")
        d$date = as.Date(d$date-25569,origin = "1970-01-01")
        d = melt(d,id="date")
        colnames(d) = c("date","state","modal")
        setwd("E:/Dropbox/jingwen_tasks/SCM/vdsa chosen features weighetd wpi marketwise/no combined/parallel plots/marketwise outlier")
        library(grid)
        
        info = join(intercount,dateinfo,by="date",type = "left")

        pl=ggplot(d, aes(x=date, y=modal, group=state)) +
          ylab(paste(product," modal price",sep=""))+
          geom_line(aes(linetype=state))+
          scale_linetype_manual(values=c("solid", "longdash"))+
          geom_vline(data = intercount, aes(xintercept = date), linetype="dotted",lwd = 0.6) +
          theme(plot.margin = unit(c(2,0,2,0), "lines"))
        
        #pl = pl+geom_text_repel(data = info,aes(x=date,y=(layer_scales(pl)$y$range$range[1]-400),label = paste(freq,"\n",portion)))
        
        for (l in 1:nrow(intercount)){
          pl = pl+annotation_custom(grob = textGrob(label = intercount$freq[l], gp = gpar(fontsize = 7,cex = 1.2,family = "bold")),xmin  = intercount$date[l]-6,xmax = intercount$date[l]-6,ymin=(layer_scales(pl)$y$range$range[1]*0.93),ymax = (layer_scales(pl)$y$range$range[1]*0.93))
        }
        for (l in 1:nrow(dateinfo)){
          pl = pl+annotation_custom(grob = textGrob(label = round(dateinfo$portion[l],2), gp = gpar(fontsize = 7,cex = 1,family = "bold")),xmin  = dateinfo$date[l]-6,xmax = dateinfo$date[l]-6,ymin=(layer_scales(pl)$y$range$range[2]*1.03),ymax = (layer_scales(pl)$y$range$range[2]*1.03))
        }
        pdf(paste(product,Model[m],"average plot 11-17 common types.pdf"),width = 10,height = 4)
        gt <- ggplot_gtable(ggplot_build(pl))
        gt$layout$clip[gt$layout$name == "panel"] <- "off"
        grid.draw(gt)
        
        dev.off()
        
      }
    }

  
}

d = rbind(datin,datno)
ms = read.xlsx("E:/Dropbox/jingwen_tasks/SCM/market_districts.xlsx",sheet=1)
colnames(ms)[1:4] = c("market","dis1","dis2","state")
ms$market = tolower(ms$market)
ms$state = tolower(ms$state)
ms$dis2 = tolower(ms$dis2)
MS = ms
ms = ms[,c(1,4)]

d = join(d,ms,by = "market",type = "left")
d = na.omit(d)
for (l in 1:nrow(d)){
  if (d[l,"state"]=="india"){
    d[l,"state"] = MS[MS$market==d[l,"market"],3]
  }
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
dat = ddply(d,.(state,month),summarise, modal =weighted.mean(modal,amount))
dat$month = as.Date(dat$month-25569,origin = "1970-01-01")
pdf("C:/Users/Administrator/Desktop/modal prices without gondal and Other type.pdf",width = 8,height = 4)
ggplot(data = dat,aes(x = month,y = modal,colour = state))+geom_line()+scale_color_brewer(palette = "Set1")
dev.off()

amountd = ddply(d,.(state),summarise, amount = sum(amount))
gd = d[d$state=="gujarat",]
gd$month = as.Date(gd$month-25569,origin = "1970-01-01")
pdf("C:/Users/Administrator/Desktop/amount of gujarat without gondal and Other type.pdf",width = 8,height = 4)
ggplot(data = gd,aes(x = month,y = amount,colour = market))+geom_line()+scale_color_brewer(palette = "Set1")
dev.off()

library(RColorBrewer)
library(wesanderson)
qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
gd = d[d$state=="karnataka",]
gd$month = as.Date(gd$month-25569,origin = "1970-01-01")
pdf("C:/Users/Administrator/Desktop/modal of karnataka.pdf",width = 8,height = 4)
ggplot(data = gd,aes(x = month,y = modal,colour = market))+geom_line()+
  scale_fill_manual(values = sample(col_vector, length(unique(d$market)),replace = T))
dev.off()
