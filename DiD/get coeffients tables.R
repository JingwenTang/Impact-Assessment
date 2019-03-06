rm(list = ls())

Product = c("Groundnut","Jowar(Sorghum)","Maize","Sunflower","Arhar (Tur-Red Gram)","Cotton","Green Gram (Moong)","Arecanut(Betelnut-Supari)","Bengal Gram(Gram)","Black Gram (Urd Beans)","Dry Chillies","Copra","Kulthi(Horse Gram)","Paddy(Dhan)")
Re = c()
for (p in c(14,1,3,7,6,5)){
  product = Product[p]
  re = c()
  dat = read.xlsx("/Users/tangjingwen/Dropbox/jingwen_tasks/organized/result/DiD/not add vdsa/10-17 week all inter coefficients.xlsx",sheet = product)
  dat$`Pr(>|t|)` = as.numeric(dat$`Pr(>|t|)`)
  dat$Estimate = as.numeric(dat$Estimate)
  dat$Estimate = round(dat$Estimate,3)
  dat$Std.Error = as.numeric(dat$Std.Error)
  dat$Std.Error = round(dat$Std.Error,3)
  for (l in 1:(nrow(dat)-2)){
    if ((dat[l,"Pr(>|t|)"]>0)&(dat[l,"Pr(>|t|)"]<0.01)) dat[l,"Estimate"] = paste(dat[l,"Estimate"],"***")
    else if ((dat[l,"Pr(>|t|)"]>=0.01)&(dat[l,"Pr(>|t|)"]<0.05)) dat[l,"Estimate"] = paste(dat[l,"Estimate"],"**")
    else if ((dat[l,"Pr(>|t|)"]>=0.05)&(dat[l,"Pr(>|t|)"]<0.1)) dat[l,"Estimate"] = paste(dat[l,"Estimate"],"*")
  }
  for (l in 1:nrow(dat)){
    dat[l,"Std.Error"] = paste("(",dat[l,"Std.Error"],")",sep = "")
  }
  r1 = dat[dat$trial == "inter1",][1,]
  re = rbind(r1$Estimate,r1$Std.Error)
  r1 = dat[dat$trial == "inter1",][3,]
  re = rbind(re,r1$Estimate)
  re = rbind(re,r1$Std.Error)
  r1 = dat[dat$trial == "inter1",][2,]
  re = rbind(re,r1$Estimate)
  re = rbind(re,r1$Std.Error)
  r1 = dat[dat$trial == "placebo",]
  re = rbind(re,r1$Estimate)
  re = rbind(re,r1$Std.Error)
  r1 = dat[dat$trial == "parallel",]
  re = rbind(re,r1$Estimate)
  re = rbind(re,r1$Std.Error)
  
  
  Re = cbind(Re,re)
}

write.xlsx(Re,paste("/Users/tangjingwen/Dropbox/jingwen_tasks/organized/result/DiD/not add vdsa/","10-17 week all inter coefficients (table2).xlsx",sep = ""),overwrite = T)