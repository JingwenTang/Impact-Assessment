rm(list = ls())
wb = createWorkbook()
Product = c("Groundnut","Jowar(Sorghum)","Maize","Sunflower","Arhar (Tur-Red Gram)","Cotton","Green Gram (Moong)","Arecanut(Betelnut-Supari)","Bengal Gram(Gram)","Black Gram (Urd Beans)","Dry Chillies","Copra","Kulthi(Horse Gram)","Paddy(Dhan)")
for (p in c(1,3,5,6,7,14)){
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
r1 = dat[dat$trial == "inter1",]
re = rbind(r1$Estimate,r1$Std.Error)
r1 = dat[dat$trial == "log(amount)",]
re = rbind(re,r1$Estimate)
re = rbind(re,r1$Std.Error)
r1 = dat[dat$trial == "yield",]
re = rbind(re,r1$Estimate)
re = rbind(re,r1$Std.Error)
r1 = dat[dat$trial == "production",]
re = rbind(re,r1$Estimate)
re = rbind(re,r1$Std.Error)
r1 = dat[dat$trial == "Per.Capita.GSDP",]
re = rbind(re,r1$Estimate)
re = rbind(re,r1$Std.Error)
r1 = dat[dat$trial == "rainfall",]
re = rbind(re,r1$Estimate)
re = rbind(re,r1$Std.Error)
r1 = dat[dat$trial == "no.observations",]
re = rbind(re,r1$Estimate)
r1 = dat[dat$trial == "R-squared",]
re = rbind(re,r1$Estimate)

addWorksheet(wb,product)

writeData(wb,sheet = product,re)
}
saveWorkbook(wb,paste("/Users/tangjingwen/Dropbox/jingwen_tasks/organized/result/DiD/not add vdsa/","10-17 week all inter coefficients (table).xlsx",sep = ""),overwrite = T)