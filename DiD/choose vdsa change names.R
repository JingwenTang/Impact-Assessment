rm(list = ls())
library(openxlsx)
lost = read.xlsx("/Users/tangjingwen/Dropbox/jingwen_tasks/organized/data/lost districts from vdsa.xlsx",sheet = 1,colNames = F)$X1
#on = read.xlsx("/Users/tangjingwen/Dropbox/jingwen_tasks/SCM/district names change attched to vdsa.xlsx",sheet = "all change list attched to vdsa")
on = read.xlsx("/Users/tangjingwen/Dropbox/jingwen_tasks/SCM/vdsa change names after remove internames.xlsx",sheet = 1)


wb = loadWorkbook("/Users/tangjingwen/Dropbox/jingwen_tasks/SCM/district names change attched to vdsa.xlsx")
addWorksheet(wb,"new names in organized form")
writeData(wb,sheet = "new names in organized form",on)

md = read.xlsx("/Users/tangjingwen/Dropbox/jingwen_tasks/SCM/market_districts.xlsx",sheet = 1)
colnames(md)[1:3] = c("market","district","subdis")
md$district = tolower(md$district)
md$subdis = tolower(md$subdis)

a = on[on$vdsa%in%md$district,"new"]
a[a%in%md$district]
on$vdsa[on$vdsa%in%on$new]
useold = on[on$vdsa%in%md$district,]
cannotuseold = on[!on$vdsa%in%md$district,]
newnames = cannotuseold$new[(cannotuseold$new)%in%lost]
thenusenew = c()
for (k in 1:length(newnames)){
  for (l in 1:nrow(cannotuseold)){
    if (newnames[k]%in%cannotuseold[l,]) thenusenew = rbind(thenusenew,cannotuseold[l,])
  }
}


addWorksheet(wb,"old names in market_districts")
writeData(wb,sheet = "old names in market_districts",useold)
addWorksheet(wb,"old not but new names in md")
writeData(wb,sheet = "old not but new names in md",thenusenew)

saveWorkbook(wb,"/Users/tangjingwen/Dropbox/jingwen_tasks/SCM/district names change attched to vdsa.xlsx",overwrite = T)

change = read.xlsx("/Users/tangjingwen/Dropbox/jingwen_tasks/SCM/vdsa to organized districts.xlsx",sheet = 1,colNames = F)
change[change$X2%in%usenew$vdsa,]
usenew[usenew$vdsa%in%change$X2,]
thenuseold[thenuseold$vdsa%in%change$X2,]
a = change[!change$X1%in%md$district,]
c1 = change[1:161,]
c2 = change[162:nrow(change),]
c1[c1$X2%in%c2$X2,]

#######operations on market_districts
md = md[!md$district == "zero_results",]
usesubdis = md[md$district=="none returned",]
usesubdis[,"district"] = usesubdis[,"subdis"]
md = md[!md$district=="none returned",]
md = rbind(md,usesubdis)
colnames(md)[4:5] = c("state","country")
write.xlsx(md,"/Users/tangjingwen/Dropbox/jingwen_tasks/SCM/market_districts.xlsx")



######deal with 
on = read.xlsx("/Users/tangjingwen/Dropbox/jingwen_tasks/SCM/district names change attched to vdsa.xlsx",sheet = "all change list attched to vdsa")
l = 1
while (l <=nrow(on)){
  temp = on[l,"new"]
  while(temp%in%on$vdsa){
    ind = which(on$vdsa %in% temp)[1]
    temp = on[ind,"new"]
    on = on[-ind,]
  }
  on[l,"new"] = temp
  l = l+1
}
write.xlsx(on,"/Users/tangjingwen/Dropbox/jingwen_tasks/SCM/vdsa change names after remove internames.xlsx")
