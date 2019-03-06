library(openxlsx)
library(stringr)
dat = read.xlsx("/Users/tangjingwen/Dropbox/jingwen_tasks/SCM/district names change attched to vdsa.xlsx",sheet = "all change list attched to vdsa",colNames = F)
dat[!is.na(dat$X6),1:5] = dat[!is.na(dat$X6),2:6]
dat = dat[,1:5]
dat[is.na(dat$X4),4:5] = dat[is.na(dat$X4),1:2]
ind = (1:nrow(dat))[is.na(dat$X3)]
for (l in 1:length(ind)){
  dat[ind[l],1:3] = dat[(ind[l]-1),1:3]
}
dat = dat[,c(1,4)]
colnames(dat) = c("new","vdsa")
dat$new = tolower(dat$new)
dat$vdsa = tolower(dat$vdsa)

######DO NOT RUN!!
#######PART 1 to get the new names unified with the district names form market_district file
dat2 = read.xlsx("/Users/tangjingwen/Dropbox/jingwen_tasks/SCM/market_districts.xlsx",sheet = 1)[,1:3]
colnames(dat2) = c("market","district","district1")
dat2 = na.omit(dat2)

dat2$market = tolower(dat2$market)
dat2$district = tolower(dat2$district)
dat2$district1 = tolower(dat2$district1)
basic = sort(unique(dat2$district))

production = sort(unique(dat$vdsa))
production = str_trim(production)
namestab = c()
for (i in 1:length(basic)){
  production1 = c()
  word = basic[i]
  cand = c()
  if((nchar(word)-2)>0){
    for (l in 1:length(production)){
      if (substr(word,1,1) == substr(production[l],1,1)) production1 = c(production1,production[l])
    }
    for (k in 1:(nchar(word)-2)){
      subword = substr(word,k,(k+2))
      subcand = grep(subword,production1,fixed = T,value = T)
      cand = c(cand,subcand)
      
    }
    cand = cand[!duplicated(cand)]
    cand = cand[!cand%in%basic]
    cand = c(word,cand)
    namestab = qpcR:::rbind.na(namestab,cand)
  }
}
namestab = namestab[!is.na(namestab[,2]),]
write.xlsx(namestab,"/Users/tangjingwen/Dropbox/jingwen_tasks/SCM/vdsa vdsa to organized districts not useful.xlsx")
########

########PART 2 to change the vdsa new names according to the results above
temp = read.xlsx("/Users/tangjingwen/Dropbox/jingwen_tasks/SCM/vdsa new to organized districts.xlsx",sheet = 1,colNames = F)
dat$new = str_trim(dat$new)
dat$vdsa = str_trim(dat$vdsa)
for (l in 1:nrow(temp)){
  dat$new[dat$new%in%temp[l,-1]] = temp[l,1]
}
write.xlsx(dat,"/Users/tangjingwen/Dropbox/jingwen_tasks/SCM/vdsa to organized districts 2.xlsx")


##
dat = read.xlsx("/Users/tangjingwen/Dropbox/jingwen_tasks/SCM/district names change attched to vdsa.xlsx",sheet = "change in organized form")
temp = read.xlsx("/Users/tangjingwen/Dropbox/jingwen_tasks/SCM/district names change attched to vdsa.xlsx",sheet = "vdsa vdsa to organized district",colNames = F)
for (l in 1:nrow(temp)){
  dat$vdsa[dat$vdsa%in%temp[l,-1]] = temp[l,1]
}
write.xlsx(dat,"/Users/tangjingwen/Dropbox/jingwen_tasks/SCM/temp.xlsx")
