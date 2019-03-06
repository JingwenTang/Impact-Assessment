dat = c(10,1,1,1,0,0,0,1,
        11,2,1,1,0,0,0,1,
        13,2,1,1,0,0,0,1,
        15,3,1,0,1,0,0,1,
        18,4,1,0,1,0,0,1,
        19,1,0,0,0,1,0,1,
        20,3,0,0,0,1,0,1,
        18,4,0,0,0,1,0,1,
        21,2,0,0,0,0,1,1,
        24,1,0,0,0,0,1,1,
        23,2,0,0,0,0,0,1,
        24,3,0,0,0,0,0,1
        )
dat = c(10,1,1,1,0,0,0,1,
        11,2,1,0,1,0,0,1,
        13,2,0,1,0,1,0,1,
        15,3,0,0,1,0,1,1,
        18,4,1,1,0,0,0,1,
        19,1,1,0,1,0,0,1,
        20,3,0,0,0,1,0,1,
        18,4,0,0,0,0,1,1,
        21,2,1,1,0,0,1,1,
        24,1,1,0,1,0,1,1,
        23,2,0,0,0,1,0,1,
        24,3,0,0,0,0,1,1
)
dat = matrix(dat,12,8,byrow = T)
dat = cbind(dat,c("a","a","a","a","b","b","b","b","c","c","c","c"))
colnames(dat) = c("y","x","lead","lead_1","lead_2","lag_1","lag_2","lead_3","market")
dat = as.data.frame(dat)
for (i in 1:8){
  dat[,i] = as.numeric(dat[,i])
}
fit1 = lm(log(y)~x+lead+lag_1+lag_2+market+0,data = dat)
fit2 = lm(log(y)~x+lead_1+lead_2+lag_1+lag_2+market+0,data = dat)
fit3 = lm(log(y)~x+lead+lag_1+lag_2+market+lead_3+0,data = dat)
fit4 = lm(log(y)~x+lead_1+lead_2+lag_1+lag_2+market+lead_3+0,data = dat)


clx <-function(fm, dfcw, cluster){
  library(sandwich)
  library(lmtest)
  M <- length(unique(cluster))
  N <- length(cluster)
  dfc <- (M/(M-1))*((N-1)/(N-fm$rank))
  u <- apply(estfun(fm),2,function(x) tapply(x, cluster, sum))
  vcovCL <- dfc*sandwich(fm, meat=crossprod(u)/N)*dfcw
  coeftest(fm, vcovCL) }
crse1 = clx(fit1,1, dat$market)
crse2 = clx(fit2,1, dat$market)
crse3 = clx(fit3,1, dat$market)
crse4 = clx(fit4,1, dat$market)
crse1
crse2
crse3
crse4
