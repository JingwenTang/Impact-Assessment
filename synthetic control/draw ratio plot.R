rm(list = ls())
library(ggplot2)
library(openxlsx)
treated = read.xlsx("E:/Dropbox/jingwen_tasks/SCM/vdsa chosen features weighetd wpi marketwise/no combined/sub2/Groundnut/result tables1.xlsx",sheet = "gaps_analysis")
untreated = read.xlsx("E:/Dropbox/jingwen_tasks/SCM/vdsa chosen features weighetd wpi marketwise/placebo/sub2/Groundnut/result tables.xlsx",sheet = "gaps_analysis")
colnames(treated)[1:2] = c("market","model")
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


p = ggplot(dat, aes(ratio, reorder(market,ratio))) +
  geom_point(aes(colour = treat))

pdf("E:/Dropbox/jingwen_tasks/SCM/vdsa chosen features weighetd wpi marketwise/placebo/sub2/Groundnut/12-17 mon-qua/30.pdf", width=8, height=4)
p
dev.off()
