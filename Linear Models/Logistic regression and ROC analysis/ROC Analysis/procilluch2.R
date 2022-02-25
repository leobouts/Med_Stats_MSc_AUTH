library(pROC)

par(mfrow=c(1,2))

brucon<-c(59, 66, 45, 62, 51, 50, 49, 58, 53, 42, 50, 47, 51, 62, 48)
brucas<-c(72, 70, 69, 82, 68, 59, 76, 61, 59, 73, 49, 77)

roc(controls=brucon,cases=brucas, plot=T, ci=T)


library(ROCR)

pred <- prediction(c(brucon,brucas), c(rep(0,15),rep(1,12)))

perf <- performance(pred,"tpr","fpr")

plot(perf,colorize=TRUE)

areaest<-performance(pred,"auc")

areaest@y.values
