setwd("~/GitHub/comppoldata")
ms<-read.csv(file="msdata.csv")
erd<-read.csv(file="erd.csv")


#country
class(erd$v001e)
erd$cntry<-as.factor(erd$v001e)
levels(erd$cntry)<-c("Denmark","France","Germany","UK")

#eff no of parties
summary(erd$v309e)
erd$effnop<-erd$v309e
table(erd$effnop)

#date
table(erd$v004e)
#erd$date0<-paste(substr(erd$v004x,5,6),substr(erd$v004x,3,4),as.numeric(substr(erd$v004x,1,2))+1900,sep="-")
require(lubridate)
erd$fulldate<-ymd(erd$v004e)
table(erd$fulldate)

#unempchange
erd$unempchange<-ifelse(erd$v705e<100,erd$v705e,NA)-ifelse(erd$v703e<100,erd$v703e,NA)

#minority?
table(erd$v329e)
erd$minority<-factor(ifelse(erd$v329e==1,1,0))

#govt duration
erd$reldur<-ifelse(erd$v603e<2,erd$v603e,NA)

dkukfrde<-c(3,5,6,17)

erd0<-erd
erd<-erd0[erd0$v001e %in% dkukfrde,]



require(ggplot2)
pdf(file="effnop.pdf",width=10,height=4)
ggplot(erd,aes(x=fulldate,y=effnop,group=cntry)) +
  geom_line(aes(color=cntry)) +
  theme_bw()
dev.off()


pdf(file="minority.pdf",width=10,height=4)
ggplot(erd,aes(x=fulldate,fill=minority)) +
  geom_bar() +
  facet_grid(cntry~.) +
  theme_bw()
dev.off()

ggplot(erd,aes(x=fulldate,y=v407e,group=cntry)) +
  geom_line(aes(color=cntry)) +
  theme_bw()


pdf(file="duration.pdf",width=8,height=4)
ggplot(erd,aes(x=fulldate,y=reldur)) +
  geom_bar(stat="identity",color="black") +
  facet_grid(cntry~.) +
  theme_bw()
dev.off()

#View(erd[erd$cntry=="Denmark",120:206])

pdf(file="unempdur.pdf",width=8,height=4)
ggplot(erd0,aes(x=unempchange,y=reldur)) +
  geom_jitter(alpha=.5) +
  geom_smooth(method="lm") +
  theme_bw()
dev.off()

summary(lm(reldur~unempchange,data=erd0))

?geom_bar

require(stargazer)
stargazer(durmodel,style="apsr",align=T)
?stargazer

pdf(file="cabsize.pdf",width=8,height=4)
ggplot(erd0,aes(x=erd0$v320e,y=erd0$v330e)) +
  geom_smooth(method="loess") +
  geom_jitter(alpha=.6) +
  xlab("No. of cabinet parties") +
  ylab("No. of ministers") +
  theme_bw()
dev.off()

plot(erd0$v320e,erd0$v330e)

summary(cabsizemodel<-lm(v330e~v320e,data=erd0))
stargazer(cabsizemodel,style="apsr",align=T)

table(erd$v600e)

ggplot(erd0,aes(x=fulldate,y=v600e,color=cntry)) +
  geom_point() +
  geom_smooth(method="lowess") +
  theme_bw()



