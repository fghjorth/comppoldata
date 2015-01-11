setwd("~/GitHub/comppoldata")
ms<-read.csv(file="msdata.csv")
erd<-read.csv(file="erd.csv")


#country
class(erd$v001e)
erd$cntry<-as.factor(ifelse(erd$v001e %in% c(3,5,6,13,16,17),erd$v001e,NA))
levels(erd$cntry)<-c("Denmark","France","Germany","Norway","Sweden","UK")

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

#regulate line width
erd$ltp<-as.character(ifelse(erd$cntry %in% c("Denmark","Norway","Sweden"),"d","s"))

require(ggplot2)
pdf(file="effnop2.pdf",width=10,height=4)
ggplot(subset(erd,!is.na(cntry)),aes(x=fulldate,y=effnop,group=cntry)) +
  geom_line(aes(color=cntry,linetype=ltp)) +
  theme_bw() +
  scale_colour_brewer(palette="Dark2")
dev.off()


pdf(file="minority2.pdf",width=10,height=4)
ggplot(subset(erd,!is.na(cntry)),aes(x=fulldate,fill=minority)) +
  geom_bar() +
  facet_grid(cntry~.) +
  theme_bw() +
  scale_colour_brewer(palette="Dark2")
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


names(erd)
class(erd$v205e)
erd$v205e[erd$cntry=="Denmark" & !is.na(erd$v205e)]

sdvot<-data.frame(cntry=c(erd$cntry[which(erd$cntry=="Denmark")],erd$cntry[which(erd$cntry=="Norway")],erd$cntry[which(erd$cntry=="Sweden")]),sdseats=c(erd$v205e[which(erd$cntry=="Denmark")],erd$v202e[which(erd$cntry=="Norway")],erd$v201e[which(erd$cntry=="Sweden")]),chambersize=c(erd$v519e[which(erd$cntry=="Denmark")],erd$v519e[which(erd$cntry=="Norway")],erd$v519e[which(erd$cntry=="Sweden")]),fulldate=c(erd$fulldate[which(erd$cntry=="Denmark")],erd$fulldate[which(erd$cntry=="Norway")],erd$fulldate[which(erd$cntry=="Sweden")]))

sdvot$cntry<-factor(sdvot$cntry,labels=c("Denmark","Norway","Sweden"))
sdvot$seatshare<-sdvot$sdseats/sdvot$chambersize

pdf(file="sdshare.pdf",width=10,height=4)
ggplot(sdvot,aes(x=fulldate,seatshare)) +
  geom_smooth(aes(colour=cntry)) +
  geom_point(aes(colour=cntry)) +
  scale_colour_brewer(palette="Dark2") +
  xlab("") +
  ylab("Social Democrats seat share") +
  theme_bw()
dev.off()
