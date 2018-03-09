library(ggplot2)
library(ggfortify)
library(survival)
library(survminer)
options(scipen = 999) #999 is the number of decimal places you want to display
cost <- read.csv("CIDS total cost.csv")
live<-read.csv("CIDS survival.csv")
m <- as.matrix(cost[,ncol(cost):1])
quantile(cost$AGE,  probs = c(25,75)/100)
young<-m[m[, "AGE"] <58,]
mid<-m[m[, "AGE"] >=58 & m[, "AGE"] <70,]
old<-m[m[, "AGE"] >=70,]

y<-as.data.frame(young)
md<-as.data.frame(mid)
od<-as.data.frame(old)

y1 <- ggplot() + geom_density(aes(x = Total),fill='red',linetype="dashed",colour="red", data=y)+xlim(0,300000)+ labs(x ='Total cost in group (age<57)')+theme(axis.text.y=element_blank())
m1 <- ggplot() + geom_density(aes(x = Total),fill='blue',linetype="solid",colour="blue", data=md)+xlim(0,300000)+ labs(x ='Total cost in group (57<age<=70)')+theme(axis.text.y=element_blank())
o1 <- ggplot() + geom_density(aes(x = Total),fill='green',linetype="longdash",colour="green", data=md)+xlim(0,300000)+ labs(x ='Total cost in group (age>70)')+theme(axis.text.y=element_blank())

m <- as.matrix(live[,ncol(live):1])
young<-m[m[, "AGE"] <58,]
mid<-m[m[, "AGE"] >=58 & m[, "AGE"] <70,]
old<-m[m[, "AGE"] >=70,]

y<-as.data.frame(young)
md<-as.data.frame(mid)
od<-as.data.frame(old)

fit<- survfit(Surv(FUDAYS, DEAD) ~ SEX+TREAT, data = live)
p0<-ggsurvplot(fit,legend.title = "Sex&Treat", legend.labs = c("Male_Defi", "Male_Amio","Female_Defi", "Female_Amio"),linetype=c("solid","dashed","dotted","longdash"))+ylim(c(0.5, 1.0))+labs(x ='Overall Survival(whole dataset)')

fit1<- survfit(Surv(FUDAYS, DEAD) ~ SEX+TREAT, data = y)
p1<-ggsurvplot(fit1,legend.title = "Sex&Treat", legend.labs = c("Male_Defi", "Male_Amio","Female_Defi", "Female_Amio"),linetype=c("solid","dashed","dotted","longdash"))+ylim(c(0.5, 1.0))+labs(x ='Total cost in group (age<57)')

fit2<- survfit(Surv(FUDAYS, DEAD) ~ SEX+TREAT, data = md)
p2<-ggsurvplot(fit2,legend.title = "Sex&Treat", legend.labs = c("Male_Defi", "Male_Amio","Female_Defi", "Female_Amio"),linetype=c("solid","dashed","dotted","longdash"))+ylim(c(0.5, 1.0))+labs(x ='Total cost in group (57<age<=70)')

fit3<- survfit(Surv(FUDAYS, DEAD) ~ SEX+TREAT, data = od)
p3<-ggsurvplot(fit3,legend.title = "Sex&Treat", legend.labs = c("Male_Defi", "Male_Amio","Female_Defi", "Female_Amio"),linetype=c("solid","dashed","dotted","longdash"))+ylim(c(0.5, 1.0))+labs(x ='Total cost in group (age>70)')

splots <- list()
splots[[1]] <- p0
splots[[2]] <- p1
splots[[3]] <- p2
splots[[4]] <- p3
arrange_ggsurvplots(splots, print = TRUE,
                    ncol = 2, nrow = 2, risk.table.height = 0.4)
