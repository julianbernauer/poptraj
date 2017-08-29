#Replication code for the paper "A Fourth Wave of Populism? Trajectories of Populist Parties in Europe"
#To be presented at the 2017 ECPR General Conference, Oslo
#Julian Bernauer, MZES 
#29 August 2017

setwd("U:/Forschung MZES/Trajectories")

library(foreign)
library(R2jags)
library(rjags)
library(lme4)
library(arm)

traj <- read.csv("trajectories_national_2000_2017_2.csv",header=TRUE,sep=";")

#Time variable - years since 2000 
traj$time <- as.numeric(as.factor(traj$election))

#Country
traj$cntry <- as.numeric(as.factor(traj$country))

#Parties
traj$prty <- as.numeric(as.factor(paste(traj$country,traj$pname)))

#Elections
traj$elec <- as.numeric(as.factor(paste(traj$country,traj$election)))

#Sort: country, party, election     
traj <- traj[order(traj$cntry,traj$prty,traj$elec),]

attach(traj)

#log of electoral result 
lpv <- log(pvotes+1)

###election level 
#immigration at the election level - sorted by country and election
immidata <- read.csv("immi.csv",header=TRUE,sep=";")

#per 1000 inhabitants 
impk <- immidata$immipop*1000

#Shock as alternative specification 
immidata$crisis <- 0 
immidata$crisis[immidata$election==2015] <- 1
#2016 no election
immidata$crisis[immidata$election==2017] <- .5

shock <- immidata$crisis 

cntryje <- as.numeric(as.factor(immidata$country))

####party-level 
trajp <-aggregate(traj, by=list(traj$prty),FUN=mean, na.rm=TRUE)

cntryjp <- trajp$cntry


####country data 
cdata <- read.table(header=TRUE, text="
country  m      dir   costs  entry
DEU      18.7   0     1	     1
CHE      7.7    13.5  0	     1
ITA      22.5   6.5   1	     1
FRA      1.0    -0.5  1	     1
GBR      1.0    -1    0	     1
SWE      10.7   -0.5  1	     1
NED      150.0  -1.5  1	     1
BEL      13.6   0     1	     1
NOR      7.9    -2    1	     1
AUT      7.7    2     1	     1
DEN      13.5   1     1	     1
FIN      15.3   -1.5  1	     1
ESP      6.7    -0.5  0	     0
ISL      9.0    0.5   1	     0
POR      10.5   -1.5  0	     0
IRL      3.9    2.5   0	     0
")

#Order A-Z
cdata <- cdata[order(cdata$country),]

#average immigration, also ESP, ISL, POR, IRL from Eurostat
immic <- aggregate(immidata, by=list(immidata$country),FUN=mean, na.rm=TRUE)
immic
cdata$immi <- c(12.3,12.0,18.4,10.1,7.7,1.1,4.8,4.9,9.0,2.0,2.1,6.5,7.1,10.5,0.2,9.5)

#for outcome equation 
cdata2 <-  cdata[cdata$entry==1,]

m <- cdata$m
lm <- log(m)


####
#Simple logistic regression of entry 
reg_entry <- glm(cdata$entry ~ cdata$m + cdata$costs, family=binomial(link=probit))
summary(reg_entry) 
#Fig A3.1
coefplot(reg_entry,intercept=TRUE,varnames=c("Mean","M (log)","Public fund."),vertical=FALSE,main="",ylim=c(-.2,.6))
#reg_entry <- glm(cdata$entry ~ log(cdata$immi), family=binomial(link=probit))
reg_entry <- lm(cdata$entry ~ log(cdata$immi) + log(cdata$m) + cdata$costs)
summary(reg_entry) 
#Fig A3.2
coefplot(reg_entry,intercept=TRUE,varnames=c("Mean","Immi. (log)","M (log)","Public fund."),vertical=FALSE,main="",ylim=c(-.2,.6))

#Residuals 
reg_entry$residuals
cs <- c("AUT","BEL","CHE","DEN","DEU","ESP","FIN","FRA","GBR","IRL","ISL","ITA","NED","NOR","POR","SWE")
st <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
#Fig A3.4
coefplot(reg_entry$residuals,st,varnames=cs,vertical=FALSE,main="",ylim=c(-.6,.4))
#Fitted versus residual -> Fig A3.3
plot(reg_entry$residuals,reg_entry$fitted,type="n",xlab="Residuals of OLS of Entry",ylab="Fitted Values of OLS of Entry")
text(reg_entry$residuals,reg_entry$fitted,cs)


####
#Trend by party -> Fig 1
par(mfrow=c(3,6))
plot(traj$time[traj$pname=="BNP"],traj$pvotes[traj$pname=="BNP"], main="BNP", xlab="Time", ylab="Vote Share")
plot(traj$time[traj$pname=="DF"],traj$pvotes[traj$pname=="DF"], main="DF", xlab="Time", ylab="Vote Share")
plot(traj$time[traj$pname=="EDU"],traj$pvotes[traj$pname=="EDU"], main="EDU", xlab="Time", ylab="Vote Share")
plot(traj$time[traj$pname=="FN"],traj$pvotes[traj$pname=="FN"], main="FN", xlab="Time", ylab="Vote Share")
plot(traj$time[traj$pname=="FPO"],traj$pvotes[traj$pname=="FPO"], main="FPÖ", xlab="Time", ylab="Vote Share")
plot(traj$time[traj$pname=="FRP"],traj$pvotes[traj$pname=="FRP"], main="FRP", xlab="Time", ylab="Vote Share")
plot(traj$time[traj$pname=="LDT"],traj$pvotes[traj$pname=="LDT"], main="LDT", xlab="Time", ylab="Vote Share")
plot(traj$time[traj$pname=="LN"],traj$pvotes[traj$pname=="LN"], main="LN", xlab="Time", ylab="Vote Share")
#plot(traj$time[traj$pname=="MPF"],traj$pvotes[traj$pname=="MPF"], main="MPF", xlab="Time", ylab="Vote Share")
plot(traj$time[traj$pname=="NPD"],traj$pvotes[traj$pname=="NPD"], main="NPD", xlab="Time", ylab="Vote Share")
#plot(traj$time[traj$pname=="P"],traj$pvotes[traj$pname=="P"], main="P", xlab="Time", ylab="Vote Share")
plot(traj$time[traj$pname=="PS"],traj$pvotes[traj$pname=="PS"], main="PS", xlab="Time", ylab="Vote Share")
plot(traj$time[traj$pname=="PVV"],traj$pvotes[traj$pname=="PVV"], main="PVV", xlab="Time", ylab="Vote Share")
plot(traj$time[traj$pname=="SD"],traj$pvotes[traj$pname=="SD"], main="SD", xlab="Time", ylab="Vote Share")
plot(traj$time[traj$pname=="SGP"],traj$pvotes[traj$pname=="SGP"], main="SGP", xlab="Time", ylab="Vote Share")
plot(traj$time[traj$pname=="SVP"],traj$pvotes[traj$pname=="SVP"], main="SVP", xlab="Time", ylab="Vote Share")
plot(traj$time[traj$pname=="UKIP"],traj$pvotes[traj$pname=="UKIP"], main="UKIP", xlab="Time", ylab="Vote Share")
plot(traj$time[traj$pname=="VB"],traj$pvotes[traj$pname=="VB"], main="VB", xlab="Time", ylab="Vote Share")


#####
#Latent curves 

#model of populist vote shares 

#Number of cases at different levels 
N <- max(cntry)
NP <- max(prty) 
NE <- length(impk)
NPV <- length(pvotes)


#Three contexts, time and time squared, no covariates 
micro.model <- "model{

for(i in 1:NPV){
pvotes[i] ~ dnorm(mu.pv[i],tau.pv)
mu.pv[i] <- a_p[prty[i]] +  a_e[elec[i]] +  b_time[prty[i]]*time[i] +  b_time2[prty[i]]*time[i]*time[i]}

tau.pv <- pow(sigma.pv, -2)
sigma.pv ~ dunif(0, 50)

#random intercepts for parties
for(j in 1:NP){
a_p[j] ~ dnorm(mu0_p[j],tau.p0) 
mu0_p[j] <- a_c[cntryjp[j]]
}

#a.p ~ dnorm(0, .0001) 

tau.p0 <- pow(sigma.p0, -2)
sigma.p0 ~ dunif(0,100)

#random slopes for time per party
for(j in 1:NP){
b_time[j] ~ dnorm(mu1_p[j],tau.p1) 
mu1_p[j] <- a.t
}

tau.p1 <- pow(sigma.p1, -2)
sigma.p1 ~ dunif(0,100)

a.t ~ dnorm(0, .0001) 

#random slopes for time squared per party
for(j in 1:NP){
b_time2[j] ~ dnorm(mu2_p[j],tau.p2) 
mu2_p[j] <- a.t2
}

tau.p2 <- pow(sigma.p2, -2)
sigma.p2 ~ dunif(0,100)

a.t2 ~ dnorm(0, .0001) 

#random intercepts for elections 
for(k in 1:NE){
a_e[k] ~ dnorm(mu1_e[k],tau.e1) 
mu1_e[k] <- a_c[cntryje[k]]
}

#a.e ~ dnorm(0, .0001) 

tau.e1 <- pow(sigma.e1, -2)
sigma.e1 ~ dunif(0,100)

#random intercepts for countries
for(m in 1:N){
a_c[m] ~ dnorm(mu1_c[m],tau.c1) 
mu1_c[m] <- a.c
}

a.c ~ dnorm(0, .0001) 

tau.c1 <- pow(sigma.c1, -2)
sigma.c1 ~ dunif(0,100)

}"

write(micro.model, file="micro.model.jags")

micro.data <- list(N=N, NP=NP, NE=NE, NPV=NPV, pvotes=lpv, time=time, prty=prty, elec=elec, cntryje=cntryje, cntryjp=cntryjp)

micro.parameters <- c("sigma.pv","sigma.p0","sigma.p1","sigma.p2","sigma.e1","sigma.c1","a.c","a.t","a.t2","b_time","b_time2","a_c","a_e","a_p")

jags.micro <- jags.model(file="micro.model.jags", data = micro.data, n.chains = 3, n.adapt = 100)

sampleshelp <- coda.samples(jags.micro, micro.parameters, n.iter=1000, thin=10)

samplesburn <- coda.samples(jags.micro, micro.parameters, n.iter=48000, thin=48)

samples <- coda.samples(jags.micro, micro.parameters, n.iter=20000, thin=200)

plot(sampleshelp, ask=TRUE)

plot(samples, ask=TRUE)



kette <- as.matrix(samples)

#intercept
ba <- kette[,"a.c"] 
mba <- mean(ba)
sba <- sd(ba)

#mean time
bt1 <- kette[,"a.t"] 
mbt1 <- mean(bt1)
sbt1 <- sd(bt1)

#mean time squared
bt2 <- kette[,"a.t2"] 
mbt2 <- mean(bt2)
sbt2 <- sd(bt2)

#SDs
#elections
bre <- kette[,"sigma.e1"] 
mre <- mean(bre)
sre <- sd(bre)

#parties
brp <- kette[,"sigma.p0"] 
mrp <- mean(brp)
srp <- sd(brp)

#countries
brc <- kette[,"sigma.c1"] 
mrc <- mean(brc)
src <- sd(brc)

#slope of time
brt <- kette[,"sigma.p1"] 
mrt <- mean(brt)
srt <- sd(brt)

#slope of time squared
brt2 <- kette[,"sigma.p2"] 
mrt2 <- mean(brt2)
srt2 <- sd(brt2)

#lowest level
bpv <- kette[,"sigma.pv"] 
mpv <- mean(bpv)
spv <- sd(bpv)


m <- c(mba,mbt1,mbt2*10,mrc,mrp,mre,mrt,mrt2*10,mpv)

s <- c(sba,sbt1,sbt2*10,src,srp,sre,srt,srt2*10,spv)


vlabels <- c("Mean","Time (mean)","T^2 (m.)*10","SD countries","SD parties","SD elections","SD time","SD t^2*10","SD") 

#Fig 2

coefplot(m,s,varnames=vlabels,vertical=FALSE,main="",ylim=c(-.5,1.5))




#Reduction rate -> Fig 4

bt21 <- kette[,"b_time2[1]"] 
mbt21 <- mean(bt21)
sbt21 <- sd(bt21)

bt22 <- kette[,"b_time2[2]"] 
mbt22 <- mean(bt22)
sbt22 <- sd(bt22)

bt23 <- kette[,"b_time2[3]"] 
mbt23 <- mean(bt23)
sbt23 <- sd(bt23)

bt24 <- kette[,"b_time2[4]"] 
mbt24 <- mean(bt24)
sbt24 <- sd(bt24)

bt25 <- kette[,"b_time2[5]"] 
mbt25 <- mean(bt25)
sbt25 <- sd(bt25)

bt26 <- kette[,"b_time2[6]"] 
mbt26 <- mean(bt26)
sbt26 <- sd(bt26)

bt27 <- kette[,"b_time2[7]"] 
mbt27 <- mean(bt27)
sbt27 <- sd(bt27)

bt28 <- kette[,"b_time2[8]"] 
mbt28 <- mean(bt28)
sbt28 <- sd(bt28)

bt29 <- kette[,"b_time2[9]"] 
mbt29 <- mean(bt29)
sbt29 <- sd(bt29)

bt210 <- kette[,"b_time2[10]"] 
mbt210 <- mean(bt210)
sbt210 <- sd(bt210)

bt211 <- kette[,"b_time2[11]"] 
mbt211 <- mean(bt211)
sbt211 <- sd(bt211)

bt212 <- kette[,"b_time2[12]"] 
mbt212 <- mean(bt212)
sbt212 <- sd(bt212)

bt213 <- kette[,"b_time2[13]"] 
mbt213 <- mean(bt213)
sbt213 <- sd(bt213)

bt214 <- kette[,"b_time2[14]"] 
mbt214 <- mean(bt214)
sbt214 <- sd(bt214)

bt215 <- kette[,"b_time2[15]"] 
mbt215 <- mean(bt215)
sbt215 <- sd(bt215)

bt216 <- kette[,"b_time2[16]"] 
mbt216 <- mean(bt216)
sbt216 <- sd(bt216)

mt2 <- c(mbt21,mbt22,mbt23,mbt24,mbt25,mbt26,mbt27,mbt28,mbt29,mbt210,mbt211,mbt212,mbt213,mbt214,mbt215,mbt216)

st2 <- c(sbt21,sbt22,sbt23,sbt24,sbt25,sbt26,sbt27,sbt28,sbt29,sbt210,sbt211,sbt212,sbt213,sbt214,sbt215,sbt216)


vlabels <- c("FPÖ","VB","EDU","LDT","SVP","DF","NPD","PS","FN","BNP","UKIP","LN","PVV","SGP","FRP","SD")



var.names <- c(vlabels)
m.v <- mt2
sd.v <- st2

pic <- data.frame(var.names,m.v,sd.v)
pic.sort <- pic[order(m.v) , ]

var.names <- pic.sort$var.names
m.v <- pic.sort$m.v
sd.v <- pic.sort$sd.v

y.axis <- length(var.names):1 
layout(matrix(c(2,1),1,2),  
    widths = c(1.5, 5)) 

par(mar=c(2,6,.5,1), lheight = .8) 
plot(m.v, y.axis, type = "p", axes = F, xlab = "", ylab = "", pch = 19, xlim = c(-.02,.01), cex=1, ylim = c(min(y.axis), max(y.axis)), main = "")
axis(1,at = seq(-.02,.01, by = .01), label = seq(-.02,.01, by = .01), cex.axis=.9)
axis(2, at = y.axis, label = var.names, las = 1, tick = T, font=1, cex.axis=.9)
abline(h = y.axis, lty = 2, lwd = .5, col = "grey")
segments(m.v-qnorm(.975)*sd.v, y.axis, m.v+qnorm(.975)*sd.v, y.axis, lwd =  1.5)
abline(v=0, lty = 2)
segments(m.v-qnorm(.9)*sd.v, y.axis -.1, m.v-qnorm(.9)*sd.v, y.axis +.1, lwd = 1.5) 
segments(m.v+qnorm(.9)*sd.v, y.axis -.1, m.v+qnorm(.9)*sd.v, y.axis +.1, lwd = 1.5)

par(mar=c(2,0,.5,0)) 
plot(seq(0,1,length=length(var.names)), y.axis, type = "n", axes = F,  xlab = "", ylab = "")

left.side <- .6 
segments(left.side,16,left.side,1) 
segments(left.side,16,left.side+.1,16) 
segments(left.side,1,left.side+.1,1)
text(.5, 8.5, "Party Slopes of Time Squared", srt = 90, cex=.9)




#b_time -> Fig 3

bt11 <- kette[,"b_time[1]"] 
mbt11 <- mean(bt11)
sbt11 <- sd(bt11)

bt12 <- kette[,"b_time[2]"] 
mbt12 <- mean(bt12)
sbt12 <- sd(bt12)

bt13 <- kette[,"b_time[3]"] 
mbt13 <- mean(bt13)
sbt13 <- sd(bt13)

bt14 <- kette[,"b_time[4]"] 
mbt14 <- mean(bt14)
sbt14 <- sd(bt14)

bt15 <- kette[,"b_time[5]"] 
mbt15 <- mean(bt15)
sbt15 <- sd(bt15)

bt16 <- kette[,"b_time[6]"] 
mbt16 <- mean(bt16)
sbt16 <- sd(bt16)

bt17 <- kette[,"b_time[7]"] 
mbt17 <- mean(bt17)
sbt17 <- sd(bt17)

bt18 <- kette[,"b_time[8]"] 
mbt18 <- mean(bt18)
sbt18 <- sd(bt18)

bt19 <- kette[,"b_time[9]"] 
mbt19 <- mean(bt19)
sbt19 <- sd(bt19)

bt110 <- kette[,"b_time[10]"] 
mbt110 <- mean(bt110)
sbt110 <- sd(bt110)

bt111 <- kette[,"b_time[11]"] 
mbt111 <- mean(bt111)
sbt111 <- sd(bt111)

bt112 <- kette[,"b_time[12]"] 
mbt112 <- mean(bt112)
sbt112 <- sd(bt112)

bt113 <- kette[,"b_time[13]"] 
mbt113 <- mean(bt113)
sbt113 <- sd(bt113)

bt114 <- kette[,"b_time[14]"] 
mbt114 <- mean(bt114)
sbt114 <- sd(bt114)

bt115 <- kette[,"b_time[15]"] 
mbt115 <- mean(bt115)
sbt115 <- sd(bt115)

bt116 <- kette[,"b_time[16]"] 
mbt116 <- mean(bt116)
sbt116 <- sd(bt116)


mt1 <- c(mbt11,mbt12,mbt13,mbt14,mbt15,mbt16,mbt17,mbt18,mbt19,mbt110,mbt111,mbt112,mbt113,mbt114,mbt115,mbt116)

st1 <- c(sbt11,sbt12,sbt13,sbt14,sbt15,sbt16,sbt17,sbt18,sbt19,sbt110,sbt111,sbt112,sbt113,sbt114,sbt115,sbt116)


vlabels <- c("FPÖ","VB","EDU","LDT","SVP","DF","NPD","PS","FN","BNP","UKIP","LN","PVV","SGP","FRP","SD")

var.names <- c(vlabels)
m.v <- mt1
sd.v <- st1

pic <- data.frame(var.names,m.v,sd.v)
pic.sort <- pic[order(m.v) , ]

var.names <- pic.sort$var.names
m.v <- pic.sort$m.v
sd.v <- pic.sort$sd.v

y.axis <- length(var.names):1 
layout(matrix(c(2,1),1,2),  
    widths = c(1.5, 5)) 

par(mar=c(2,6,.5,1), lheight = .8) 
plot(m.v, y.axis, type = "p", axes = F, xlab = "", ylab = "", pch = 19, xlim = c(-.25,.5), cex=1, ylim = c(min(y.axis), max(y.axis)), main = "")
axis(1,at = seq(-.25,.5, by = .25), label = seq(-.25,.5, by = .25), cex.axis=.9)
axis(2, at = y.axis, label = var.names, las = 1, tick = T, font=1, cex.axis=.9)
abline(h = y.axis, lty = 2, lwd = .5, col = "grey")
segments(m.v-qnorm(.975)*sd.v, y.axis, m.v+qnorm(.975)*sd.v, y.axis, lwd =  1.5)
abline(v=0, lty = 2)
segments(m.v-qnorm(.9)*sd.v, y.axis -.1, m.v-qnorm(.9)*sd.v, y.axis +.1, lwd = 1.5) 
segments(m.v+qnorm(.9)*sd.v, y.axis -.1, m.v+qnorm(.9)*sd.v, y.axis +.1, lwd = 1.5)

par(mar=c(2,0,.5,0)) 
plot(seq(0,1,length=length(var.names)), y.axis, type = "n", axes = F,  xlab = "", ylab = "")

left.side <- .6 
segments(left.side,16,left.side,1) 
segments(left.side,16,left.side+.1,16) 
segments(left.side,1,left.side+.1,1)
text(.5, 8.5, "Party Slopes of Time", srt = 90, cex=.9)




#Full model 


micro.model <- "model{

for(i in 1:NPV){
pvotes[i] ~ dnorm(mu.pv[i],tau.pv)
mu.pv[i] <- a_p[prty[i]] +  a_e[elec[i]] +  b_time[prty[i]]*time[i] +  b_time2[prty[i]]*time[i]*time[i] 
#+ b_gov*gov[i] 
}

tau.pv <- pow(sigma.pv, -2)
sigma.pv ~ dunif(0, 50)

#b_gov ~ dnorm(0, .0001)

#random intercepts for parties
for(j in 1:NP){
a_p[j] ~ dnorm(mu0_p[j],tau.p0) 
mu0_p[j] <- a_c[cntryjp[j]]
}

#a.p ~ dnorm(0, .0001) 

tau.p0 <- pow(sigma.p0, -2)
sigma.p0 ~ dunif(0,100)

#random slopes for time per party
for(j in 1:NP){
b_time[j] ~ dnorm(mu1_p[j],tau.p1) 
mu1_p[j] <- a.t
}

tau.p1 <- pow(sigma.p1, -2)
sigma.p1 ~ dunif(0,100)

a.t ~ dnorm(0, .0001) 

#random slopes for time squared per party
for(j in 1:NP){
b_time2[j] ~ dnorm(mu2_p[j],tau.p2) 
mu2_p[j] <- a.t2 
}

tau.p2 <- pow(sigma.p2, -2)
sigma.p2 ~ dunif(0,100)

a.t2 ~ dnorm(0, .0001) 

#random intercepts for elections 
for(k in 1:NE){
a_e[k] ~ dnorm(mu1_e[k],tau.e1) 
mu1_e[k] <- a_c[cntryje[k]] + b_shock*shock[k] + b_immi*immi[k]
}

#a.e ~ dnorm(0, .0001) 
b_shock ~ dnorm(0, .0001)
b_immi ~ dnorm(0, .0001)

tau.e1 <- pow(sigma.e1, -2)
sigma.e1 ~ dunif(0,100)

#random intercepts for countries
for(m in 1:N){
a_c[m] ~ dnorm(mu1_c[m],tau.c1) 
mu1_c[m] <- a.c + b_lm*lm[m]
}

a.c ~ dnorm(0, .0001) 
b_lm ~ dnorm(0, .0001)

tau.c1 <- pow(sigma.c1, -2)
sigma.c1 ~ dunif(0,100)

}"

write(micro.model, file="micro.model.jags")

micro.data <- list(N=N, NP=NP, NE=NE, NPV=NPV, pvotes=lpv, time=time, prty=prty, elec=elec, cntryje=cntryje, cntryjp=cntryjp, immi=impk, gov=gov, shock=shock, lm=lm)

micro.parameters <- c("sigma.pv","sigma.p0","sigma.p1","sigma.p2","sigma.e1","sigma.c1","a.c","a.t","a.t2","b_time","b_time2","a_c","a_e","a_p","b_shock","b_lm","b_immi"
#,"b_gov"
)

jags.micro <- jags.model(file="micro.model.jags", data = micro.data, n.chains = 3, n.adapt = 100)

sampleshelp <- coda.samples(jags.micro, micro.parameters, n.iter=1000, thin=10)

samplesburn <- coda.samples(jags.micro, micro.parameters, n.iter=48000, thin=48)

samples <- coda.samples(jags.micro, micro.parameters, n.iter=20000, thin=200)

plot(sampleshelp, ask=TRUE)

plot(samples, ask=TRUE)





kette <- as.matrix(samples)

#intercept
ba <- kette[,"a.c"] 
mba <- mean(ba)
sba <- sd(ba)

#mean time
bt1 <- kette[,"a.t"] 
mbt1 <- mean(bt1)
sbt1 <- sd(bt1)

#mean time squared
bt2 <- kette[,"a.t2"] 
mbt2 <- mean(bt2)
sbt2 <- sd(bt2)

#SDs
#elections
bre <- kette[,"sigma.e1"] 
mre <- mean(bre)
sre <- sd(bre)

#parties
brp <- kette[,"sigma.p0"] 
mrp <- mean(brp)
srp <- sd(brp)

#countries
brc <- kette[,"sigma.c1"] 
mrc <- mean(brc)
src <- sd(brc)

#slope of time
brt <- kette[,"sigma.p1"] 
mrt <- mean(brt)
srt <- sd(brt)

#slope of time squared
brt2 <- kette[,"sigma.p2"] 
mrt2 <- mean(brt2)
srt2 <- sd(brt2)

#lowest level
bpv <- kette[,"sigma.pv"] 
mpv <- mean(bpv)
spv <- sd(bpv)

#b_shock
bsh <- kette[,"b_shock"] 
mbsh <- mean(bsh)
sbsh <- sd(bsh)

#b_gov
#bgv <- kette[,"b_gov"] 
#mbgv <- mean(bgv)
#sbgv <- sd(bgv)

#b_immi
bim <- kette[,"b_immi"] 
mbim <- mean(bim)
sbim <- sd(bim)

#b_lm
blm <- kette[,"b_lm"] 
mblm <- mean(blm)
sblm <- sd(blm)


m <- c(mba,mbt1,mbt2*10,mbsh,mbim,
#mbgv,
mblm,mrc,mrp,mre,mrt,mrt2*10,mpv)

s <- c(sba,sbt1,sbt2*10,sbsh,sbim,
#sbgv,
sblm,src,srp,sre,srt,srt2*10,spv)


vlabels <- c("Mean","Time (mean)","T^2 (m.)*10","Shock","Immi.",
#"Gov.",
"M (log)","SD countries","SD parties","SD elections","SD time","SD t^2*10","SD") 


#Fig 5
coefplot(m,s,varnames=vlabels,vertical=FALSE,main="",ylim=c(-.5,1.5))


