#This code was used on 2019-??-?? to perform the analyses of microtonal consonance published in:
#Consonance responses to microtonal intervals Ho, Tomokane, Konno, Harrison, McDermott, Fujii, Savage (in prep)

#Please cite this publication when adapting this code


#First, to prevent errors due to running in Japan, run the following line of code then restart R:
system("defaults write org.R-project.R force.LANG en_US.UTF-8")

#This code requires installing the following packages.

if (!require(devtools)) install.packages("devtools")
devtools::install_github("pmcharrison/incon")


if (!require(devtools)) install.packages("devtools")
devtools::install_github("pmcharrison/inconData")

install.packages("lm.beta")
install.packages("lsr")
install.packages("pwr")
install.packages("ggplot2")
install.packages("tidyr")


library(incon)
library(pwr)
library(lm.beta)
library(lsr)
library(ggplot2)
library(tidyr)

#create custom function to test for microtonality
is.wholenumber <- function(x, tol = .Machine$double.eps^0.5)  abs(x - round(x)) < tol

#open R, change workspace to correct folder
setwd("/Users/jamsie_jump0928/Documents/Compmusic/Aesthetics/Analyzation/microtone-main")

#Sample consonance
models <- c("hutch_78_roughness", 
            "parn_94_pure",
            "huron_94_dyadic")
incon(c(60, 67), models) # perfect 5th

#power analysis
pwr.t.test(n = , d = 0.4 , sig.level = 0.025, power = 0.8, type = c("paired"), alternative=c("two.sided"))





#######for harmonic intervals:

#import spreadsheet:

data<-read.csv("consonance_raw_transposed.csv")
mdata<-read.csv("consonance_melodic_raw_transposed.csv")
data5<-read.csv("4_15 Consonance Workshop (Responses) - Overview.csv")

#set base pitch
p<-60

#see which models can accommodate microtonality
incon_models

#create new columns of predicted consonance based on various models that can accommodate microtonality or unison (Bowling) 


for(i in 1:length(data$participants.Intervals)){
  
  data$har[i]<-incon(c(p, (p+data$participants.Intervals[i])), "har_18_harmonicity")
}

for(i in 1:length(data$participants.Intervals)){
  
  data$milne[i]<-incon(c(p, (p+data$participants.Intervals[i])), "milne_13_harmonicity")
}

for(i in 1:length(data$participants.Intervals)){
  
  data$stolz[i]<-incon(c(p, (p+data$participants.Intervals[i])), "stolz_15_periodicity")
}


for(i in 1:length(data$participants.Intervals)){
  
  data$hutch[i]<-incon(c(p, (p+data$participants.Intervals[i])), "hutch_78_roughness")
}

for(i in 1:length(data$participants.Intervals)){
  
  data$seth[i]<-incon(c(p, (p+data$participants.Intervals[i])), "seth_93_roughness")
}

for(i in 1:length(data$participants.Intervals)){
  
  data$vass[i]<-incon(c(p, (p+data$participants.Intervals[i])), "vass_01_roughness")
}

for(i in 1:length(data$participants.Intervals)){
  
  data$wang[i]<-incon(c(p, (p+data$participants.Intervals[i])), "wang_13_roughness")
}

data$corp<-c(-2,-7,-4.7,-7,-3.4,-7,-2.5,-7,-2.6,-7,-2.3,-7,-4.3,-7,-2,-7,-2.8,-7,-2.5,-7,-3.1,-7,-4.3,-7,-1.1) #these values are eyeballed from Fig. 3a in Jacoby et al (in prep), using -7 as log2 probability for microtonal values because can't get logarithm of 0. I estimate "-2" for unison since Jacoby et al. did not provide that

for(i in 1:length(data$participants.Intervals)){
  data$micro[i]<-ifelse(is.wholenumber(data$participants.Intervals[i]),1,0)
}

data$mean<-rowMeans(data[,1:n+1])

#calculate all possible correlations (not including with interval size)
cor<-cor(data[,2:length(data)])

#remove correlations not of interest
cor<-cor[1:n,(n+1):(n+9)]

#make all correlations in the same direction:
cor[,3:7]<- -(cor[,3:7])


#perform multiple regression

######NB: THIS IS CURRENTLY NOT FULLY AUTOMATED TO DEAL WITH THE POSSIBILITY THAT DIFFERENT MODELS MAY PERFORM BETTER OR WORSE, AND ALSO DOESN'T YET FULLY DEAL WITH THE POSSIBILITY THAT REGRESSION COEFFICIENTS MAY BE POSITIVE OR NEGATIVE, AND IT IS THE AVERAGE ABSOLUTE SIZE THAT IS IMPORTANT TO COMPARE.

fit <- lm(mean ~ stolz + hutch + corp, data=data)

#add beta coefficients
beta<-lm.beta(fit)

summary(beta)

#create new data frame summarizing regressions for each participant 
out<-data.frame(coefficients(fit))

#set number of participants
n<-16

for(i in 1:n){
  fit <- lm.beta(lm(data[,1+i] ~ stolz + hutch + corp, data=data))
  out[,i]<-data.frame(coefficients(fit))
}

#transpose
out<-t(out)

#make Hutchinson coefficients positive (NB: THIS MAY NOT BE APPROPRIATE FOR ALL MODELS - DOUBLE-CHECK/AUTOMATE IN FUTURE)
out[,3]<- -(out[,3])


#paired t test of standardized regression coefficients:
t.test(out[,2],out[,3], paired=TRUE, alternative="two.sided")

#calculate effect size:
cohensD(out[,2],out[,3],method="paired")

#paired t test of standardized regression coefficients:
t.test(out[,2],out[,4], paired=TRUE, alternative="two.sided")

#calculate effect size:
cohensD(out[,2],out[,4],method="paired")

#paired t test of standardized regression coefficients:
t.test(out[,3],out[,4], paired=TRUE, alternative="two.sided")

#calculate effect size:
cohensD(out[,3],out[,4],method="paired")




#######for melodic intervals:

#import spreadsheet:
mdata<-read.csv("consonance_melodic_raw_transposed.csv")

#set base pitch
p<-60

#see which models can accommodate microtonality
incon_models

#create new columns of predicted consonance based on various models that can accommodate microtonality or unison (Bowling) 


for(i in 1:length(mdata$participants.Intervals)){
  
  mdata$har[i]<-incon(c(p, (p+mdata$participants.Intervals[i])), "har_18_harmonicity")
}

for(i in 1:length(mdata$participants.Intervals)){
  
  mdata$milne[i]<-incon(c(p, (p+mdata$participants.Intervals[i])), "milne_13_harmonicity")
}

for(i in 1:length(mdata$participants.Intervals)){
  
  mdata$stolz[i]<-incon(c(p, (p+mdata$participants.Intervals[i])), "stolz_15_periodicity")
}


for(i in 1:length(mdata$participants.Intervals)){
  
  mdata$hutch[i]<-incon(c(p, (p+mdata$participants.Intervals[i])), "hutch_78_roughness")
}

for(i in 1:length(mdata$participants.Intervals)){
  
  mdata$seth[i]<-incon(c(p, (p+mdata$participants.Intervals[i])), "seth_93_roughness")
}

for(i in 1:length(mdata$participants.Intervals)){
  
  mdata$vass[i]<-incon(c(p, (p+mdata$participants.Intervals[i])), "vass_01_roughness")
}

for(i in 1:length(mdata$participants.Intervals)){
  
  mdata$wang[i]<-incon(c(p, (p+mdata$participants.Intervals[i])), "wang_13_roughness")
}

mdata$corp<-c(-2,-7,-2,-7,-1.3,-7,-2,-7,-2.5,-7,-2.2,-7,-4.3,-7,-2.6,-7,-3.5,-7,-3.3,-7,-3.7,-7,-4.8,-7,-2.8) #these values are eyeballed from Fig. 3a in Jacoby et al (in prep), using -7 as log2 probability for microtonal values because can't get logarithm of 0. I estimate "-2" for unison since Jacoby et al. did not provide that

for(i in 1:length(mdata$participants.Intervals)){
  mdata$micro[i]<-ifelse(is.wholenumber(mdata$participants.Intervals[i]),1,0)
}

mdata$mean<-rowMeans(mdata[,1:n+1])


#calculate all possible correlations (not including with interval size)
mcor<-cor(mdata[,2:length(mdata)])

#remove correlations not of interest
mcor<-mcor[1:n,(n+1):(n+9)]

#make all correlations in the same direction:
mcor[,3:7]<- -(mcor[,3:7])


#perform multiple regression

######NB: THIS IS CURRENTLY NOT FULLY AUTOMATED TO DEAL WITH THE POSSIBILITY THAT DIFFERENT MODELS MAY PERFORM BETTER OR WORSE, AND ALSO DOESN'T YET FULLY DEAL WITH THE POSSIBILITY THAT REGRESSION COEFFICIENTS MAY BE POSITIVE OR NEGATIVE, AND IT IS THE AVERAGE ABSOLUTE SIZE THAT IS IMPORTANT TO COMPARE.

fit <- lm(means ~ har + seth + corp, data=mdata)

#add beta coefficients
beta<-lm.beta(fit)

summary(beta)

#create new data frame summarizing regressions for each participant 
mout<-data.frame(coefficients(fit))

#set number of participants
n<-16

for(i in 1:n){
  fit <- lm.beta(lm(mdata[,1+i] ~ har + seth + corp, data=mdata))
  mout[,i]<-data.frame(coefficients(fit))
}

#transpose
mout<-t(mout)

#make coefficients positive (NB: COMMENTING THIS OUT FOR HAR BECAUSE IT IS NOT NEEDED - DOUBLE-CHECK/AUTOMATE IN FUTURE)
#mout[,2]<- -(mout[,2])


#paired t test of standardized regression coefficients:
t.test(mout[,2],mout[,3], paired=TRUE, alternative="two.sided")

#calculate effect size:
cohensD(mout[,2],mout[,3],method="paired")

#paired t test of standardized regression coefficients:
t.test(mout[,2],mout[,4], paired=TRUE, alternative="two.sided")

#calculate effect size:
cohensD(mout[,2],mout[,4],method="paired")

#paired t test of standardized regression coefficients:
t.test(mout[,3],mout[,4], paired=TRUE, alternative="two.sided")

#calculate effect size:
cohensD(mout[,3],mout[,4],method="paired")


#plotting figures
#violin plot for correlations with consonance models
cor<-as.data.frame(cor)
cor.long<-gather(cor, model, cor, har:micro, factor_key=TRUE)
p <- ggplot(cor.long, aes(x=model,y=cor,label=cor,color=model))  + geom_violin(trim=FALSE,fill="white")
data_summary <- function(x) {
  m <- mean(x)
  ymin <- m-sd(x)*1.96
  ymax <- m+sd(x)*1.96
  return(c(y=m,ymin=ymin,ymax=ymax))
}
p<-p + stat_summary(fun.data=data_summary,col="red")
p<-p + geom_jitter(shape=16, position=position_jitter(0.2),color="black")
p<-p + scale_color_manual(values=c("blue","blue","blue","red","red","red","red","yellow","yellow"),
                       name="Model type",
                       breaks=c("stolz","hutch","corp"),
                       labels=c("Harmonicity", "Roughness", "Culture"))
p

attach(mtcars)
par(mfrow=c(2,2))

#violin plot for Beta 
out<-as.data.frame(out)
out.long<-gather(out, Consonance_model, Beta, stolz:corp, factor_key=TRUE)
q <- ggplot(out.long, aes(x=Consonance_model,y=Beta,color=Consonance_model)) + geom_violin(trim=FALSE,fill="white")
data_summary <- function(x) {
  m <- mean(x)
  ymin <- m-sd(x)*1.96
  ymax <- m+sd(x)*1.96
  return(c(y=m,ymin=ymin,ymax=ymax))
}
q<-q + stat_summary(fun.data=data_summary,col="red")
q<-q + geom_jitter(shape=16, position=position_jitter(0.2),color="black")
q<-q + scale_color_manual(values=c("blue","red","yellow"),
                           name="Model type",
                           labels=c("Harmonicity", "Roughness", "Culture"))
q


boxplot(cor,ylab="Partial correlation with consonance ratings", main="Harmonic \n A",col=c("blue","blue","blue","red","red","red","red","yellow","yellow"))
boxplot(mcor,main="Melodic \n C",col=c("blue","blue","blue","red","red","red","red","yellow","yellow"))
boxplot(out[,2:4],ylab="Beta",xlab="Consonance model",main="B",col=c("blue","red","yellow"))
legend("topleft",legend= c("Harmonicity","Roughness","Culture"),fill=c("blue","red","yellow"))
boxplot(mout[,2:4],xlab="Consonance model",main="D",col=c("blue","red","yellow"))

#average plot
#harmonic diotic comparison (microtonal vs. chromatic)
data5<-read.csv("4_15 Consonance Workshop (Responses) - Overview.csv")
columnList <- c("group","P_MELODIC_DIOTIC", "P_MELODIC_DICHOTIC", "P_HARMONIC_DIOTIC", "P_HARMONIC_DICHOTIC")
data5plot<-data5[,columnList]
#To test for "consonance" type C, to test for "beauty", type B instead of P as "pleasantness"
hardio <- c ("group","P_HARMONIC_DIOTIC")
hardioplot <- data5plot[,hardio]
df5<-as.data.frame(hardioplot)
s <- ggplot(df5, aes(x=group,y=P_HARMONIC_DIOTIC ,color=group)) + geom_violin(trim=FALSE,fill="white")
data_summary <- function(x) {
  m <- mean(x)
  ymin <- m-sd(x)*1.96
  ymax <- m+sd(x)*1.96
  return(c(y=m,ymin=ymin,ymax=ymax))
}
s<-s + stat_summary(fun.data=data_summary,col="red")
s<-s + geom_jitter(shape=16, position=position_jitter(0.2),color="black")
s<-s + scale_color_manual(values=c("blue","orange"),
                          name="Interval type",
                          labels=c("Chromatic", "Microtonal"))
s

#correlation plot for melodic vs harmonic (diotic)
data5<-read.csv("4_15 Consonance Workshop (Responses) - Overview.csv")
columnList <- c("group","P_MELODIC_DIOTIC", "P_MELODIC_DICHOTIC", "P_HARMONIC_DIOTIC", "P_HARMONIC_DICHOTIC")
data5plot<-data5[,columnList]
cordio <- c("P_MELODIC_DIOTIC","P_HARMONIC_DIOTIC")
d5cor <- data5plot[,cordio]
df5cor <- as.data.frame(d5cor)
t<-ggplot(df5cor,aes(x=P_MELODIC_DIOTIC,y=P_HARMONIC_DIOTIC)) + geom_point (size =2)+ geom_smooth(method=lm, se=FALSE)









