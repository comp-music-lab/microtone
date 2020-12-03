setwd("/Users/pesavage/Documents/Research/Papers/Unpublished/Ho et al (Consonance experiments)")

ho<-read.csv("Ho et al Data for SMPC abstract.csv",header=TRUE)
micro<-subset(ho,ho$Microtonal==1)
chromatic<-subset(ho,ho$Microtonal==0)
dissonant<-subset(ho,ho$Consonance==2)
consonant<-subset(ho,ho$Consonance==3)


t.test(micro$Diotic.harmonic.pleasantness,chromatic$Diotic.harmonic.pleasantness)
t.test(micro$Melodic.pleasantness,chromatic$Melodic.pleasantness)

cor.test(ho$Melodic.pleasantness,ho$Harmonic.similarity)
cor.test(ho$Diotic.harmonic.pleasantness,ho$Harmonic.similarity)

t.test(micro$Diotic.harmonic.pleasantness,dissonant$Diotic.harmonic.pleasantness)
t.test(micro$Melodic.pleasantness,dissonant$Melodic.pleasantness)

t.test(consonant$Diotic.harmonic.pleasantness,dissonant$Diotic.harmonic.pleasantness)
t.test(consonant$Melodic.pleasantness,dissonant$Melodic.pleasantness)

boxplot(micro$Diotic.harmonic.pleasantness,dissonant$Diotic.harmonic.pleasantness,consonant$Diotic.harmonic.pleasantness)

boxplot(micro$Melodic.pleasantness,dissonant$Melodic.pleasantness,consonant$Melodic.pleasantness)