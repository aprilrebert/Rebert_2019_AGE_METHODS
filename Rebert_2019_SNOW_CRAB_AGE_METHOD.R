###################################################
## ----------------
## Evaluation of a direct age method for terminally molted male snow crab
## ----------------
## 
## April L Rebert
## april.rebert@alaska.gov
## Alaska Department of Fish and Game, 802 3rd Street, Douglas, AK 99824, United States 
##
## PART I:   BAND COUNT AND SHELL CONDITION
##      IA:  AGE BIAS
##            FIGURE 3: INDEPENDENT BAND COUNT ESTIMATES
##      IB:  BAND COUNT AND SHELL CONDITION
##            FIGURE 4: FINAL BAND COUNT BY SHELL CONDITION
## PART II:  BAND COUNT AND ENDOCUTICLE THICKNESS
##            FIGURE 5: ENDOCUTICLE MEASUREMENTS ACROSS SHELL CONDITION
##
###################################################

rm(list=ls(all=T))

# LOAD LIBRARIES
library(FSA)
library(ggplot2)

windowsFonts("Times"=windowsFont("TT Times New Roman"))

## LOAD DATA "SHELL_CONDITION_AND_ENDOCUTICLE_MEASUREMENTS.csv"

Shell <- read.csv(file=file.choose())
Shell <- na.omit(Shell)

###################################################
## 
## PART I:
## BAND COUNT AND SHELL CONDITION
## 
########--------------########
######## IA: AGE BIAS ########
########--------------########

# Age Bias for each reader against the final band count ("Determinator")

ab.tA1 <- ageBias(Band.Count.Reader.1~Final.Band.Count,data=Shell,ref.lab="Determinator",nref.lab="Reader 1")
ab.tA2 <- ageBias(Band.Count.Reader.2~Final.Band.Count,data=Shell,ref.lab="Determinator",nref.lab="Reader 2")
ab.tA3 <- ageBias(Band.Count.Reader.1~Band.Count.Reader.2,data=Shell,ref.lab="Reader 2",nref.lab="Reader 1")

plot(ab.tA1,col.CIsig="black")
plot(ab.tA1,col.CIsig="black",show.range=TRUE)
plot(ab.tA1,col.CIsig="black",show.pts=TRUE,transparency=1/6)

summary(ab.tA1,what="n")

### APE and CV by reader ####
# !!! NOTE: APE and CV references may change with version of FSA package !!!

ap.R1<-agePrecision(~Band.Count.Reader.1+Final.Band.Count, data=Shell) 
R1.stats<-summary(ap.R1, what="precision")
# APE
R1.stats[1,5]
# CV
R1.stats[1,4]

ap.R2<-agePrecision(~Band.Count.Reader.2+Final.Band.Count, data=Shell) 
R2.stats<-summary(ap.R2, what="precision")
# APE
R2.stats[1,5]
# CV
R2.stats[1,4]

ap.R12<-agePrecision(~Band.Count.Reader.1+Band.Count.Reader.2, data=Shell) 
R12.stats<-summary(ap.R12, what="precision")
# APE
R12.stats[1,5]
# CV
R12.stats[1,4]

par(mfrow=c(1,3))
plot(ab.tA1,col.CIsig="black")
plot(ab.tA2,col.CIsig="black")
plot(ab.tA3,col.CIsig="black")



  ######################
  ##                  ##
  ##    FIGURE 3      ##
  ##                  ##
  ###################### 

par(mfrow=c(1,2), family="Times",omi=c(0.2,0.2,0.5,0))


par(mai=c(0.6,0.6,0.1,0.1))
plotAB(ab.tA1,what="bias",col.agree="gray50", xlim=c(4,19), ylim=c(4,19), xlab= "", ylab="")
title(ylab="Reader 1", line=2, cex.lab=1.4, family="Times")
legend("topleft", bty="n", cex=0.9, border=NULL, paste("APE",round(R1.stats[1,5],1), "\n", "CV", round(R2.stats[1,4], 1))) 

par(mai=c(0.6,0.6,0.1,0.1))
plotAB(ab.tA2,what="bias",col.agree="gray50", xlim=c(4,19), ylim=c(4,19), xlab= "", ylab=" ")
title(ylab="Reader 2", line=2, cex.lab=1.4, family="Times")
legend("topleft",bty="n", cex=0.9, paste("APE", round(R2.stats[1,5], 1), "\n", "CV", round(R2.stats[1,4], 1)))  

mtext("Determinator Band Count", side=1, outer=T, cex=1.4, at=0.535, line=-0.5)


########------------------------------------########
######## IB: BAND COUNT AND SHELL CONDITION ########
########------------------------------------########

# ANOVA

# Determinator band counts
mod<-with(Shell, aov(Final.Band.Count~factor(Shell.Condition)))
anova(mod)

# Reader 1 band counts
mod<-with(Shell, aov(Band.Count.Reader.1~factor(Shell.Condition)))
anova(mod)

# Reader 2 band counts
mod<-with(Shell, aov(Band.Count.Reader.2~factor(Shell.Condition)))
anova(mod)

#********** Size and Band Count **********#

# Linear models

# Determinator band counts
mod<-with(Shell, lm(Final.Band.Count~Animal.Size))
summary(mod)

# Reader 1 band counts
mod<-with(Shell, lm(Band.Count.Reader.1~Animal.Size))
summary(mod)

# Reader 2 band counts
mod<-with(Shell, lm(Band.Count.Reader.2~Animal.Size))
summary(mod)


  ######################
  ##                  ##
  ##    FIGURE 4      ##
  ##                  ##
  ###################### 

sc.plot<- ggplot(Shell, aes(x=as.factor(Shell.Condition), y=Final.Band.Count, size=15))+
  geom_boxplot(colour = "black", size = 2, show.legend = FALSE)+
  guides(fill=FALSE)+
  theme(axis.line = element_line(colour = "black"),
        text = element_text(size=16, family="Times"),
        axis.text.x = element_text(colour="black"),
        axis.text.y = element_text(colour="black"),
        plot.title = element_text(hjust=0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  scale_x_discrete(labels=c("2" = "New", "3" = "Old",
                            "4" = "Very Old"))+
  labs(x="Shell Condition", y="Band Count") 

sc.plot

###################################################
## 
## PART II:
## ENDOCUTICLE THICKNESS AND SHELL CONDITION
## 

# ANOVA
mod<-with(Shell, aov(Endocuticle.Measurement~factor(Shell.Condition)))
anova(mod)

  ######################
  ##                  ##
  ##    FIGURE 5      ##
  ##                  ##
  ###################### 

et.plot<- ggplot(Shell, aes(x=as.factor(Shell.Condition), y=Endocuticle.Measurement, size=15))+
  geom_boxplot(colour = "black", size = 2, show.legend = FALSE)+
  guides(fill=FALSE)+
  theme(axis.line = element_line(colour = "black"),
        text = element_text(size=16, family="Times"),
        axis.text.x = element_text(colour="black"),
        axis.text.y = element_text(colour="black"),
        plot.title = element_text(hjust=0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  scale_x_discrete(labels=c("2" = "New", "3" = "Old",
                            "4" = "Very Old"))+
  labs(x="Shell Condition", y="Endocuticle Thickness (mm)") 

et.plot


