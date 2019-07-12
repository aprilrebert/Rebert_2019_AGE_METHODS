###################################################
## ----------------
## Development of procedures for sectioning calcified hard parts 
## as potential age structures for three Alaska crustacean species
## ----------------
## 
## April L Rebert
## april.rebert@alaska.gov
## Alaska Department of Fish and Game, 802 3rd Street, Douglas, AK 99824, United States 
##
## PART I:   STRUCTURE PROCESSING METHODS DEVELOPMENT
##      IA:  THIN SECTION THICKNESS
##            FIGURE 5: CRAB THIN SECTION THICKNESS READABILITY
##            FIGURE 6: SHRIMP THIN SECTION THICKNESS READABILITY
##      IB:  PREFERRED STRUCTURE AND LOCATION
##            FIGURE 7: RED KING CRAB ZYGOCARDIAC READABILITY
##            FIGURE 8: LARGE AND SMALL SNOW CRAB ZYGOCARDIAC READABILITY
##            FIGURE 9: SNOW CRAB ZYGOCARDIAC READABILITY
## PART II:  BAND COUNT
##      IIA: PRECISION
##            FIGURE 10: INDEPENDENT RED KING AND SNOW CRAB BAND COUNT ESTIMATES
##            FIGURE 11: INDEPENDENT SPOT SHRIMP BAND COUNT ESTIMATES
##      IIB: BAND COUNT AND SIZE CLASS
##            FIGURE 12:  BAND COUNT ESTIMATES
##
###################################################
## 
## PART I:
## STRUCTURE PROCESSING METHODS DEVELOPMENT
## 
########----------------------------########
######## IA: THIN SECTION THICKNESS ########
########----------------------------########

rm(list=ls(all=T))

# LOAD LIBRARIES
library(ggplot2)
library(mgcv)
library(dplyr)
library(scales)
library(ggpubr)
windowsFonts("Times"=windowsFont("TT Times New Roman"))

## LOAD DATA "THIN_SECTION_QUALITY.csv"

Quality <- read.csv(file=file.choose(), header=T, na.strings="")
names(Quality)

# EXCLUDE DAMAGED AND BROKEN THIN SECTIONS

Quality<- subset(Quality, Readability<6)


## READABLE SECTIONS AS READBILITY EQUAL TO 1

Quality$Readable <- Quality$Readability==1

Quality$Animal.ID <- as.factor(Quality$Animal.ID)


## BREAK DATA INTO SUBSETS FOR EACH SPECIES

Red.Crab <- subset(Quality, Species == "red king crab")
Snow.Crab <- subset(Quality, Species == "snow crab")
Spot.Shrimp <- subset(Quality, Species == "spot shrimp")


#********** Red King Crab Thickness **********#

# BASIC VISUAL

plot(Red.Crab$Readability~Red.Crab$Section.Thickness)

# FIT GENERALIZED ADDITIVE MODEL AND SEE RESULTS

RedMod<-gam(Readability~s(Section.Thickness), family=binomial, data=Red.Crab)
summary(RedMod)

# PREPARE VARIABLES FOR GRAPH OF THE FITTED MODEL

Red.Crab1<-with(Red.Crab, data.frame(Section.Thickness))
Red.Crab1$P<- predict(RedMod, newdata=Red.Crab1, type="response")

Red.Crab2<-with(Red.Crab1, data.frame(Section.Thickness=rep(seq(from=1, to=600, length.out=500))))

Red.Crab3<-cbind(Red.Crab2, predict(RedMod, newdata=Red.Crab2, type="link", se=TRUE))
plot(Red.Crab2)
Red.Crab2<-within(Red.Crab3, 
              { PredictedProb<-plogis(fit) 
              LL<- plogis(fit-1.96*se.fit) 
              UL<-plogis(fit+(1.96*se.fit))})


RedCTRUE <- subset(Red.Crab, Readable=="TRUE")
RedCFALSE <- subset(Red.Crab, Readable=="FALSE")

  #** RED KING CRAB READABILITY ACROSS SECTION THICKNESS GRAPH OF FITTED GAM **#

Red.King.Crab.Thickness <- 
  ggplot(Red.Crab2, aes(x=Section.Thickness, y=PredictedProb))+ 
  scale_y_continuous(labels = scales::percent_format(suffix = ""))+
  theme(axis.line = element_line(colour = "black"),
        text = element_text(size = 20, family= "Times"),
        axis.text.x = element_text(family = "Times", colour="black", size = 20),
        axis.text.y = element_text(family = "Times", colour="black", size = 20),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  geom_ribbon(aes(ymin=LL, ymax=UL), alpha= 0.2) +
  geom_line(size=1 )+ 
  labs(title = expression(paste("Red King Crab")), x=NULL, y=NULL)+
  geom_rug(data=RedCTRUE,aes(x=Section.Thickness,y=0), alpha=0.5, sides="t")+
  geom_rug(data=RedCFALSE,aes(x=Section.Thickness,y=1),alpha=0.5, sides="b")


Red.King.Crab.Thickness

#********** Snow Crab Thickness **********#

# BASIC VISUAL

plot(Snow.Crab$Readability~Snow.Crab$Section.Thickness)

# FIT GENERALIZED ADDITIVE MODEL AND SEE RESULTS

SnowMod<-gam(Readability~s(Section.Thickness), family=binomial, data=Snow.Crab)
summary(SnowMod)

# PREPARE VARIABLES FOR GRAPH OF THE FITTED MODEL

Snow.Crab1<-with(Snow.Crab, data.frame(Section.Thickness))
Snow.Crab1$P<- predict(SnowMod, newdata=Snow.Crab1, type="response")

Snow.Crab2<-with(Snow.Crab1, data.frame(Section.Thickness=rep(seq(from=50, to=300, length.out=500))))

Snow.Crab3<-cbind(Snow.Crab2, predict(SnowMod, newdata=Snow.Crab2, type="link", se=TRUE))
plot(Snow.Crab2)
Snow.Crab2<-within(Snow.Crab3, 
                { PredictedProb<-plogis(fit) 
                LL<- plogis(fit-1.96*se.fit) 
                UL<-plogis(fit+(1.96*se.fit))})

SnowCTRUE <- subset(Snow.Crab, Readable=="TRUE")
SnowCFALSE <- subset(Snow.Crab, Readable=="FALSE")



#** SNOW CRAB READABILITY ACROSS SECTION THICKNESS GRAPH OF FITTED GAM  **#

Snow.Crab.Thickness <- 
    ggplot(Snow.Crab2, aes(x=Section.Thickness, y=PredictedProb))+ 
    scale_y_continuous(labels = scales::percent_format(suffix = ""))+
    theme(axis.line = element_line(colour = "black"),
          text = element_text(size=20, family= "Times"),
          axis.text.x = element_text(family = "Times", colour="black", size= 20),
          axis.text.y = element_text(family = "Times", colour="black", size= 20),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank()) +
    geom_ribbon(aes(ymin=LL, ymax=UL), alpha= 0.2) +
    geom_line(size=1 )+ 
    labs(title = expression(paste("Snow Crab")), x=NULL, y=NULL)+
    geom_rug(data=SnowCTRUE,aes(x=Section.Thickness,y=0), alpha=0.5, sides="t")+
    geom_rug(data=SnowCFALSE,aes(x=Section.Thickness,y=1),alpha=0.5, sides="b")
  
  
Snow.Crab.Thickness
  
  
#********** Spot Shrimp Thickness **********#
  
# BASIC VISUAL  
  
plot(Spot.Shrimp$Readability~Spot.Shrimp$Section.Thickness)

# FIT GENERALIZED ADDITIVE MODEL AND SEE RESULTS  

ShrimpMod<-gam(Readability~s(Section.Thickness), family=binomial, data=Spot.Shrimp)
summary(ShrimpMod)

# PREPARE VARIABLES FOR GRAPH OF THE FITTED MODEL

Spot.Shrimp1<-with(Spot.Shrimp, data.frame(Section.Thickness))
Spot.Shrimp1$P<- predict(ShrimpMod, newdata=Spot.Shrimp1, type="response")
  
Spot.Shrimp2<-with(Spot.Shrimp1, data.frame(Section.Thickness=rep(seq(from=50, to=300, length.out=500))))
  
Spot.Shrimp3<-cbind(Spot.Shrimp2, predict(ShrimpMod, newdata=Spot.Shrimp2, type="link", se=TRUE))
plot(Spot.Shrimp2)
Spot.Shrimp2<-within(Spot.Shrimp3, 
                { PredictedProb<-plogis(fit) 
                LL<- plogis(fit-1.96*se.fit) 
                UL<-plogis(fit+(1.96*se.fit))})
  
ShrimpCTRUE <- subset(Spot.Shrimp, Readable=="TRUE")
ShrimpCFALSE <- subset(Spot.Shrimp, Readable=="FALSE")


#** SPOT SHRIMP READABILITY ACROSS SECTION THICKNESS GRAPH OF FITTED GAM **#

Spot.Shrimp.Thickness <- 
    ggplot(Spot.Shrimp2, aes(x=Section.Thickness, y=PredictedProb))+ 
    scale_y_continuous(labels = scales::percent_format(suffix = ""))+
     xlim(150, 300)+
    theme(axis.line = element_line(colour = "black"),
          text = element_text(size=20, family= "Times"),
          axis.text.x = element_text(family = "Times", colour="black", size=20),
          axis.text.y = element_text(family = "Times", colour="black", size= 20),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank()) +
    geom_ribbon(aes(ymin=LL, ymax=UL), alpha= 0.2) +
    geom_line(size=1 )+ 
    labs(title = expression(paste("Spot Shrimp")), x=expression(paste("Thickness (", mu, "m)")), y="Readable (%)")+
    geom_rug(data=ShrimpCTRUE,aes(x=Section.Thickness,y=0), alpha=0.5, sides="t")+
    geom_rug(data=ShrimpCFALSE,aes(x=Section.Thickness,y=1),alpha=0.5, sides="b")

Spot.Shrimp.Thickness
  
  ######################
  ##                  ##
  ##  FIGURES 5 & 6   ##
  ##                  ##
  ######################
  
  ##      FIGURE 5:   CRAB THIN SECTION THICKNESS READABILITY
  
Figure.5 <- ggarrange(Red.King.Crab.Thickness, Snow.Crab.Thickness, 
                        ncol=1, nrow=2)
  annotate_figure(Figure.5, 
                  left = text_grob( expression(paste("Readable (%)")), family = "Times", rot=90, size = 20 ), 
                  bottom = text_grob(expression(paste("Thickness (", mu, "m)")), family = "Times", size = 20))

Figure.5
  
  ##      FIGURE 6:   SHRIMP THIN SECTION THICKNESS READABILITY
  
Spot.Shrimp.Thickness

  
########--------------------------------------########
######## IB: PREFERRED STRUCTURE AND LOCATION ########
########--------------------------------------######## 

rm(list=ls(all=T))

# LOAD LIBRARIES
library(ggplot2)
library(mgcv)
library(dplyr)
library(scales)
library(ggpubr)
windowsFonts("Times"=windowsFont("TT Times New Roman"))
  
## LOAD DATA "THIN_SECTION_QUALITY.csv"
  
Quality <- read.csv(file=file.choose(), header=T, na.strings="")
  names(Quality)

# EXCLUDE ORIENTATION TRAIL THIN SECTIONS

Quality <- subset(Quality, Orientation.Method =="published")

# EXCLUDE DAMAGED AND BROKEN THIN SECTIONS
  
Quality<- subset(Quality, Readability<6)
Quality$Animal.ID <- as.factor(Quality$Animal.ID)  
  
# READABLE SECTIONS AS READBILITY EQUAL TO 1
  
Quality$Readable <- Quality$Readability==1

  
## BREAK DATA INTO SUBSETS FOR EACH SPECIES
  
Red.Crab <- subset(Quality, Species == "red king crab")
Snow.Crab <- subset(Quality, Species == "snow crab")
Spot.Shrimp <- subset(Quality, Species == "spot shrimp")
  
## ASSIGN SIZE CLASSES TO EACH INDIVIDUAL FOR EACH SPECIES
  
Red.Crab$Length<-as.factor(Red.Crab$Animal.Size>80)
Snow.Crab$Length<-as.factor(Snow.Crab$Animal.Size>65)
Spot.Shrimp$Length<-as.factor(Spot.Shrimp$Animal.Size>30)
  
# LARGE = TRUE, SMALL = FALSE
  

#********** RED KING CRAB STRUCTURE SELECTION **********#
  
# FIT ANOVAS AND VIEW RESULTS #
  
# AMONG STRUCTURES 
Red.mod<-with(Red.Crab, lm(Readable~Structure))
anova(Red.mod)
summary(Red.mod)
    
# AMONG PAIRED (LEFT & RIGHT) STRUCTURES 
Red.mod<-with(Red.Crab, lm(Readable~Paired.Structure))
anova(Red.mod)
summary(Red.mod)
  
# CHI-SQUARE TESTS AND RESULTS #
  
# AMONG STRUCTURES
tbl<-table(Red.Crab$Readable, Red.Crab$Structure)
chisq.test(tbl, simulate.p.value=TRUE)
    
# AMONG PAIRED (LEFT & RIGHT) STRUCTURES
tbl<-table(Red.Crab$Readable, Red.Crab$Paired.Structure)
chisq.test(tbl, simulate.p.value=TRUE)
    
    # PROPORTIONS OF READABLE SECTIONS DO NOT SIGNIFICANTLY DIFFER... 
    # AMONG STRUCTURES OR AMONG LEFT AND RIGHT PAIRED STRUCTURES.
    # USE ZYGOCARDIAC MOVING FORWARD DUE TO ITS PREVALENCE IN THE LITERATURE...
    # AND COMBINE LEFT AND RIGHT PAIRED STRUCTURES.  
  
#********** SNOW CRAB STRUCTURE SELECTION **********#
  
# FIT ANOVAS AND VIEW RESULTS #
    
# AMONG STRUCTURES 
Snow.mod<-with(Snow.Crab, lm(Readable~Structure))
anova(Snow.mod)
summary(Snow.mod)
    
# AMONG PAIRED (LEFT & RIGHT) STRUCTURES 
Snow.mod<-with(Snow.Crab, lm(Readable~Paired.Structure))
anova(Snow.mod)
summary(Snow.mod)
    
# CHI-SQUARE TESTS AND RESULTS #
    
# AMONG STRUCTURES
tbl<-table(Snow.Crab$Readable, Snow.Crab$Structure)
chisq.test(tbl, simulate.p.value=TRUE)
    
# AMONG PAIRED (LEFT & RIGHT) STRUCTURES
tbl<-table(Snow.Crab$Readable, Snow.Crab$Paired.Structure)
chisq.test(tbl, simulate.p.value=TRUE)
    
    # PROPORTIONS OF READABLE SECTIONS DO NOT SIGNIFICANTLY DIFFER... 
    # AMONG STRUCTURES OR AMONG LEFT AND RIGHT PAIRED STRUCTURES.
    # USE ZYGOCARDIAC MOVING FORWARD DUE TO ITS PREVALENCE IN THE LITERATURE...
    # AND COMBINE LEFT AND RIGHT PAIRED STRUCTURES. 

#********** SPOT SHRIMP STRUCTURE SELECTION **********#
    
# FIT ANOVA AND VIEW RESULTS #
    
# AMONG PAIRED (LEFT & RIGHT) STRUCTURES
Shrimp.mod<-with(Spot.Shrimp, lm(Readable~Paired.Structure))
anova(Snow.mod)
summary(Snow.mod)
    
# CHI-SQUARE TESTS AND RESULTS #
    
# AMONG PAIRED (LEFT & RIGHT) STRUCTURES
tbl<-table(Spot.Shrimp$Readable, Spot.Shrimp$Paired.Structure)
    chisq.test(tbl, simulate.p.value=TRUE)
    
    # PROPORTIONS OF READABLE SECTIONS DO NOT SIGNIFICANTLY DIFFER... 
    # AMONG LEFT AND RIGHT PAIRED EYESTALKS.
    # COMBINE LEFT AND RIGHT PAIRED STRUCTURES MOVING FORWARD.

    
#********** RED KING CRAB LOCATION WITHIN ZYGOCARDIAC **********#
    
Zygo <- subset(Red.Crab, Structure=="zygocardiac")
  
# FOR MODEL COMPARISON, FIT BASIC GAM MODELS VIA 'gamm'
    
mod1<-gamm(Readable~s(Thin.Section.Location,k=4), family=binomial, data=Zygo)
    
mod2<-gamm(Readable~s(Thin.Section.Location, by=Animal.ID), family=binomial, data=Zygo, niterPQL=100)
# NOTE: WILL NOT CONVERGE
    
mod3<-gamm(Readable~s(Thin.Section.Location,k=4, by=Length), family=binomial, data=Zygo)
    
mod4<-gamm(Readable~s(Thin.Section.Location,k=4), family=binomial, random = list(Animal.ID = ~ 1), data=Zygo)
    
mod5<-gamm(Readable~s(Thin.Section.Location,k=4)+ Length, family=binomial, data=Zygo)
# FIXED EFFECT FOR LENGTH
    
mod6<-gamm(Readable~s(Thin.Section.Location,k=4, by=Animal.ID), family=binomial, random = list(Animal.ID = ~ 1), data=Zygo)
# NOTE: WILL NOT CONVERGE
    
mod7<-gamm(Readable~s(Thin.Section.Location,k=4, by=Length) + Length, family=binomial,  data=Zygo)
    
# MODEL COMPARISONS (Table 2 - Red King Crab):
AIC(mod1$lme, mod3$lme, mod4$lme, mod5$lme, mod7$lme)

  # AIC SUGGESTS THE SIMPLEST MODEL (COMBINE SMALL AND LARGE SIZE CLASSES)
    
    
  ##############
  ##          ##
  ## FIGURE 7 ##
  ##          ##
  ##############

# USE THE MODEL SUGGESTED BY AIC AND FIT AS GAM 
    
mod1<-gam(Readable~s(Thin.Section.Location,k=4), family=binomial, data=Zygo)
summary(mod1) 
    
Crab1<-with(Zygo, data.frame(Thin.Section.Location))
Crab1$P<- predict(mod1, newdata=Crab1, type="response")
    
Crab2<-with(Crab1, data.frame(Thin.Section.Location=rep(seq(from=0.00000000, to=1.00000000, length.out=290))))

Crab3<-cbind(Crab2, predict(mod1, newdata=Crab2, type="link", se=TRUE))
plot(Crab2)
Crab2<-within(Crab3, 
                  { PredictedProb<-plogis(fit) 
                  LL<- plogis(fit-1.96*se.fit) 
                  UL<-plogis(fit+(1.96*se.fit))})
    
ggplot(Crab2, aes(x=Thin.Section.Location, y=PredictedProb))+ 
      scale_y_continuous(labels = scales::percent_format(suffix = "", accuracy = 10 ))+
      scale_x_continuous(labels = scales::percent_format(suffix = ""), limits = c(0, 1.05))+
      geom_ribbon(aes(ymin=LL, ymax=UL), alpha= 0.2) +
      geom_line(size=1 )+ 
      guides(fill=FALSE)+
      theme(axis.line = element_line(colour = "black"),
            axis.text.x = element_text(family = "Times", colour="black", size = 20),
            axis.text.y = element_text(family = "Times", colour="black", size = 20),
            axis.title.y = element_text( margin = margin(t=0, r=10, b=0, l=0)),
            axis.title.x = element_text( margin = margin(t=10, r=0, b=0, l=0)),
            title = element_text( margin = margin(t=0, r=0, b=20, l=0)),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),  
            text = element_text(size=20),
            panel.background = element_blank()) +
      labs(title="Red King Crab Zygocardiac", x="Location (%)", y="Readable (%)")
    
    
#********** SNOW CRAB LOCATION WITHIN ZYGOCARDIAC **********#
    
Zygo <- subset(Snow.Crab, Structure=="zygocardiac")
    
# FOR MODEL COMPARISON, FIT BASIC GAM MODELS VIA 'gamm'
    
mod1<-gamm(Readable~s(Thin.Section.Location,k=4), family=binomial, data=Zygo)
    
mod2<-gamm(Readable~s(Thin.Section.Location, by=Animal.ID), family=binomial, data=Zygo, niterPQL=100)
# NOTE: WILL NOT CONVERGE
    
mod3<-gamm(Readable~s(Thin.Section.Location,k=4, by=Length), family=binomial, data=Zygo)
    
mod4<-gamm(Readable~s(Thin.Section.Location,k=4), family=binomial, random = list(Animal.ID = ~ 1), data=Zygo)
    
mod5<-gamm(Readable~s(Thin.Section.Location,k=4)+ Length, family=binomial, data=Zygo)
# FIXED EFFECT FOR LENGTH
    
mod6<-gamm(Readable~s(Thin.Section.Location,k=4, by=Animal.ID), family=binomial, random = list(Animal.ID = ~ 1), data=Zygo)
# NOTE: WILL NOT CONVERGE
    
mod7<-gamm(Readable~s(Thin.Section.Location,k=4, by=Length) + Length, family=binomial,  data=Zygo)
    
# MODEL COMPARISONS (Table 2 - Snow Crab):
AIC(mod1$lme, mod3$lme, mod4$lme, mod5$lme, mod7$lme)
    
    # AIC SUGGESTS THE SIMPLEST MODEL (COMBINE SMALL AND LARGE SIZE CLASSES)
    
    ########################################################
    ## 
    ## FIGURES 8 & 9 ----- NEED TO FIX - UPDATE MANUSCRIPT!!
    ## 
    ######################################################## 
    
    mod1<-gam(Readable~s(Thin.Section.Location,k=4), family=binomial, data=Zygo)
    summary(mod1) 
    
    Crab1<-with(Zygo, data.frame(Thin.Section.Location))
    Crab1$P<- predict(mod1, newdata=Crab1, type="response")
    
    Crab2<-with(Crab1, data.frame(Thin.Section.Location=rep(seq(from=0.00000000, to=1.00000000, length.out=290))))
    
    Crab3<-cbind(Crab2, predict(mod1, newdata=Crab2, type="link", se=TRUE))
    plot(Crab2)
    Crab2<-within(Crab3, 
                  { PredictedProb<-plogis(fit) 
                  LL<- plogis(fit-1.96*se.fit) 
                  UL<-plogis(fit+(1.96*se.fit))})
    
    ggplot(Crab2, aes(x=Thin.Section.Location, y=PredictedProb))+ 
      scale_y_continuous(labels = scales::percent_format(suffix = "", accuracy = 10 ))+
      scale_x_continuous(labels = scales::percent_format(suffix = ""), limits = c(0, 1.05))+
      geom_ribbon(aes(ymin=LL, ymax=UL), alpha= 0.2) +
      geom_line(size=1 )+ 
      guides(fill=FALSE)+
      theme(axis.line = element_line(colour = "black"),
            axis.text.x = element_text(family = "Times", colour="black", size = 20),
            axis.text.y = element_text(family = "Times", colour="black", size = 20),
            axis.title.y = element_text( margin = margin(t=0, r=10, b=0, l=0)),
            axis.title.x = element_text( margin = margin(t=10, r=0, b=0, l=0)),
            title = element_text( margin = margin(t=0, r=0, b=20, l=0)),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),  
            text = element_text(size=20),
            panel.background = element_blank()) +
      labs(title="Red King Crab Zygocardiac", x="Location (%)", y="Readable (%)")
 
    
###################################################
## 
## PART II:
## BAND COUNT
## 
########----------------########
######## IIA: PRECISION ########
########----------------########
    
rm(list=ls(all=T))
    
# LOAD LIBRARIES
library(FSA)
library(ggplot2)
library(ggpubr)
    
windowsFonts("Times"=windowsFont("TT Times New Roman"))
    
## LOAD DATA "PRECISION_TESTING.csv"
    
Precision <- read.csv(file=file.choose(), header=T, na.strings="")
names(Precision)

## EXCLUDE FIRST 5 SPECIMENS (WHICH WERE USED FOR CALIBRATION)

Precision <- subset(Precision, Animal.ID > 5)

# CALCULATE AVERAGE BAND COUNT EXCLUDING EACH READER

#AVERAGE.ALR
Precision$average.excluding1 <- round(rowMeans(subset(Precision, select = c(Reader.2, Reader.3, Reader.4)), na.rm=T), digits=0)
#AVERAGE.JCN
Precision$average.excluding2 <- round(rowMeans(subset(Precision, select = c(Reader.1, Reader.3, Reader.4)), na.rm=T), digits=0)
#AVERAGE.KWM
Precision$average.excluding3 <- round(rowMeans(subset(Precision, select = c(Reader.1, Reader.2, Reader.4)), na.rm=T), digits=0)
#AVERAGE.JBW
Precision$average.excluding4 <- round(rowMeans(subset(Precision, select = c(Reader.1, Reader.2, Reader.3)), na.rm=T), digits=0)  

## BREAK DATA INTO SUBSETS FOR EACH SPECIES

Red.Crab <- subset(Precision, Species == "red king crab")
Snow.Crab <- subset(Precision, Species == "snow crab")
Spot.Shrimp <- subset(Precision, Species == "spot shrimp")

  

#********** RED KING CRAB PRECISION **********#    

# AGE BIAS    

ab.tA1 <- ageBias(Reader.1 ~ average.excluding1, data=Red.Crab, ref.lab="AVE", nref.lab="Reader 1")
ab.tA2 <- ageBias(Reader.2 ~ average.excluding2, data=Red.Crab, ref.lab="AVE", nref.lab="Reader 2")
ab.tA3 <- ageBias(Reader.3 ~ average.excluding3, data=Red.Crab, ref.lab="AVE", nref.lab="Reader 3")
ab.tA4 <- ageBias(Reader.4 ~ average.excluding4, data=Red.Crab, ref.lab="AVE", nref.lab="Reader 4")

# SYMMETRY TEST RESULTS (Table 3 - Red King Crab) 
summary(ab.tA1,what="symmetry") # presented as Red king crab Reader 1 in Table 3
summary(ab.tA2,what="symmetry")	# presented as Red king crab Reader 2 in Table 3
summary(ab.tA3,what="symmetry")	# presented as Red king crab Reader 3 in Table 3
summary(ab.tA4,what="symmetry")	# presented as Red king crab Reader 4 in Table 3


### APE and CV by reader ####

# presented as Red King Crab Reader 1 in Figure 10 
# !!! NOTE: APE and CV references may change with version of FSA package !!!
ap.Reader1<-agePrecision(~Reader.1+average.excluding1, data=Red.Crab) 
Reader1.stats<-summary(ap.Reader1, what="precision")
# APE
Reader1.stats[1,8]
# CV
Reader1.stats[1,6]

# presented as Red King Crab Reader 2 in Figure 10
ap.Reader2<-agePrecision(~Reader.2+average.excluding2, data=Red.Crab) 
Reader2.stats<-summary(ap.Reader2, what="precision")
# APE
Reader2.stats[1,8]
# CV
Reader2.stats[1,6]

# presented as Red King Crab Reader 3 in Figure 10
ap.Reader3<-agePrecision(~Reader.3+average.excluding3, data=Red.Crab) 
Reader3.stats<-summary(ap.Reader3, what="precision")
# APE
Reader3.stats[1,8]
# CV
Reader3.stats[1,6]

# presented as Red King Crab Reader 4 in Figure 10
ap.Reader4<-agePrecision(~Reader.4+average.excluding4, data=Red.Crab) 
Reader4.stats<-summary(ap.Reader4, what="precision")
# APE
Reader4.stats[1,8]
# CV
Reader4.stats[1,6]

    
#********** SNOW CRAB PRECISION **********# 

# Repeat the process with snow crab

# AGE BIAS   

ab.tA5 <- ageBias(Reader.1 ~ average.excluding1, data=Snow.Crab, ref.lab="AVE", nref.lab="Reader 1")
ab.tA6 <- ageBias(Reader.2 ~ average.excluding2, data=Snow.Crab, ref.lab="AVE", nref.lab="Reader 2")
ab.tA7 <- ageBias(Reader.3 ~ average.excluding3, data=Snow.Crab, ref.lab="AVE", nref.lab="Reader 3")
ab.tA8 <- ageBias(Reader.4 ~ average.excluding4, data=Snow.Crab, ref.lab="AVE", nref.lab="Reader 4")

# SYMMETRY TEST RESULTS (Table 3 - Snow Crab) 

summary(ab.tA5,what="symmetry") # presented as Snow crab Reader 1 in Table 3
summary(ab.tA6,what="symmetry")	# presented as Snow crab Reader 2 in Table 3
summary(ab.tA7,what="symmetry")	# presented as Snow crab Reader 3 in Table 3
summary(ab.tA8,what="symmetry")	# presented as Snow crab Reader 4 in Table 3

### APE and CV by reader ####

# presented as Snow Crab Reader 1 in Figure 10 
# !!! NOTE: APE and CV references may change with version of FSA package !!!
ap.Reader5<-agePrecision(~Reader.1+average.excluding1, data=Snow.Crab) 
Reader5.stats<-summary(ap.Reader5, what="precision")
# APE
Reader5.stats[1,8]
# CV
Reader5.stats[1,6]

# presented as Snow Crab Reader 2 in Figure 10
ap.Reader6<-agePrecision(~Reader.2+average.excluding2, data=Snow.Crab) 
Reader6.stats<-summary(ap.Reader6, what="precision")
# APE
Reader6.stats[1,8]
# CV
Reader6.stats[1,6]

# presented as Snow Crab Reader 3 in Figure 10
ap.Reader7<-agePrecision(~Reader.3+average.excluding3, data=Snow.Crab) 
Reader7.stats<-summary(ap.Reader7, what="precision")
# APE
Reader7.stats[1,8]
# CV
Reader7.stats[1,6]

# presented as Snow Crab Reader 4 in Figure 10
ap.Reader8<-agePrecision(~Reader.4+average.excluding4, data=Snow.Crab) 
Reader8.stats<-summary(ap.Reader8, what="precision")
# APE
Reader8.stats[1,8]
# CV
Reader8.stats[1,6]


    ######################
    ##                  ##
    ##    FIGURE 10     ##
    ##                  ##
    ######################  


par(mfrow=c(4,2), family="Times", omi=c(0.2,0.2,0.5,0))

# !!! NOTE: APE and CV references may change with version of FSA package !!!

par(mai=c(0.2,0.5,0.1,0))
plotAB(ab.tA1,what="bias",col.agree="gray50", xlim=c(0,19), ylim=c(0,19), xlab= "", ylab="")
title(ylab="Reader 1", line=2, cex.lab=1.4, family="Times")
legend("topleft", bty="n", border=NULL, paste("APE",round(Reader1.stats[1,8],1), "\n", "CV", round(Reader1.stats[1,6], 1))) 


par(mai=c(0.2,0.3,0.1,0.1))
plotAB(ab.tA5,what="bias",col.agree="gray50", xlim=c(0,19), ylim=c(0,19), xlab= "", ylab="")
legend("topleft", bty="n", border=NULL, paste("APE", round(Reader5.stats[1,8], 1), "\n", "CV", round(Reader5.stats[1,6], 1))) 

par(mai=c(0.2,0.5,0.1,0))
plotAB(ab.tA2,what="bias",col.agree="gray50", xlim=c(0,19), ylim=c(0,19), xlab= "", ylab=" ")
title(ylab="Reader 2", line=2, cex.lab=1.4, family="Times")
legend("topleft",bty="n",paste("APE", round(Reader2.stats[1,8], 1), "\n", "CV", round(Reader2.stats[1,6], 1))) 

par(mai=c(0.2,0.3,0.1,0.1))
plotAB(ab.tA6,what="bias",col.agree="gray50", xlim=c(0,19), ylim=c(0,19), xlab= "", ylab="")
legend("topleft",bty="n",paste("APE",round(Reader6.stats[1,8], 1), "\n", "CV", round(Reader6.stats[1,6], 1))) 

par(mai=c(0.2,0.5,0.1,0))
plotAB(ab.tA3,what="bias",col.agree="gray50", xlim=c(0,19), ylim=c(0,19), xlab= "", ylab="")
title(ylab="Reader 3", line=2, cex.lab=1.4, family="Times")
legend("topleft", bty="n", paste("APE", round(Reader3.stats[1,8], 1), "\n", "CV", round(Reader3.stats[1,6], 1))) 

par(mai=c(0.2,0.3,0.1,0.1))
plotAB(ab.tA7,what="bias",col.agree="gray50", xlim=c(0,19), ylim=c(0,19), xlab= "Average",ylab="")
legend("topleft", bty="n", paste("APE",round(Reader7.stats[1,8], 1), "\n", "CV", round(Reader7.stats[1,6], 1))) 

par(mai=c(0.4,0.5,0.1,0))
plotAB(ab.tA4,what="bias",col.agree="gray50", xlim=c(0,19), ylim=c(0,19), xlab= "Average" , ylab="")
title(ylab="Reader 4", line=2, cex.lab=1.4, family="Times")
legend("topleft", bty="n", paste("APE", round(Reader4.stats[1,8], 1), "\n", "CV", round(Reader4.stats[1,6], 1))) 

par(mai=c(0.4,0.3,0.1,0.1))
plotAB(ab.tA8,what="bias",col.agree="gray50", xlim=c(0,19), ylim=c(0,19), xlab= "Average" , ylab="")
legend("topleft", bty="n", paste("APE",round(Reader8.stats[1,8], 1), "\n", "CV", round(Reader8.stats[1,6], 1))) 

mtext("Band Count Average Excluding Reader", side=1, outer=T, at=0.5)

mtext("Red King Crab", side = 3, line = -0.5, adj=0.25, outer = TRUE)
mtext("Snow Crab", side = 3, line = -0.5, adj=0.80, outer = TRUE)

#********** SPOT SHRIMP PRECISION **********#

# AGE BIAS  

ab.tA9 <- ageBias(Reader.1 ~ average.excluding1, data=Spot.Shrimp, ref.lab="AVE", nref.lab="Reader 1")
ab.tA10 <- ageBias(Reader.2 ~ average.excluding2, data=Spot.Shrimp, ref.lab="AVE", nref.lab="Reader 2")
ab.tA11 <- ageBias(Reader.3 ~ average.excluding3, data=Spot.Shrimp, ref.lab="AVE", nref.lab="Reader 3")
ab.tA12 <- ageBias(Reader.4 ~ average.excluding4, data=Spot.Shrimp, ref.lab="AVE", nref.lab="Reader 4")

# SYMMETRY TEST RESULTS (Table 3 - Spot Shrimp) 
summary(ab.tA9,what="symmetry")   # presented as Spot shrimp Reader 1 in Table 3
summary(ab.tA10,what="symmetry")	# presented as Spot shrimp Reader 2 in Table 3
summary(ab.tA11,what="symmetry")	# presented as Spot shrimp Reader 3 in Table 3
summary(ab.tA12,what="symmetry")	# presented as Spot shrimp Reader 4 in Table 3


### APE and CV by reader ####

# presented as Spot Shrimp Reader 1 in Figure 11 
ap.Reader9<-agePrecision(~Reader.1+average.excluding1, data=Spot.Shrimp) 
Reader9.stats<-summary(ap.Reader9, what="precision")
# APE
Reader9.stats[1,8]
# CV
Reader9.stats[1,6]

# presented as Spot Shrimp Reader 2 in Figure 11
ap.Reader10<-agePrecision(~Reader.2+average.excluding2, data=Spot.Shrimp) 
Reader10.stats<-summary(ap.Reader10, what="precision")
# APE
Reader10.stats[1,8]
# CV
Reader10.stats[1,6]

# presented as Spot Shrimp Reader 3 in Figure 11
ap.Reader11<-agePrecision(~Reader.3+average.excluding3, data=Spot.Shrimp) 
Reader11.stats<-summary(ap.Reader11, what="precision")
# APE
Reader11.stats[1,8]
# CV
Reader11.stats[1,6]

# presented as Spot Shrimp Reader 4 in Figure 11
ap.Reader12<-agePrecision(~Reader.4+average.excluding4, data=Spot.Shrimp) 
Reader12.stats<-summary(ap.Reader12, what="precision")
# APE
Reader12.stats[1,8]
# CV
Reader12.stats[1,6]

    ######################
    ##                  ##
    ##    FIGURE 11     ##
    ##                  ##
    ######################

par(mfrow=c(2,2), family="Times", omi=c(0.2,0.2,0.5,0.2))

par(mai=c(0.35,0.5,0.05,0))
plotAB(ab.tA9,what="bias",col.agree="gray50", xlim=c(1,9), ylim=c(1,11), xlab= "", ylab="")
title(ylab="Reader 1", line=2, cex.lab=1.4, family="Times")
legend("topleft", bty="n", border=NULL, paste("APE",round(Reader9.stats[1,8],1), "\n", "CV", round(Reader9.stats[1,6], 1))) 

par(mai=c(0.35,0.55,0.05,0))
plotAB(ab.tA10,what="bias",col.agree="gray50", xlim=c(1,9), ylim=c(1,11), xlab= "", ylab=" ")
title(ylab="Reader 2", line=2, cex.lab=1.4, family="Times")
legend("topleft",bty="n",paste("APE", round(Reader10.stats[1,8], 1), "\n", "CV", round(Reader10.stats[1,6], 1)))  

par(mai=c(0.35,0.5,0.05,0))
plotAB(ab.tA11,what="bias",col.agree="gray50", xlim=c(1,9), ylim=c(1,11), xlab= "", ylab="")
title(ylab="Reader 3", line=2, cex.lab=1.4, family="Times")
legend("topleft", bty="n", border=NULL, paste("APE",round(Reader11.stats[1,8],1), "\n", "CV", round(Reader11.stats[1,6], 1)))  

par(mai=c(0.35,0.55,0.05,0))
plotAB(ab.tA12,what="bias",col.agree="gray50", xlim=c(1,9), ylim=c(1,11), xlab= "", ylab=" ")
title(ylab="Reader 4", line=2, cex.lab=1.4, family="Times")
legend("topleft",bty="n",paste("APE", round(Reader12.stats[1,8], 1), "\n", "CV", round(Reader12.stats[1,6], 1))) 

mtext("Band Count Average Excluding Reader", side=1, outer=T, at=0.55)

mtext("Spot Shrimp", side = 3, line = 0, outer = TRUE, at=0.55)

##############################
    
########--------------------------------########
######## IIB: BAND COUNT AND SIZE CLASS ########
#######---------------------------------########  

rm(list=ls(all=T))

# LOAD LIBRARIES
library(dplyr)
library(ggplot2)
library(mgcv)
library(ggpubr)

windowsFonts("Times"=windowsFont("TT Times New Roman"))

## LOAD DATA "Final_Band_Count.csv"

BandCount <- read.csv(file=file.choose(), header=T, na.strings="")
names(BandCount)

## BREAK DATA INTO SUBSETS FOR EACH SPECIES

Red.Crab<- subset(BandCount, Species=="red king crab")
Snow.Crab<- subset(BandCount, Species=="snow crab")
Spot.Shrimp<- subset(BandCount, Species=="spot shrimp")

#********** RED KING CRAB COUNT AND SIZE **********#    

rc.plot<- ggplot(Red.Crab, aes(x=Animal.Size, y=Final.Band.Count, size=15))+
  geom_point(colour = "black", size = 2, show.legend = FALSE)+
  guides(fill=FALSE)+
  scale_x_continuous(limits = c(60, 160))+
  scale_y_continuous(breaks=c(5,7,9,11, 13, 15, 17, 19),limits = c(5, 19))+
  theme(axis.line = element_line(colour = "black"),
        text = element_text(size=16, family="Times"),
        axis.text.x = element_text(colour="black"),
        axis.text.y = element_text(colour="black"),
        plot.title = element_text(hjust=0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  labs(title="Red King Crab",  size=12, x="Carapace Lenth (mm)", y="Band Count")

# BREAK CARAPACE SIZE INTO LARGE AND SMALL SIZE CLASSES
Red.Crab$Length<-as.factor(Red.Crab$Animal.Size > 80)
# Large = TRUE and Small = FALSE

# T TEST
with(Red.Crab, t.test(Final.Band.Count~Length))

# ANOVA
Red.mod<-with(Red.Crab, lm(Animal.Size~Final.Band.Count))
summary(Red.mod)
anova(Red.mod)


#********** SNOW CRAB COUNT AND SIZE **********# 

sc.plot<- ggplot(Snow.Crab, aes(x=Animal.Size, y=Final.Band.Count, size=15))+
  geom_point(colour = "black", size = 2, show.legend = FALSE)+
  guides(fill=FALSE)+
  scale_x_continuous(limits = c(40, 100))+
  theme(axis.line = element_line(colour = "black"),
        text = element_text(size=16, family="Times"),
        axis.text.x = element_text(colour="black"),
        axis.text.y = element_text(colour="black"),
        plot.title = element_text(hjust=0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  scale_y_continuous(breaks=c(3,5,7,9,11 ),limits = c(3, 12))+
  labs(title="Snow Crab", size=12, x="Carapace Width (mm)", y="Band Count") 

# BREAK CARAPACE SIZE INTO LARGE AND SMALL SIZE CLASSES
Snow.Crab$Length<-as.factor(Snow.Crab$Animal.Size > 65)
# Large = TRUE and Small = FALSE

# T TEST
with(Snow.Crab, t.test(Final.Band.Count~Length))

# ANOVA
Snow.mod<-with(Snow.Crab, lm(Animal.Size~Final.Band.Count))
summary(Snow.mod)
anova(Snow.mod)
    
#********** SPOT SHRIMP COUNT AND SIZE **********# 
    
ss.plot<- ggplot(Spot.Shrimp, aes(x=Animal.Size, y=Final.Band.Count, size=15))+
  geom_point(colour = "black", size = 2, show.legend = FALSE)+
  guides(fill=FALSE)+
  scale_x_continuous(limits = c(20, 60))+
  scale_y_continuous(breaks=c(3,4,5,6, 7, 8, 9),limits = c(2.5, 9))+
  theme(axis.line = element_line(colour = "black"),
        text = element_text(size=16, family="Times"),
        axis.text.x = element_text(colour="black"),
        axis.text.y = element_text(colour="black"),
        plot.title = element_text(hjust=0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  labs(title="Spot Shrimp", size=12, x="Carapace Length (mm)", y="Band Count") 


# T TEST
Spot.Shrimp$Length<-as.factor(Spot.Shrimp$Animal.Size>30)
with(Spot.Shrimp, t.test(Final.Band.Count~Length))

# ANOVA
Shrimp.mod<-with(Spot.Shrimp, lm(Animal.Size~Final.Band.Count))
summary(Shrimp.mod)
anova(Shrimp.mod)

    ######################
    ##                  ##
    ##    FIGURE 12     ##
    ##                  ##
    ###################### 

ggarrange(rc.plot, sc.plot, ss.plot,
          ncol = 2, nrow = 2) 
