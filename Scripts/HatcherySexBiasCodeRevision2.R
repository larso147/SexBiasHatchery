#Authors: Doug Larson, Joe Riedy, Gabrielle Safilippo, Kim Scribner
#Date: 4/6/2022 
#Title: Multi-year evidence of unbiased sex ratios in hatchery and
#wild-reared juvenile lake sturgeon (Acipenser fulvescens)

library(readr)
library(tidyverse)
library(gridExtra)
library(grid)
library(gtable)
library(cowplot)
library(pwr)


#####2016 ChiSquare test of Homogeneity - Is there domestication selection in the hatchery?###
#Build Table#
Table2016=matrix(c(25,16,20,21),ncol=2)
colnames(Table2016)=c("Alive","Dead")
rownames(Table2016)=c("Males","Females")
Table2016

#ChiSquarePowerTest
pwr.chisq.test(df = 1, w=0.3, N=82, sig.level = 0.05, power = NULL)

#ChiSquareExpected
chisq.test(Table2016)$expected

#ChiSqareTest#
chisq.test(Table2016) #Note - did not use in the final analysis. 

#FishersExact
fisher.test(Table2016)

#Mosaicplot
mosaicplot(Table2016,
           main = "Mosaic Plot 2016",
           color = TRUE
)

#####2017 ChiSquare test of Homogeneity - Is there domestication selection in the hatchery?###
#Build Table#
Table2017=matrix(c(103,88,101,90),ncol=2)
colnames(Table2017)=c("Alive","Dead")
rownames(Table2017)=c("Males","Females")
Table2017

#ChiSquarePowerTest
pwr.chisq.test(df = 1, w=0.3, N=382, sig.level = 0.05, power = NULL)

#ChiSquareExpected
chisq.test(Table2017)$expected

#ChiSqareTest#
chisq.test(Table2017)

#Mosaicplot
mosaicplot(Table2017,
           main = "Mosaic Plot 2017",
           color = TRUE
)

#####2018 ChiSquare test of Homogeneity - Is there domestication selection in the hatchery?###
#Build Table#
Table2018=matrix(c(252,220,225,247),ncol=2)
colnames(Table2018)=c("Alive","Dead")
rownames(Table2018)=c("Males","Females")
Table2018

#ChiSquarePowerTest
pwr.chisq.test(df = 1, w=0.3, N=994, sig.level = 0.05, power = NULL)

#ChiSquareExpected
chisq.test(Table2018)$expected

#ChiSquareTest#
chisq.test(Table2018)

#Mosaicplot
mosaicplot(Table2018,
           main = "Mosaic Plot 2018",
           color = TRUE
)

#####################Juvenile Data############################################################

#####ChiSquare test of Homogeneity - Does Sex Ratio Differ across years?######################
###Build Table###

JuvTable=matrix(c(3,8,16,24),ncol=2)
colnames(JuvTable)=c("2017","2018")
rownames(JuvTable)=c("Males","Females")

#Fisher's Exact Test
fisher.test(JuvTable)

#Power analysis
pwr.chisq.test(df = 1, w=0.3, N=51, sig.level = 0.05, power = NULL)

#Mosaicplot
mosaicplot(JuvTable,
           main = "Mosaic Plot of Wild Captured Age-0 Fish",
           color = TRUE
)

###Build Vector / ChiSquareTest assuming equal sex ratio 2017##################################
chisq.test(c(3,8), p=c(0.5,0.5)) #Could not use - small sample size, no non-parametric analog. 

pwr.chisq.test(df = 1, w=0.5, N=11, sig.level = 0.05, power = NULL)


###Build Vector / ChiSquareTest assuming equal sex ratio 2018##################################
chisq.test(c(16,24), p=c(0.5,0.5)) #Could not use - small sample size, no non-parametric analog. 

pwr.chisq.test(df = 1, w=0.5, N=40, sig.level = 0.05, power = NULL)


#######################Figure 2 - Distribution of Larvae By Night##############################

DriftbyYear <- read_csv("data/DriftbyYear1.csv")

Dates1<- c("5/17", "5/18", "5/19", "5/20", "5/21", "5/22", "5/23", "5/24", "5/25", "5/26", "5/27", "5/28", "5/29", 
          "5/30", "5/31", "6/1", "6/2", "6/3", "6/4", "6/5", "6/6", "6/7", "6/8", "6/9", "6/10", "6/11", "6/12",
          "6/13", "6/14", "6/15", "6/16", "6/17", "6/18", "6/19", "6/20", "6/21", "6/22", "6/23")

#png(file="DriftbyYearRevision.png", width = 1200, height = 800)

p<-ggplot(data=na.omit(DriftbyYear),mapping = aes(x=Order, y=Nsturgeon2016)) +
  geom_bar(stat = "identity")+
  geom_line(mapping = aes(x=Order, y=AveTemp2016/0.05), size=0.75, linetype = 2)+
     theme_classic() + 
     theme(axis.text.x=element_blank())+ 
     theme(axis.ticks.x = element_blank())+
     theme(axis.text.y = element_text(size=20),axis.title.y=element_text(size=20)) +
     xlab(NULL)+
     ylab(NULL)+
  scale_y_continuous("", sec.axis = sec_axis(~ . * 0.05, name = NULL))+
  annotate(geom="text", 
           x=37, 
           y=700, 
           label = "2016", 
           color = "black", 
           size=10);

q<-ggplot(data=na.omit(DriftbyYear),mapping = aes(x=Order, y=NSturgeon2017)) +
  geom_bar(stat = "identity")+
  geom_line(mapping = aes(x=Order, y=AveTemp2017/0.006),  size=0.75, linetype = 2)+
     theme_classic() + 
     theme(axis.text.x=element_blank())+
     theme(axis.ticks.x = element_blank())+
     theme(axis.text.y = element_text(size=20), axis.title.y=element_text(size=20))+
     xlab(NULL)+
     ylab(NULL)+
  scale_y_continuous("", sec.axis = sec_axis(~ . * 0.006, name = NULL))+
  annotate(geom="text", 
           x=37, 
           y=5000, 
           label = "2017",  
           color = "black",  
           size=10)

r<-ggplot(data=na.omit(DriftbyYear),mapping = aes(x=Order, y=NSturgeon2018))+
   geom_bar(stat = "identity")+
  geom_line(mapping = aes(x=Order, y=AveTemp2018/0.002), size=0.75, linetype = 2)+
      theme_classic()+ 
      theme(axis.text.x = element_text(angle = 90, size=20))+
      theme(axis.text.y = element_text(size=20), axis.title.y=element_text(size=20))+
      xlab(NULL)+
      ylab(NULL)+
   scale_y_continuous("", sec.axis = sec_axis(~ . * 0.002, name = NULL))+
   annotate(geom="text", 
           x=37,
           y=16000, 
           label = "2018",  
           color = "black",  
           size=10)+
   scale_x_continuous(breaks=c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 
                        18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 
                        33, 34, 35, 36, 37, 38),
                       labels=c("5/17", "5/18", "5/19", "5/20", "5/21", "5/22", "5/23", "5/24", 
                       "5/25", "5/26", "5/27", "5/28", "5/29",  "5/30", "5/31", "6/1",
                        "6/2", "6/3", "6/4", "6/5", "6/6", "6/7", "6/8", "6/9", "6/10", 
                       "6/11", "6/12","6/13", "6/14", "6/15", "6/16", "6/17", "6/18", 
                       "6/19", "6/20", "6/21", "6/22", "6/23"))

Plot1<-cowplot::plot_grid(p,q,r, ncol = 1, align = "v")

y.grob <-  textGrob("Number of Sturgeon Collected", gp=gpar(fontface="bold", fontsize=20), vjust = 2, rot=90)
y.grob2 <- textGrob("Temp °C", gp=gpar(fontface="bold", fontsize=20), vjust = -0.01, rot=90)
x.grob <-  textGrob("Date", gp=gpar(fontface="bold", fontsize=20), vjust = -0.25)

grid.arrange(arrangeGrob(Plot1, left = y.grob, bottom = x.grob, right = y.grob2))

#dev.off()

#######################Comparison of Juvenile Sizes by Sex##############################


SW2017 = read_csv("data/SW2017.csv")
SW2018 = read_csv("data/SW2018.csv")
SW20172018 = read_csv("data/SW20172018.csv")


TLX2017 = wilcox.test(TL ~ Sex, data=SW2017)
TLX2017
WX2017 = wilcox.test(W ~ Sex, data=SW2017)
WX2017

TLx2018 = wilcox.test(TL ~ Sex, data=SW2018)
TLx2018
WX2018 = wilcox.test(W ~ Sex, data=SW2018)
WX2018

TLX20172018 = wilcox.test(TL ~ Sex, data=SW20172018)
TLX20172018
WX20172018 = wilcox.test(W ~ Sex, data=SW20172018)
WX20172018

