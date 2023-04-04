##Install necessary packages
install.packages("rio")
install.packages("stringi")
install.packages ("tidyverse")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("ggbeeswarm")
install.packages("qdap")
install.packages(("EnvStats"))
install.packages("ggpubr")
install.packages("hrbrthemes")
install.packages("viridis")
install.packages("htmltools")
install.packages("treemapify")


##Load necessary packages
library(plyr)
library(hrbrthemes)
library(tidyr)
library(viridis)
library (stringi)
library(stringr)
library(rio) 
library(readxl)
library (tidyr)
library (dplyr)
library(tidyverse)
library(ggplot2)
library(ggbeeswarm)
library(qdap)
library(EnvStats)
library(ggpubr)
library(treemapify)




SimpleSummary$Condition <- factor(SimpleSummary$Condition, levels = c("mgm1", "mdm37", "mdm39", "num1", "WT", "mdm34","mdm10", "fis1","mdm36" ))

SimpleSummary$Condition <- factor(SimpleSummary$Condition, levels = c('Fermentation', 'Transition', 'Respiration'))

#####T-TESTS####

MyWTComparisons <- list( c("Fermentation", "Respiration"), c("Fermentation", "Transition"), c("Respiration", "Transition") )
MyKOComparisons <- list( c("FermentationKO", "Fermentation"), c("TransitionKO", "Transition"), c("RespirationKO", "Respiration") )
MyOnlyKOComparisons <- list( c("FermentationKO", "RespirationKO"), c("FermentationKO", "TransitionKO"), c("RespirationKO", "TransitionKO") )


#WTOnlyPlots#
##Figure 3##
ggplot(SimpleSummary, aes(x=Condition, y=as.numeric(SimpleSummary$Volume.Mitochondria), fill=Condition)) +
  geom_violin(alpha = 0.75) +
  geom_boxplot(width=0.3, color="black", alpha=1) +
  geom_beeswarm(alpha = 0.2) +
  xlab("Conditions") +
  ylab (bquote('Mitochondrial Volume '(µm^3)))+
  stat_compare_means(label = "p.signif",comparisons = MyWTComparisons, size=5)+
  scale_fill_manual(values=c("#d44842", "#fac228", "#365c8d"))+ theme_linedraw()+ theme(legend.position="none")+
  theme( plot.title=element_text(size=22),
         axis.text=element_text(size=18),
         axis.title=element_text(size=22))

ggplot(SimpleSummary, aes(x=Condition, y=as.numeric(SimpleSummary$NormalizedMitoVol), fill=Condition)) +
  geom_violin(alpha = 0.75) +
  geom_boxplot(width=0.3, color="black", alpha=1) +
  geom_beeswarm(alpha = 0.2) +
  xlab("Conditions") +
  ylab ('Mitochondrial Volume Normalized to Cell Volume')+
  stat_compare_means(label = "p.signif",comparisons = MyWTComparisons, size=5)+
  scale_fill_manual(values=c("#d44842", "#fac228", "#365c8d"))+ theme_linedraw()+ theme(legend.position="none")+
  theme( plot.title=element_text(size=22),
         axis.text=element_text(size=18),
         axis.title=element_text(size=22))

ggplot(SimpleSummary, aes(x=Volume.Cell, y=Volume.Mitochondria, shape=Condition, color=Condition)) + 
  geom_point(size= 5) +
  xlab(bquote('Cell Volume '(µm^3)))+ ylab (bquote('Mitochondrial Volume '(µm^3)))+
  geom_smooth(method = "lm", se = FALSE)+
  scale_color_manual(values=c("#d44842", "#fac228", "#365c8d", "#ae0001", "#ffe600","#6497b1" ))+
  theme_linedraw(base_size = 22)


#Figure 4#

ggplot(SimpleSummary, aes(x=Condition, y=as.numeric(SimpleSummary$SurfaceArea.Mitochondria), fill=Condition)) +
  geom_violin(alpha = 0.75) +
  geom_boxplot(width=0.3, color="black", alpha=1) +
  geom_beeswarm(alpha = 0.2) +
  xlab("Conditions") +
  ylab (bquote('Mitochondrial Surface Area '(µm^2)))+
  stat_compare_means(label = "p.signif",comparisons = MyWTComparisons, size=5)+
  scale_fill_manual(values=c("#d44842", "#fac228", "#365c8d"))+ theme_linedraw()+ theme(legend.position="none")+
  theme( plot.title=element_text(size=22),
         axis.text=element_text(size=18),
         axis.title=element_text(size=22))


ggplot(SimpleSummary, aes(x=Condition, y=as.numeric(SimpleSummary$HalfOverlapSA), fill=Condition)) +
  geom_violin(alpha = 0.75) +
  geom_boxplot(width=0.3, color="black", alpha=1) +
  geom_beeswarm(alpha = 0.2) +
  xlab("Conditions") +
  ylab (bquote('Total Mitochondria-ER Contact Area '(µm^2)))+
  stat_compare_means(label = "p.signif",comparisons = MyWTComparisons, size=5)+
  scale_fill_manual(values=c("#d44842", "#fac228", "#365c8d"))+ theme_linedraw()+ theme(legend.position="none")+
  theme( plot.title=element_text(size=22),
         axis.text=element_text(size=18),
         axis.title=element_text(size=22))


ggplot(SimpleSummary, aes(x=Condition, y=as.numeric(SimpleSummary$MitoOverlap), fill=Condition)) +
  geom_violin(alpha = 0.75) +
  geom_boxplot(width=0.3, color="black", alpha=1) +
  geom_beeswarm(alpha = 0.2) +
  xlab("Conditions") +
  ylab (bquote('% of Mitochondrial Surface Contacting ER'))+
  stat_compare_means(label = "p.signif",comparisons = MyWTComparisons, size=5)+
  scale_fill_manual(values=c("#d44842", "#fac228", "#365c8d"))+ theme_linedraw()+ theme(legend.position="none")+
  theme( plot.title=element_text(size=22),
         axis.text=element_text(size=18),
         axis.title=element_text(size=22))


PTotalContacts <- ggplot(SimpleSummary, aes(x=Condition, y=as.numeric(TotalContacts),  fill=Condition)) +
  geom_boxplot() +
  xlab("Conditions") +
  ylab("Number of Contacts")+
  ggtitle("Total Mitochondrial-ER Contacts")+
  stat_compare_means(label = "p.signif",comparisons = MyWTComparisons)+
  scale_fill_manual(values=c("#d44842", "#fac228", "#365c8d"))+ theme_linedraw()+ theme(legend.position="none")+
  theme( plot.title=element_text(size=18,face="bold"),
         axis.text=element_text(size=18),
         axis.title=element_text(size=18,face="bold"))

PFirstCutOffContacts <- ggplot(SimpleSummary, aes(x=Condition, y=as.numeric(FirstCutOffContacts),  fill=Condition)) +
  geom_boxplot() +
  xlab("Conditions") +
  ylab("Number of Contacts")+
  ggtitle(paste("Mitochondrial-ER Contacts Over", FirstCutOff, "μm²"))+
  stat_compare_means(label = "p.signif",comparisons = MyWTComparisons)+
  scale_fill_manual(values=c("#d44842", "#fac228", "#365c8d"))+ theme_linedraw()+ theme(legend.position="none")+
  theme( plot.title=element_text(size=18,face="bold"),
         axis.text=element_text(size=18),
         axis.title=element_text(size=18,face="bold"))

PSecondCutOffContacts <-ggplot(SimpleSummary, aes(x=Condition, y=as.numeric(SecondCutOffContacts),  fill=Condition)) +
  geom_boxplot() +
  xlab("Conditions") +
  ylab("Number of Contacts")+
  ggtitle(paste("Mitochondrial-ER Contacts Over", SecondCutOff,  "μm²"))+
  stat_compare_means(label = "p.signif",comparisons = MyWTComparisons)+
  scale_fill_manual(values=c("#d44842", "#fac228", "#365c8d"))+ theme_linedraw()+ theme(legend.position="none")+
  theme( plot.title=element_text(size=18,face="bold"),
         axis.text=element_text(size=16),
         axis.title=element_text(size=18,face="bold"))

PThirdCutOffContacts <-ggplot(SimpleSummary, aes(x=Condition, y=as.numeric(ThirdCutOffContacts),  fill=Condition)) +
  geom_boxplot() +
  xlab("Conditions") +
  ylab("Number of Contacts")+
  ggtitle(paste("Mitochondrial-ER Contacts Over", ThirdCutOff, "μm²"))+
  stat_compare_means(label = "p.signif",comparisons = MyWTComparisons)+
  scale_fill_manual(values=c("#d44842", "#fac228", "#365c8d"))+ theme_linedraw()+ theme(legend.position="none")+
  theme( plot.title=element_text(size=18,face="bold"),
         axis.text=element_text(size=16),
         axis.title=element_text(size=18,face="bold"))

ggarrange(PTotalContacts, PFirstCutOffContacts, PSecondCutOffContacts,PThirdCutOffContacts,
          labels = c("I", "II", "III", "IV"),
          ncol = 2, nrow = 2)

ggarrange(PTotalContacts, PFirstCutOffContacts,
          labels = c("I", "II"),
          ncol = 2, nrow = 1)



ggplot(SimpleSummary, aes(x=Condition, y=as.numeric(SimpleSummary$ContactsNormToMitoSA), fill=Condition)) +
  geom_violin(alpha = 0.75) +
  geom_boxplot(width=0.3, color="black", alpha=1) +
  geom_beeswarm(alpha = 0.2) +
  xlab("Conditions") +
  ylab (bquote('Mitochondria-ER Contacts/ Mitochondrial Area (μm-²)'))+
  stat_compare_means(label = "p.signif",comparisons = MyWTComparisons, size=5)+
  scale_fill_manual(values=c("#d44842", "#fac228", "#365c8d"))+ theme_linedraw()+ theme(legend.position="none")+
  theme( plot.title=element_text(size=22),
         axis.text=element_text(size=18),
         axis.title=element_text(size=22))

#Figure 5#

ggplot(SimpleSummary, aes(x=Condition, y=as.numeric(SimpleSummary$NormPeriphDistMito), fill=Condition)) +
  geom_violin(alpha = 0.75) +
  geom_boxplot(width=0.3, color="black", alpha=1) +
  geom_beeswarm(alpha = 0.2) +
  xlab("Conditions") +
  ylab("Peripheral Mitochondrial Concentration")+
  stat_compare_means(label = "p.signif",comparisons = MyWTComparisons)+
  scale_fill_manual(values=c("#d44842", "#fac228", "#365c8d"))+ theme_linedraw()+ theme(legend.position="none")+
  theme( plot.title=element_text(size=22),
         axis.text=element_text(size=18),
         axis.title=element_text(size=22))

ggplot(SimpleSummary, aes(x=Condition, y=as.numeric(SimpleSummary$PeripherySum), fill=Condition)) +
  geom_violin(alpha = 0.75) +
  geom_boxplot(width=0.3, color="black", alpha=1) +
  geom_beeswarm(alpha = 0.2) +
  xlab("Conditions") +
  ylab("Peripheral Mitochondrial Volume")+
  stat_compare_means(label = "p.signif",comparisons = MyWTComparisons)+
  scale_fill_manual(values=c("#d44842", "#fac228", "#365c8d"))+ theme_linedraw()+ theme(legend.position="none")+
  theme( plot.title=element_text(size=22),
         axis.text=element_text(size=18),
         axis.title=element_text(size=22))

ggplot(SimpleSummary, aes(x=Condition, y=as.numeric(SimpleSummary$AsymmetryMetric), fill=Condition)) +
  geom_violin(alpha = 0.75) +
  geom_boxplot(width=0.3, color="black", alpha=1) +
  geom_beeswarm(alpha = 0.2) +
  xlab("Conditions") +
  ylab("Mitochondrial Asymmetry")+
  stat_compare_means(label = "p.signif",comparisons = MyWTComparisons)+
  scale_fill_manual(values=c("#d44842", "#fac228", "#365c8d"))+ theme_linedraw()+ theme(legend.position="none")+
  theme( plot.title=element_text(size=22),
         axis.text=element_text(size=18),
         axis.title=element_text(size=22))


ggplot(SimpleSummary, aes(x=Condition, y=as.numeric(SimpleSummary$SurfaceArea.SubcellularStructure/SimpleSummary$Volume.SubcellularStructure/SimpleSummary$TotalMitoComponents), fill=Condition)) +
  geom_violin(alpha = 0.75) +
  geom_boxplot(width=0.3, color="black", alpha=1) +
  geom_beeswarm(alpha = 0.2) +
  xlab("Conditions") +
  ylab("Peripheral Mitochondrial Concentration")+
  #stat_compare_means(label = "p.signif",comparisons = MyWTComparisons)+
  #scale_fill_manual(values=c("#d44842", "#fac228", "#365c8d"))+ 
  theme_linedraw()+ theme(legend.position="none")+
  theme( plot.title=element_text(size=22),
         axis.text=element_text(size=18),
         axis.title=element_text(size=22))


ggplot(SimpleSummary, aes(x=Condition, y=as.numeric(SimpleSummary$AsymmetryMetric), fill=Condition)) +
  geom_violin(alpha = 0.75) +
  geom_boxplot(width=0.3, color="black", alpha=1) +
  geom_beeswarm(alpha = 0.2) +
  xlab("Conditions") +
  ylab("Mitochondrial Asymmetry")+
  #stat_compare_means(label = "p.signif",comparisons = MyWTComparisons)+
  #scale_fill_manual(values=c("#d44842", "#fac228", "#365c8d"))+ 
  theme_linedraw()+ theme(legend.position="none")+
  theme( plot.title=element_text(size=22),
         axis.text=element_text(size=18),
         axis.title=element_text(size=22))

ggplot(SimpleSummary, aes(x=Condition, y=as.numeric(), fill=Condition)) +
  geom_violin(alpha = 0.75) +
  geom_boxplot(width=0.3, color="black", alpha=1) +
  geom_beeswarm(alpha = 0.2) +
  xlab("Conditions") +
  ylab("Mitochondrial Asymmetry")+
  #stat_compare_means(label = "p.signif",comparisons = MyWTComparisons)+
  #scale_fill_manual(values=c("#d44842", "#fac228", "#365c8d"))+ 
  theme_linedraw()+ theme(legend.position="none")+
  theme( plot.title=element_text(size=22),
         axis.text=element_text(size=18),
         axis.title=element_text(size=22))

ggplot(SimpleSummary, aes(x=Condition, y=as.numeric(SimpleSummary$SurfaceAreaToVolume), fill=Condition)) +
  geom_violin(alpha = 0.75) +
  geom_boxplot(width=0.3, color="black", alpha=1) +
  geom_beeswarm(alpha = 0.2) +
  xlab("Conditions") +
  ylab("Mitochondrial Surface Area/Volume")+
  #stat_compare_means(label = "p.signif",comparisons = MyWTComparisons)+
  #scale_fill_manual(values=c("#d44842", "#fac228", "#365c8d"))+ 
  theme_linedraw()+ theme(legend.position="none")+
  theme( plot.title=element_text(size=22),
         axis.text=element_text(size=18),
         axis.title=element_text(size=22))

ggplot(SimpleSummary, aes(x=Condition, y=as.numeric(SimpleSummary$NormalizedMitoVol), fill=Condition)) +
  geom_violin(alpha = 0.75) +
  geom_boxplot(width=0.3, color="black", alpha=1) +
  geom_beeswarm(alpha = 0.2) +
  xlab("Conditions") +
  ylab("Mitochondrial Volume Normalized to Cell Volume")+
  #stat_compare_means(label = "p.signif",comparisons = MyWTComparisons)+
  #scale_fill_manual(values=c("#d44842", "#fac228", "#365c8d"))+ 
  theme_linedraw()+ theme(legend.position="none")+
  theme( plot.title=element_text(size=22),
         axis.text=element_text(size=18),
         axis.title=element_text(size=22))


ggplot(SimpleSummary, aes(x=Condition, y=as.numeric(SimpleSummary$NormPeriphDistMito), fill=Condition)) +
  geom_violin(alpha = 0.75) +
  geom_boxplot(width=0.3, color="black", alpha=1) +
  geom_beeswarm(alpha = 0.2) +
  xlab("Conditions") +
  ylab("Peripheral Mitochondrial Concentration")+
  stat_compare_means(label = "p.signif",comparisons = MyWTComparisons)+
  scale_fill_manual(values=c("#d44842", "#fac228", "#365c8d"))+ theme_linedraw()+ theme(legend.position="none")+
  theme( plot.title=element_text(size=22),
         axis.text=element_text(size=18),
         axis.title=element_text(size=22))

ggplot(SimpleSummary, aes(x=Condition, y=as.numeric(SimpleSummary$NormCenterDistMito), fill=Condition)) +
  geom_violin(alpha = 0.75) +
  geom_boxplot(width=0.3, color="black", alpha=1) +
  geom_beeswarm(alpha = 0.2) +
  xlab("Conditions") +
  ylab("Central Mitochondrial Concentration")+
  stat_compare_means(label = "p.signif",comparisons = MyWTComparisons)+
  scale_fill_manual(values=c("#d44842", "#fac228", "#365c8d"))+ theme_linedraw()+ theme(legend.position="none")+
  theme( plot.title=element_text(size=22),
         axis.text=element_text(size=18),
         axis.title=element_text(size=22))

ggplot(SimpleSummary, aes(x=Condition, y=as.numeric(SimpleSummary$NormPeriphDistContactSA), fill=Condition)) +
  geom_violin(alpha = 0.75) +
  geom_boxplot(width=0.3, color="black", alpha=1) +
  geom_beeswarm(alpha = 0.2) +
  xlab("Conditions") +
  ylab("Peripheral Contact Concentration")+
  stat_compare_means(label = "p.signif",comparisons = MyWTComparisons)+
  scale_fill_manual(values=c("#d44842", "#fac228", "#365c8d"))+ theme_linedraw()+ theme(legend.position="none")+
  theme( plot.title=element_text(size=22),
         axis.text=element_text(size=18),
         axis.title=element_text(size=22))

ggplot (SimpleSummary, aes(x=Condition, y=as.numeric(SimpleSummary$AsymmetryMetric), fill=Condition)) +
  geom_violin(alpha = 0.75) +
  geom_boxplot(width=0.3, color="black", alpha=1) +
  geom_beeswarm(alpha = 0.2) +
  xlab("Conditions") +
  ylab("Mitochondrial Asymmetry")+
  stat_compare_means(label = "p.signif",comparisons = MyWTComparisons)+
  scale_fill_manual(values=c("#d44842", "#fac228", "#365c8d"))+ theme_linedraw()+ theme(legend.position="none")+
  theme( plot.title=element_text(size=22),
         axis.text=element_text(size=18),
         axis.title=element_text(size=22))




##### DONUT PLOTS #######
data <- data.frame(
  Distribution =c("Peripheral","Center"),
  count=c(Means ['NormPeriphDistMito', 'Fermentation' ],Means ['NormCenterDistMito','Fermentation' ])
)



# Compute percentages
data$fraction = data$count / sum(data$count)

# Compute the cumulative percentages (top of each rectangle)
data$ymax = cumsum(data$fraction)

# Compute the bottom of each rectangle
data$ymin = c(0, head(data$ymax, n=-1))

# Make the plot
ggplot(data, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Distribution)) +
  geom_rect() +
  coord_polar(theta="y") + # Try to remove that to understand how the chart is built initially # Try to remove that to see how to make a pie chart
  scale_fill_brewer(palette="Reds") +
  scale_color_brewer(palette="Reds") +
  coord_polar(theta="y") +
  xlim(c(-1, 4)) +
  theme_void() 


# Create test data
data <- data.frame(
  Distribution =c("Periphery", "Center"),
  count=c(Means ['NormPeriphDistContactSA', 'Fermentation' ],Means ['NormCenterDistContactSA','Fermentation' ])
)

# Compute percentages
data$fraction = data$count / sum(data$count)

# Compute the cumulative percentages (top of each rectangle)
data$ymax = cumsum(data$fraction)

# Compute the bottom of each rectangle
data$ymin = c(0, head(data$ymax, n=-1))

# Make the plot
ggplot(data, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Distribution)) +
  geom_rect() +
  coord_polar(theta="y") + # Try to remove that to understand how the chart is built initially # Try to remove that to see how to make a pie chart
  scale_fill_brewer(palette="Reds") +
  scale_color_brewer(palette="Reds") +
  coord_polar(theta="y") +
  xlim(c(-1, 4)) +
  theme_void() 




###Transition###

data <- data.frame(
  Distribution =c("Peripheral","Center"),
  count=c(Means ['NormPeriphDistMito', 'Transition' ],Means ['NormCenterDistMito','Transition' ])
)


# Compute percentages
data$fraction = data$count / sum(data$count)

# Compute the cumulative percentages (top of each rectangle)
data$ymax = cumsum(data$fraction)

# Compute the bottom of each rectangle
data$ymin = c(0, head(data$ymax, n=-1))

# Make the plot
ggplot(data, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Distribution)) +
  geom_rect() +
  coord_polar(theta="y") + # Try to remove that to understand how the chart is built initially # Try to remove that to see how to make a pie chart
  scale_fill_brewer(palette="Oranges") +
  scale_color_brewer(palette="Oranges") +
  coord_polar(theta="y") +
  xlim(c(-1, 4)) +
  theme_void() 


# Create test data
data <- data.frame(
  Distribution =c("Periphery", "Center"),
  count=c(Means ['NormPeriphDistContactSA', 'Transition' ],Means ['NormCenterDistContactSA','Transition' ])
)

# Compute percentages
data$fraction = data$count / sum(data$count)

# Compute the cumulative percentages (top of each rectangle)
data$ymax = cumsum(data$fraction)

# Compute the bottom of each rectangle
data$ymin = c(0, head(data$ymax, n=-1))

# Make the plot
ggplot(data, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Distribution)) +
  geom_rect() +
  coord_polar(theta="y") + # Try to remove that to understand how the chart is built initially # Try to remove that to see how to make a pie chart
  scale_fill_brewer(palette="Oranges") +
  scale_color_brewer(palette="Oranges") +
  coord_polar(theta="y") +
  xlim(c(-1, 4)) +
  theme_void() 


###Respiration###

data <- data.frame(
  Distribution =c("Peripheral","Center"),
  count=c(Means ['NormPeriphDistMito', 'Respiration' ],Means ['NormCenterDistMito','Respiration' ])
)


# Compute percentages
data$fraction = data$count / sum(data$count)

# Compute the cumulative percentages (top of each rectangle)
data$ymax = cumsum(data$fraction)

# Compute the bottom of each rectangle
data$ymin = c(0, head(data$ymax, n=-1))

# Make the plot
ggplot(data, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Distribution)) +
  geom_rect() +
  coord_polar(theta="y") + # Try to remove that to understand how the chart is built initially # Try to remove that to see how to make a pie chart
  scale_fill_brewer(palette="Blues") +
  scale_color_brewer(palette="Blues") +
  coord_polar(theta="y") +
  xlim(c(-1, 4)) +
  theme_void() 

# Create test data
data <- data.frame(
  Distribution =c("Periphery", "Center"),
  count=c(Means ['NormPeriphDistContactSA', 'Respiration' ],Means ['NormCenterDistContactSA','Respiration' ])
)

# Compute percentages
data$fraction = data$count / sum(data$count)

# Compute the cumulative percentages (top of each rectangle)
data$ymax = cumsum(data$fraction)

# Compute the bottom of each rectangle
data$ymin = c(0, head(data$ymax, n=-1))

# Make the plot
ggplot(data, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Distribution)) +
  geom_rect() +
  coord_polar(theta="y") + # Try to remove that to understand how the chart is built initially # Try to remove that to see how to make a pie chart
  scale_fill_brewer(palette="Blues") +
  scale_color_brewer(palette="Blues") +
  coord_polar(theta="y") +
  xlim(c(-1, 4)) +
  theme_void() 

