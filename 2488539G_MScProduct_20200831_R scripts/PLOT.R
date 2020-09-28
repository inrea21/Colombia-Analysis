library(ggplot2)
library(cowplot)
library(ggpubr)

library(randomForest)
theme_set(theme_cowplot())
setwd('D:/uni/project MSc/Patia/mosaic')
data<- read.csv("Patiadataorig.csv", header= TRUE)
View(data)
str(data)
data$Neutral.Detergent.Acid <- as.numeric(as.character(data$Neutral.Detergent.Acid))
data$X.Acid.Detergent.Fibre <- as.numeric(as.character(data$X.Acid.Detergent.Fibre))

a<- ggplot(data, aes(Cultivar, DM_g_m2)) + 
  geom_boxplot( col= 'black',)+ 
  ggtitle("DM") + xlab("Cultivar") + ylab("DM (g/m²)") +
  theme(plot.title = element_text(hjust=0.1))
a

#i<- ggplot(data, aes(Cultivar, DM.g.m2.)) + 
#  geom_boxplot( col= 'black',)+ 
#  ggtitle("DM") +
#  xlab("Cultivar") + ylab("DM (g/m2)") +
 # theme(plot.title = element_text(hjust=0.1))
#i

#j<- ggplot(data, aes(Cultivar, DM.g.m2.)) + 
#  geom_boxplot( col= 'black',)+ 
 # ggtitle("DM") +
# xlab("Cultivar") + ylab("DM (g/mÂ²)") +
 # theme(plot.title = element_text(hjust=0.1))
#j



b<- ggplot(data, aes(Location, NDVI..handheld.)) + 
  geom_boxplot( col= 'black')+ ggtitle("NDVI") +
  xlab("Location") + ylab("NDVI") + 
  theme(plot.title = element_text(hjust=0.1))
b


c<- ggplot(data, aes(Cultivar, Crude_Protein)) + 
  geom_boxplot( col= 'black')+ 
  ggtitle("CP") + ylab("CP Concentration (%)") +
  theme(plot.title = element_text(hjust=0.1), axis.title.x = element_blank())
c


d<- ggplot(data, aes(Cultivar, In_vitro_digestibility)) + 
  geom_boxplot( col= 'black')+ 
  ggtitle("IVDMD") +
  xlab("Cultivar") + ylab("IVDMD Concentration (%)") + 
  theme(plot.title = element_text(hjust=0.1))
d


e<- ggplot(data, aes(Location, Dry.Weight..g.)) + 
  geom_boxplot( col= 'black')+ 
  ggtitle("Dry Weight") +
  xlab("Location") + ylab("Dry Weight (g)") + 
  theme(plot.title = element_text(hjust=0.1))
e


f<- ggplot(data, aes(Cultivar, Ash)) + 
  geom_boxplot( col= 'black')+ 
  ggtitle("Ash") +ylab("Ash Concentration (%)") + 
  theme(plot.title = element_text(hjust=0.1), axis.title.x = element_blank())
f


g<- ggplot(data, aes(Location, Neutral.Detergent.Acid)) + 
  geom_boxplot( col= 'black')+ 
  ggtitle("oNDF") +
  xlab("Location") + ylab("oNDF Concentration (%)") + 
  theme(plot.title = element_text(hjust=0.1))
 g

 
 h<- ggplot(data, aes(Location, X.Acid.Detergent.Fibre)) + 
   geom_boxplot( col= 'black')+ 
   ggtitle("oADF") +
   xlab("Location") + ylab("oADF Concentration (%)") + 
   theme(plot.title = element_text(hjust=0.1))
 h
 
 
 ggarrange( f, c,a, d,
           ncol = 2, nrow = 2)
 