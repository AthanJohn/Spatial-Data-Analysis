#install lctools
install.packages("lctools")
library(lctools)

#Load necessary data from GR.Municipalities dataset
data("GR.Municipalities")
df <- GR.Municipalities

#store data into variables for males and females
male_un <- df$UnemrM01
female_un <- df$UnemrF01

#for multiple plots
par(mfrow=c(1,2)) # set the plotting area into a 1*2 array

#histogramms
hist(male_un, main = "Male Unemployment Histogramm", xlab = "% of Unemployment", ylab = "# of municipalities", border = "black", col = "blue", xlim = c(0,30), 
     ylim = c(0,150), breaks = 17, las=1)

hist(female_un, main = "Female Unemployment Histogramm", xlab = "% of Unemployment", ylab = "# of female municipalities", border = "black", col = "red", xlim = c(0,35), 
     ylim = c(0,100), breaks = 15, las=1)
