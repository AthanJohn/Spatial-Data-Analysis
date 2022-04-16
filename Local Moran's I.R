#install lctools
install.packages("lctools")
library(lctools)

#Load necessary data from GR.Municipalities dataset
data("GR.Municipalities")
df <- GR.Municipalities

#coordinates of geometric centroids of the new 325 Municipalities in Greece
crd<-cbind(df$X, df$Y)

l.moran <- l.moransI(crd,6,df$Income01)

#Calculate horizontal and vertical axis limits, according to min and max values of 7th and 8th columns of l.moran
xmin <- round(ifelse(abs(min(l.moran[,7])) > abs(min(l.moran[,8])) , abs(min(l.moran[,7])) , abs(min(l.moran[,8]))))
xmax <- round(ifelse(abs(max(l.moran[,7])) > abs(max(l.moran[,8])) , abs(max(l.moran[,7])) , abs(max(l.moran[,8]))))
xmax <- ifelse(xmin > xmax, xmin, xmax) + 1
ymax <- xmax
xmin <- -xmax
ymin <- -ymax

#Create regression line
rl <- lm(l.moran[,8]~l.moran[,7])

#Create scatterplot
plot(l.moran[,7] , l.moran[,8], main = "Income vs Lagged Income", sub="" , xlab="Income" , ylab="Lagged Income", xlim=c(xmin,xmax) , 
     ylim=c(ymin,ymax), col='red')

#Add horizontal axis at x=0
abline(h=0)

#Add vertical axis at y=0
abline(v=0)

#Add regression line
abline(rl)

