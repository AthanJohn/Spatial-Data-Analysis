#install lctools
install.packages("lctools")
library(lctools)

#Load necessary data from GR.Municipalities dataset
data("GR.Municipalities")
df <- GR.Municipalities

#coordinates of geometric centroids of the new 325 Municipalities in Greece
crd<-cbind(df$X, df$Y)

#number of nearest neighbors
k <- c(3, 5, 9, 12, 15, 18, 20, 24, 30)

#initialize moran table
moran<-matrix(data=NA,nrow=9,ncol=7)
colnames(moran) <- c("ID", "# of nearest neighbors", "Moran's", "Z Resampling", "P-value Resampling", "Z randomization", "P-value Randomization")
counter<-1
for(b in k){
  moranI<-moransI(crd,b,df$UnemrT01)
  moran[counter,1]<-counter
  moran[counter,2]<-b 
  moran[counter,3]<-moranI$Morans.I 
  moran[counter,4]<-moranI$z.resampling 
  moran[counter,5]<-moranI$p.value.resampling 
  moran[counter,6]<-moranI$z.randomization 
  moran[counter,7]<-moranI$p.value.randomization 
  counter<-counter+1 
} 

moran

l.moran <- l.moransI(crd,9,df$UnemrT01)
l.moran

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
plot(l.moran[,7] , l.moran[,8], main = "Unemployment VS Lagged Unemployment", sub="" , xlab="Unemployment" , ylab="Lagged Unemploeyment", xlim=c(xmin,xmax) , 
     ylim=c(ymin,ymax), col='red')

#Add horizontal axis at x=0
abline(h=0)

#Add vertical axis at y=0
abline(v=0)

#Add regression line
abline(rl)



#add our data to the map
df@data$Idx<-seq_len(nrow(df)) 
df_tmp<-merge(df@data, l.moran, by.x="OBJECTID", by.y="ID", sort=FALSE, all=TRUE) 
df@data<-df_tmp[order(df_tmp$Idx),] 

#install ggplot2 and rgeos
install.packages(c("ggplot2","rgeos"))
library(ggplot2)
library(rgeos)

#data preparation
map.f <- fortify(df, region = "OBJECTID")
map.f <- merge(map.f, df@data, by.x = "id", by.y = "OBJECTID")


map <- ggplot(map.f, aes(long, lat, group = group)) +
  geom_polygon(colour="gray80", aes(fill=as.factor(Cluster))) +
  scale_fill_manual(values=c("white", "yellow", "green", "blue", "red")) +
  coord_equal() +
  labs(x = "Easting (m)", y = "Northing (m)", fill = "Class") +
  ggtitle("Unemployment per municipality")

map