#install lctools
install.packages("lctools")
library(lctools)

#Load necessary data from GR.Municipalities dataset
data("GR.Municipalities")
df <- GR.Municipalities

#create dataframe with UnemrM01 and UnemrF01
merge_data <- data.frame(male_un = df$UnemrM01, female_un = df$UnemrF01)

#Boxplot
b.plot <- boxplot(merge_data, main = "Unemployment rate boxplot for males (left) and females (right) ", col="green")

#store outliers to variable per sex
outliers <- cbind(out=b.plot$out, sex = b.plot$group)

#find the max outliers per sex
out.limits <- aggregate(out ~ sex, data=outliers, max)
