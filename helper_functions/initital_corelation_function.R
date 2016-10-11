temp<-train[,c("SalePrice","BsmtUnfSF")]

temp_scale<-scale(temp)
head(temp_scale)

cluster<-kmeans(temp_scale, 3)

temp<-cbind(temp, cluster$cluster)
temp_scale<-data.frame(temp_scale)
temp_scale<-cbind(temp_scale, cluster$cluster)

library(ggplot2)

ggplot(data = temp_scale)+
    geom_point(aes(x = BsmtUnfSF, y = SalePrice, col=`cluster$cluster`))+
    geom_point(data = data.frame(cluster$centers), aes(x = BsmtUnfSF, y = SalePrice))

cluster$centers

dist()
