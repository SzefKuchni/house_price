result<-data.frame()

for (a in 1:10) {
    clusters<-kmeans(scaled_data, a)
    
    scaled_data<-data.frame(scaled_data)
    scaled_data$cluster<-clusters$cluster
    scaled_data$idx<-1:nrow(temp_data)
    
    centers<-data.frame(clusters$centers)
    names(centers)<-c("Y_center", "X_center")
    centers$cluster<-row.names(centers)
    
    scaled_data<-merge(scaled_data, centers, by="cluster")
    
    #temp_data$Y_center<-temp_data$Y_center*sd(temp_data[,y_variable])+mean(temp_data[,y_variable])
    #temp_data$X_center<-temp_data$X_center*sd(temp_data[,i])+mean(temp_data[,i])
    scaled_data$dist_from_center<-sqrt((scaled_data[,y_variable]-scaled_data$Y_center)^2+(scaled_data[,i]-scaled_data$X_center)^2)
    
    temp<-sum(scaled_data$dist_from_center^2)
    result<-rbind(result, data.frame(a, temp))
    
    low_outlier<-unname(quantile(scaled_data$dist_from_center, 0.25, na.rm = T)-1.5*(quantile(scaled_data$dist_from_center, 0.75, na.rm = T)-quantile(scaled_data$dist_from_center, 0.25, na.rm = T)))
    high_outlier<-unname(quantile(scaled_data$dist_from_center, 0.75, na.rm = T)+1.5*(quantile(scaled_data$dist_from_center, 0.75, na.rm = T)-quantile(scaled_data$dist_from_center, 0.25, na.rm = T)))
    
    scaled_data$outlier_flag<-as.factor(ifelse(scaled_data$dist_from_center>high_outlier, 1, 0))
    
    temp_data$cluster<-clusters$cluster
    temp_data<-merge(temp_data, scaled_data[,c("idx", "outlier_flag")], by="idx")
    head(temp_data)
    
    p<-ggplot()+
        geom_point(data=temp_data, aes_string(y=y_variable, x=i, col="cluster"))+
        geom_point(data=temp_data[temp_data$outlier_flag==1,], aes_string(y=y_variable, x=i), col="red", shape=1, size=5)+
        #geom_point(data=temp_data, aes(y=Y_center, x=X_center), col="red", shape=4, size=5)+
        geom_vline(xintercept = outliers_num_result[outliers_num_result$Variable==i, "high_outlier"], col="red")+
        geom_hline(yintercept = outliers_num_result[outliers_num_result$Variable==y_variable, "high_outlier"], col="red")+
        geom_smooth(data=temp_data, aes_string(y=y_variable, x=i))+
        geom_smooth(data=temp_data, aes_string(y=y_variable, x=i), method = "lm")+
        scale_y_continuous(labels = short)
}

library(dplyr)
max_temp<-max(result$temp)
result<-result %>% 
    mutate("change"=(lag(temp)-temp)/max_temp,
           "flag"=ifelse(change<0.1,1,0))
nr_cluster<-which(result$flag==1)[1]-1

name<-paste("scree_", i,"_bi_num.png", sep = "")
full_path<-paste("plots/",name, sep="")

png(full_path, width = 800, height = 800)
plot(x = result$a, y = result$temp, type = "l")
abline(v=nr_cluster, col="red")
dev.off()