#bivariate analysis of numerical variables
# as arguments it takes data set containing numerical features and the variable that we want to predict

plots_bi_num <- function(data_num, y_variable) {
    source("helper_functions/explore_outliers_num.R")
    outliers_num_result<-outliers_num(data_num)
    
    source("helper_functions/explore_cor_with.R")
    cor_result<-cor_with(data_num, y_variable)
    
    short <- function(a) {
        return(paste(a/1000, "k", sep = ""))
    }
    
    for (i in cor_result$Variable) {
        temp_data<-data_num[,c(y_variable, i)]
        
        scaled_data<-scale(temp_data)
        
        clusters<-kmeans(scaled_data, 3)
        
        temp_data$cluster<-clusters$cluster
        
        centers<-data.frame(clusters$centers)
        names(centers)<-c("Y_center", "X_center")
        centers$cluster<-row.names(centers)
        
        temp_data<-merge(temp_data, centers, by="cluster")
        
        temp_data$Y_center<-temp_data$Y_center*sd(temp_data[,y_variable])+mean(temp_data[,y_variable])
        temp_data$X_center<-temp_data$X_center*sd(temp_data[,i])+mean(temp_data[,i])
        temp_data$dist_from_center<-sqrt((temp_data[,y_variable]-temp_data$Y_center)^2+(temp_data[,i]-temp_data$X_center)^2)
        
        low_outlier<-unname(quantile(temp_data$dist_from_center, 0.25, na.rm = T)-3*(quantile(temp_data$dist_from_center, 0.75, na.rm = T)-quantile(temp_data$dist_from_center, 0.25, na.rm = T)))
        high_outlier<-unname(quantile(temp_data$dist_from_center, 0.25, na.rm = T)+3*(quantile(temp_data$dist_from_center, 0.75, na.rm = T)-quantile(temp_data$dist_from_center, 0.25, na.rm = T)))
        
        temp_data$outlier_flag<-as.factor(ifelse(temp_data$dist_from_center>high_outlier, 1, 0))
        
        head(temp_data)
        
        p<-ggplot()+
            geom_point(data=temp_data, aes_string(y=y_variable, x=i, col="cluster"))+
            geom_point(data=temp_data[temp_data$outlier_flag==1,], aes_string(y=y_variable, x=i), col="red", shape=1, size=5)+
            geom_point(data=temp_data, aes(y=Y_center, x=X_center), col="red", shape=4, size=5)+
            geom_vline(xintercept = outliers_num_result[outliers_num_result$Variable==i, "high_outlier"], col="red")+
            geom_hline(yintercept = outliers_num_result[outliers_num_result$Variable==y_variable, "high_outlier"], col="red")+
            geom_smooth(data=temp_data, aes_string(y=y_variable, x=i))+
            geom_smooth(data=temp_data, aes_string(y=y_variable, x=i), method = "lm")+
            scale_y_continuous(labels = short)#+
            #scale_colour_manual(values=c("0"="black","1"="red"))
        
        cor<-round(cor_result[cor_result$Variable==i,"Cor"],2)
        name<-paste(cor, "_", i,"_bi_num.png", sep = "")
        full_path<-paste("plots/",name, sep="")
        
        png(full_path, width = 800, height = 800)
        print(p)
        dev.off()
    }
}

#plots_bi_num(data_num = train_div$numeric, y_variable = "SalePrice")
