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
        
        formula<-formula(paste(y_variable,"~",i))
        lm<-lm(formula=formula, data=temp_data)
        prediction<-predict(object = lm, newdata = temp_data[i])
        
        temp_data<-cbind(temp_data, prediction)
        
        temp_data$dist_from_lm<-sqrt((temp_data[,y_variable]-temp_data$prediction)^2)
        
        low_outlier<-unname(quantile(temp_data$dist_from_lm, 0.25, na.rm = T)-1.5*(quantile(temp_data$dist_from_lm, 0.75, na.rm = T)-quantile(temp_data$dist_from_lm, 0.25, na.rm = T)))
        high_outlier<-unname(quantile(temp_data$dist_from_lm, 0.75, na.rm = T)+1.5*(quantile(temp_data$dist_from_lm, 0.75, na.rm = T)-quantile(temp_data$dist_from_lm, 0.25, na.rm = T)))
        
        temp_data$outlier_flag<-as.factor(ifelse(temp_data$dist_from_lm>high_outlier, 1, 0))
        
        head(temp_data)
        
        p<-ggplot()+
            geom_point(data=temp_data, aes_string(y=y_variable, x=i, col="outlier_flag"))+
            geom_vline(xintercept = outliers_num_result[outliers_num_result$Variable==i, "high_outlier"], col="red")+
            geom_hline(yintercept = outliers_num_result[outliers_num_result$Variable==y_variable, "high_outlier"], col="red")+
            geom_smooth(data=data_num, aes_string(y=y_variable, x=i))+
            geom_smooth(data=temp_data, aes_string(y=y_variable, x=i), method = "lm", se = FALSE)+
            scale_y_continuous(labels = short)+
            scale_color_manual(values = c("0"="black","1"="red"))
        
        cor<-round(cor_result[cor_result$Variable==i,"Cor"],2)
        name<-paste(cor, "_", i,"_bi_num.png", sep = "")
        full_path<-paste("plots/",name, sep="")
        
        png(full_path, width = 800, height = 800)
        print(p)
        dev.off()
    }
}

#plots_bi_num(data_num = train_div$numeric, y_variable = "SalePrice")
