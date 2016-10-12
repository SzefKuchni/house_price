#bivariate analysis of numerical variables
# as arguments it takes data set containing numerical features and the variable that we want to predict

library(ggplot2)

plots_bi_cat <- function(data_cat, y_variable) {
    data<-cbind(data_cat, train[y_variable])
    for (i in names(data_cat)) {
        p<-ggplot(data = data)+
            geom_boxplot(aes_string(x = i, y = "SalePrice"))+
            geom_violin(aes_string(x = i, y = "SalePrice"), alpha=0)
        p
        
        name<-paste("bi_cat_",i,".png", sep = "")
        full_path<-paste("plots/",name, sep="")
        
        png(full_path, width = 800, height = 800)
        print(p)
        dev.off()
    }
}

#plots_bi_num(data_cat = train_div$other, y_variable = "SalePrice")
