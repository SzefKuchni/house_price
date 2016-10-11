#plotting - takes as argument train and test sets combined with a column 
#saying whether it is train ir test named origin

library(ggplot2)

#univariate analysis
#categorical variables

plot_uni_cat <- function(data_with_cat_var, data_to_plot_with) {
    for (i in names(data_with_cat_var)) {
        p<-ggplot(data = data_to_plot_with)+
            geom_bar(aes_string(x=i, fill="origin"), stat = "count", position = "dodge")+
            ggtitle("Count in clases")
        p
        name<-paste("uni_cat_",i,".png", sep="")
        full_path<-paste("plots/",name, sep="")
        png(full_path)
        print(p)
        dev.off()
    }
}

