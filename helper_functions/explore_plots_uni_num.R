#plotting - takes as argument train and test sets combined with a column 
#saying whether it is train ir test named origin

library(ggplot2)

#univariate analysis
#numerical variables

plot_uni_num <- function(data_with_num_var, data_to_plot_with) {
    for (i in names(data_with_num_var)) {
        p<-ggplot(data = data_to_plot_with)+
            geom_histogram(aes_string(x=i, fill="origin"), position = "dodge")+
            ggtitle(paste("Histogram of ", i, sep = ""))
        name<-paste("uni_num_",i,".png", sep="")
        full_path<-paste("plots/",name, sep="")
        png(full_path)
        print(p)
        dev.off()
    }
}

#plot_uni_num(data_with_num_var = train_div$numeric, data_to_plot_with = combi)
