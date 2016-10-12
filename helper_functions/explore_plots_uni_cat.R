#plotting - takes as argument train and test sets combined with a column 
#saying whether it is train ir test named origin

library(ggplot2)
library(reshape2)

#univariate analysis
#categorical variables
source("helper_functions/multiplot.R")

plot_uni_cat <- function(data_with_cat_var, data_to_plot_with) {
    for (i in names(data_with_cat_var)) {
        
        x<-if (length(table(train[i]))==length(table(test[i]))) {
            x<-melt(round((table(train[i])-table(test[i]))/table(train[i]),2))
            
            p2<-ggplot(data = x)+
                geom_bar(aes(x = Var1, y = value), stat = "identity")+
                ggtitle("realtive difference between datasets")
        } else {
            p2<-ggplot()
        }
        
        p1<-ggplot(data = data_to_plot_with)+
            geom_bar(aes_string(x=i, fill="origin"), stat = "count", position = "dodge")+
            ggtitle("Count in clases")+ theme(legend.position="bottom")
        
        name<-paste("uni_cat_",i,".png", sep="")
        full_path<-paste("plots/",name, sep="")
        png(full_path, width = 1000, height = 1000)
        print(multiplot(p1,p2, layout = matrix(c(1,1,2), nrow=3, byrow=TRUE)))
        dev.off()
    }
}

#plot_uni_cat(data_with_cat_var = train_div$other, data_to_plot_with = combi)