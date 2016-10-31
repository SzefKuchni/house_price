source("C://Users/T540pDLEWYNBQ/Desktop/house_price/helper_functions/sample_ec.R")

source("helper_functions/explore_feature_div.R")
train_div<-feature_div(train, "Id")

train_div$class

source("helper_functions/explore_cor_with.R")
correlation<-cor_with(data = train_div$numeric, measure_to_check_cor_with = "SalePrice")

source("helper_functions/explore_isNA.R")
missing<-isNA(data = train)

source("helper_functions/explore_outliers_num.R")
outliers<-outliers_num(data = train_div$numeric)

test$SalePrice<-NA
combi<-rbind(train, test)
combi$origin<-ifelse(is.na(combi$SalePrice), "test", "train")
source("helper_functions/explore_plots_uni_num.R")
plot_uni_num(data_with_num_var = train_div$numeric, data_to_plot_with = combi)

source("helper_functions/explore_plots_uni_cat.R")
plot_uni_cat(data_with_cat_var = train_div$other, data_to_plot_with = combi)

source("helper_functions/explore_plots_bi_num_outliers_LM.R")
plots_bi_num(data_num = train_div$numeric, y_variable = "SalePrice")

source("helper_functions/explore_plots_bi_cat.R")
plots_bi_cat(data_cat = train_div$other, y_variable = "SalePrice")

#changes to be implemented
#-count of observations below the bivariate categorical chart
#-outliers in the bivvariate numerical plot based on the distance from cluster