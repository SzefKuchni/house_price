source("helper_functions/sample_ec.R")

source("helper_functions/explore_feature_div.R")
train_div<-feature_div(train, "Id")

train_div$class

source("helper_functions/explore_cor_with.R")
correlation<-cor_with(data = train_div$numeric, measure_to_check_cor_with = "SalePrice")

source("helper_functions/explore_isNA.R")
missing<-isNA(data = train)

source("helper_functions/explore_outliers_num.R")
outliers_num(data = train_div$numeric)

