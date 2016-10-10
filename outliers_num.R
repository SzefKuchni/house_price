#outliers on one measure

outliers_num <- function(data) {
    result<-data.frame()
    for (i in names(data)) {
        low_outlier<-unname(quantile(data[,i], 0.25, na.rm = T)-3*(quantile(data[,i], 0.75, na.rm = T)-quantile(data[,i], 0.25, na.rm = T)))
        high_outlier<-unname(quantile(data[,i], 0.25, na.rm = T)+3*(quantile(data[,i], 0.75, na.rm = T)-quantile(data[,i], 0.25, na.rm = T)))
        result<-rbind(result,data.frame("Variable"=i, low_outlier, high_outlier))
    }
    result
}
