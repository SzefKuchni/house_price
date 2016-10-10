#is na function

isNA <- function(data) {
    result<-data.frame()
    for (i in names(data)) {
        result<-rbind(result,data.frame(i, sum(is.na(data[,i]))/nrow(data)))
    }
    
    names(result)<-c("Variable", "Prct_NA")
    result<-result[order(result$Prct_NA, decreasing = T),]
    result
}