#correlation analysis

cor_with <- function(data, measure_to_check_cor_with) {
    result<-data.frame()
    for (i in names(data[,-which(names(data) %in% c(measure_to_check_cor_with))])) {
        corel<-cor(x = data[,measure_to_check_cor_with], y = data[,i], use = "pairwise")
        result<-rbind(result, data.frame("Variable"=i, "Cor"=corel))
    }
    result<-result[order(result$Cor, decreasing = T),]
    result
}
