var_analysis <- function(data, ommit) {
    wynik<-list()
    result<-data.frame()
    ommit_idx<-which(names(data) %in% ommit)
    for (i in names(data)) {
        temp<-data[[i]]
        result<-rbind(result,data.frame("Variable"=i, "Class"=class(temp)))
    }
    wynik$class<-result
    wynik$ommited<-data[,which(names(data) %in% ommit)]
    data<-data[,-ommit_idx]
    wynik$numeric<-data[,which(result[-ommit_idx,"Class"]=="numeric" | result[-ommit_idx,"Class"]=="integer")]
    wynik$other<-data[,which(result[-ommit_idx,"Class"]!="numeric" & result[-ommit_idx,"Class"]!="integer")]
    return(wynik)
}