#creating a function for creation of data sets for cross validation

# idx<-1:1459
# 
# idx1<-sample(idx,size = 292, replace = F)
# 
# idx<-setdiff(idx,idx1)
# idx2<-sample(idx,size = 292, replace = F)
# 
# idx<-setdiff(idx,idx2)
# idx3<-sample(idx,size = 292, replace = F)
# 
# idx<-setdiff(idx,idx3)
# idx4<-sample(idx,size = 292, replace = F)
# 
# idx5<-setdiff(idx,idx4)
# 
# saveRDS(idx1, file = "/media/dominik/Storage/House/Data/idx1.rds")
# saveRDS(idx2, file = "/media/dominik/Storage/House/Data/idx2.rds")
# saveRDS(idx3, file = "/media/dominik/Storage/House/Data/idx3.rds")
# saveRDS(idx4, file = "/media/dominik/Storage/House/Data/idx4.rds")
# saveRDS(idx5, file = "/media/dominik/Storage/House/Data/idx5.rds")

# idx1<-readRDS("/media/dominik/Storage/House/Data/idx1.rds")
# idx2<-readRDS("/media/dominik/Storage/House/Data/idx2.rds")
# idx3<-readRDS("/media/dominik/Storage/House/Data/idx3.rds")
# idx4<-readRDS("/media/dominik/Storage/House/Data/idx4.rds")
# idx5<-readRDS("/media/dominik/Storage/House/Data/idx5.rds")

crosval <- function(fold) {
  if (fold==1) {
    idx1<-readRDS("/media/dominik/Storage/House/Data/idx1.rds")
    test<<-train[idx1,]
    train<<-train[-idx1,]
  }
  if (fold==2) {
    idx2<-readRDS("/media/dominik/Storage/House/Data/idx2.rds")
    test<<-train[idx2,]
    train<<-train[-idx2,]
  }
  if (fold==3) {
    idx3<-readRDS("/media/dominik/Storage/House/Data/idx3.rds")
    test<<-train[idx3,]
    train<<-train[-idx3,]
  }
  if (fold==4) {
    idx4<-readRDS("/media/dominik/Storage/House/Data/idx4.rds")
    test<<-train[idx4,]
    train<<-train[-idx4,]
  }
  if (fold==5) {
    idx5<-readRDS("/media/dominik/Storage/House/Data/idx5.rds")
    test<<-train[idx5,]
    train<<-train[-idx5,]
  }
}
