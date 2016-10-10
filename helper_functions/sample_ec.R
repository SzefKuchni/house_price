#Setting directory - Ubuntu
temp<-Sys.info()['sysname']=="Linux"

if (temp) {
  setwd("/home/dominik/Desktop/House/")
}


#Reading the data

train<-read.csv("data/train.csv")
test<-read.csv("data/test.csv")

