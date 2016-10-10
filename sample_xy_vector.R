#prepare x y vectors

ignore<-c("Id")
y <- "SalePrice"
x <- setdiff(names(train), y)
x <- setdiff(x, ignore)