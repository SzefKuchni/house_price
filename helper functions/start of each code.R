source("/media/dominik/Storage/House/sample_ec.R")
source("/media/dominik/Storage/House/sample_fold.R")
crosval(1)
source("/media/dominik/Storage/House/sample_xy_vector.R")

library(h2o)
h2o.shutdown()
h2o.init(nthreads = 4, max_mem_size = "8g")

train_h2o <- as.h2o(train)
test_h2o <- as.h2o(test)

activation_sel<-list("Tanh","Rectifier","Maxout")
hidden_sel<-list(c(512), c(200,200))
epochs_sel<-list(10,100,1000)

params<-expand.grid(activation_sel, epochs_sel)

idx<-sample(1:9, 8, replace=F)

params[idx,]

results<-data.frame()

for (i in 1:length(idx)) {
  model<-h2o.deeplearning(x = x,
                          y = y,
                          training_frame = train_h2o,
                          validation_frame = test_h2o,
                          #distribution = "bernoulli",
                          activation = as.character(params$Var1[idx[i]]),
                          hidden = c(200,200),
                          #l1 = 1e-5,
                          epochs = as.numeric(params$Var2[idx[i]]),
                          sparse = T)

  RMSE<-model@model$validation_metrics@metrics$RMSE
  
  results<-rbind(results, data.frame("activation"=as.character(params$Var1[idx[i]]),
                                     "epochs"=as.numeric(params$Var2[idx[i]]),
                                     "RMSE"=RMSE))
}

model<-h2o.deeplearning(x = x,
                        y = y,
                        training_frame = train_h2o,
                        validation_frame = test_h2o,
                        #distribution = "bernoulli",
                        activation = "Tanh",
                        hidden = c(200,200),
                        #l1 = 1e-5,
                        epochs = 200,
                        sparse = T)
