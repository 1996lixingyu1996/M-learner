####### Visualize XGB mediator  ##########
library(tidyverse)
library(rio)
library(akima)
library(xgboost)

set.seed(1)
n = 3721
p=10
#Y = rep(0,n)
#Y_CT = rep(0,n)
#M_CT = rep(0,n)
X1 = seq(-1.5,1.5,0.05)
X2 = seq(-1.5,1.5,0.05)
X_12 = expand.grid(X1,X2)

X <- matrix(rnorm(n * p), n, p)
X[,1]=X_12[,1]
X[,2]=X_12[,2]

W0 = c(rep(1,1861),rep(0,1860))
W <- sample(W0,n,replace = FALSE)


M = rep(0,n)
Y = rep(0,n)
#Y_CT = rep(0,n)
#M_CT = rep(0,n)

#### replace other xgb scenario's code here.
#### replacement start
indi = rep(0,n)
for (i in 1:n){
  if(X[i,1]>0&X[i,2]>0){
    M[i] = 1*X[i,1]*W[i] +1*X[i,2]*W[i] + rnorm(1,0,0.01) +1*X[i,3] +1*X[i,4] +0.5*X[i,1]+0.5*X[i,2] 
    #M_CT[i] = M[i] - 1*X[i,1]*W[i] +1*X[i,2]*W[i]
    #p <- 1 / (1 + exp(-logit_p))
    #= round(p)
    indi[i]  = 1
  }else{
    M[i] = rnorm(1,0,0.01)+1*X[i,3] + 1*X[i,4] +0.5*X[i,1]+0.5*X[i,2] 
    #M_CT[i] = M[i]
    #p <- 1 / (1 + exp(-logit_p))
    #M[i] = round(p)
  }
  Y[i] = 1 + 1*M[i] + rnorm(1,0,0.01)+ 0.5*X[i,3] +0.5*X[i,4]
  #Y_CT[i] = Y[i] - 1*M[i] + M_CT[i]
}
##### replacement end


data_new = as.data.frame(X)
colnames(data_new) = paste0("Cov",1:10)
data_new$M = M
data_new$W = W
#colnames(data_new) = paste0("Cov",1:10)

data_new = data.frame(X)
colnames(data_new) = paste0("Cov",1:10)

df_trt = data.frame(matrix(rnorm(3721 * 100), 3721, 100))
for (seed in 1:100){
  
  data_save_path = paste0("/seed_",seed,".csv")  # original data path (generated in the first stage)
  data = rio::import(data_save_path)
 
  data0 = data[data$TRT==0,]
  data1 = data[data$TRT==1,]
  
  ##### feature with X and M
  data0_fea_XM = as.matrix(data0[,c(1:10,12)])
  data1_fea_XM = as.matrix(data1[,c(1:10,12)])
  ##### feature with X 
  data0_fea_X = as.matrix(data0[,c(1:10)])
  data1_fea_X = as.matrix(data1[,c(1:10)])
  
  data0_label_Y = as.vector(data0$Y)
  data1_label_Y = as.vector(data1$Y)
  
  data0_label_M = as.vector(data0$M)
  data1_label_M = as.vector(data1$M)
  
  dtrain0_XM_Y = xgb.DMatrix(data = data0_fea_XM, label = data0_label_Y)
  dtrain1_XM_Y = xgb.DMatrix(data = data1_fea_XM, label = data1_label_Y)
  
  dtrain0_X_M = xgb.DMatrix(data = data0_fea_X, label = data0_label_M)
  dtrain1_X_M = xgb.DMatrix(data = data1_fea_X, label = data1_label_M)
  
  params <- list(
    objective = "reg:squarederror", 
    eta = 0.1,
    max_depth = 4,
    eval_metric = "rmse"
  )
  
  fit_M_1 = xgb.train(
    params = params,
    data = dtrain1_X_M,
    nrounds = 100,
    verbose = 1
  )
  fit_M_0 = xgb.train(
    params = params,
    data = dtrain0_X_M,
    nrounds = 100,
    verbose = 1
  )
  fit_Y_1 = xgb.train(
    params = params,
    data = dtrain1_XM_Y,
    nrounds = 100,
    verbose = 1
  )
  
  data_fea_X = as.matrix(data_new[,1:10])
  #data_fea_XM = as.matrix(data[,c(1:10,12)])
  
  df_pred_M_trt_1 = predict(object=fit_M_1, newdata = data_fea_X)
  pred_M_trt_1 = df_pred_M_trt_1#$predicted
  df_pred_M_trt_0 = predict(object=fit_M_0, newdata = data_fea_X)
  pred_M_trt_0 = df_pred_M_trt_0#$predicted
  
  data_trt1_m1 = data_new
  data_trt1_m1$M = pred_M_trt_1
  data_fea_XM1 = as.matrix(data_trt1_m1[,c(1:10,11)])
  Y_trt1_m1 = predict(object = fit_Y_1, newdata = data_fea_XM1)
  
  data_trt1_m0 = data_new
  data_trt1_m0$M = pred_M_trt_0
  data_fea_XM0 = as.matrix(data_trt1_m0[,c(1:10,11)])
  Y_trt1_m0 = predict(object = fit_Y_1, newdata = data_fea_XM0)

  indir_trt_effect = Y_trt1_m1 - Y_trt1_m0
  
  df_trt[,seed] = indir_trt_effect
}



data_new$trt_effect = rowMeans(df_trt)

p = ggplot(data_new, aes(x=Cov1, y=Cov2, color=trt_effect)) +
  geom_point(size=2) +
  scale_color_gradient(low="green", high="red") +
  theme_minimal() +
  labs(title="plot")
p





interp_res <- with(data_new, interp(Cov1, Cov2,z=trt_effect, duplicate="mean"))
filled.contour(interp_res, color.palette = terrain.colors,
               main="visualize")
