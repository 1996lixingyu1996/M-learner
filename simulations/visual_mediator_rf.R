####### Visualize RF mediator ##########
library(randomForestSRC)
library(tidyverse)
library(rio)
library(akima)
library(xgboost)

n = 3721
p=10

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

## replace the mediator data in first stage 
## start
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

  }
  Y[i] = 1 + 1*M[i] + rnorm(1,0,0.01)+ 0.5*X[i,3] +0.5*X[i,4]
  #Y_CT[i] = Y[i] - 1*M[i] + M_CT[i]
}
## end

data_new = as.data.frame(X)
colnames(data_new) = paste0("Cov",1:10)
data_new$M = M
data_new$W = W

data_new = data.frame(X)
colnames(data_new) = paste0("Cov",1:10)

df_trt = data.frame(matrix(rnorm(3721 * 100), 3721, 100))
for (seed in 1:100){
  
  data_save_path = paste0("/seed_",seed,".csv")
  data = rio::import(data_save_path)
  
  data0 = data[data$TRT==0,]
  data1 = data[data$TRT==1,]
  
  fit_M_1 = rfsrc(M~Cov1+Cov2+Cov3+Cov4+Cov5+Cov6+Cov7+Cov8+Cov9+Cov10, 
                  data = data1, ntree = 2000, var.used = "all.trees")
  fit_M_0 = rfsrc(M~Cov1+Cov2+Cov3+Cov4+Cov5+Cov6+Cov7+Cov8+Cov9+Cov10, 
                  data = data0, ntree = 2000, var.used = "all.trees")
  
  rf_y_1 = rfsrc(Y~Cov1+Cov2+Cov3+Cov4+Cov5+Cov6+Cov7+Cov8+Cov9+Cov10+M,data = data1, ntree = 2000, var.used = "all.trees")
  
  df_pred_M_trt_1 = predict(object=fit_M_1, newdata = data_new)
  pred_M_trt_1 = df_pred_M_trt_1$predicted
  df_pred_M_trt_0 = predict(object=fit_M_0, newdata = data_new)
  pred_M_trt_0 = df_pred_M_trt_0$predicted
  
  data_trt1_m1 = data_new
  data_trt1_m1$M = pred_M_trt_1
  Y_trt1_m1 = predict(object = rf_y_1, newdata = data_trt1_m1)
  
  data_trt1_m0 = data_new
  data_trt1_m0$M = pred_M_trt_0
  Y_trt1_m0 = predict(object = rf_y_1, newdata = data_trt1_m0)
  
  indir_trt_effect = Y_trt1_m1$predicted - Y_trt1_m0$predicted
  
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
