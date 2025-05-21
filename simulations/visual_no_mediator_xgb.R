####### Visualize XGB scenario ##########
library(randomForestSRC)
library(tidyverse)
library(rio)
library(akima)
library(xgboost)

data_new = rio::import("vis.csv")
df_trt = data.frame(matrix(rnorm(3721 * 100), 3721, 100))
for (seed in 1:100){
  
  data_save_path = paste0("/seed_",seed,".csv")
  data = rio::import(data_save_path)
  
  data0 = data[data$TRT==0,]
  data1 = data[data$TRT==1,]
  
  data0_fea = as.matrix(data0[,1:10])
  data1_fea = as.matrix(data1[,1:10])
  data0_label = as.vector(data0$Y)
  data1_label = as.vector(data1$Y)
  dtrain0 = xgb.DMatrix(data = data0_fea, label = data0_label)
  dtrain1 = xgb.DMatrix(data = data1_fea, label = data1_label)
  
  params <- list(
    objective = "reg:squarederror", 
    eta = 0.1,
    max_depth = 4,
    eval_metric = "rmse"
  )
  
  fit_Y_1 = xgb.train(
    params = params,
    data = dtrain1,
    nrounds = 100,
    verbose = 1
  )
  fit_Y_0 = xgb.train(
    params = params,
    data = dtrain0,
    nrounds = 100,
    verbose = 1
  )
  

  data_fea = as.matrix(data_new[,1:10])
  df_pred_Y_trt_1 = predict(object=fit_Y_1, newdata = as.matrix(data_fea))
  pred_Y_trt_1 = df_pred_Y_trt_1#$predicted
  df_pred_Y_trt_0 = predict(object=fit_Y_0, newdata = as.matrix(data_fea))
  pred_Y_trt_0 = df_pred_Y_trt_0#$predicted
  
  df_trt[,seed] = pred_Y_trt_1 - pred_Y_trt_0
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
