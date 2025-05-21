####### Visualize RF scenario ##########
library(randomForestSRC)
library(tidyverse)
library(rio)
library(akima)


data_new = rio::import("vis.csv") # you can find it in the same folder
df_trt = data.frame(matrix(rnorm(3721 * 100), 3721, 100))
for (seed in 1:100){
  
  data_save_path = paste0("/seed_",seed,".csv") # data path
  data = rio::import(data_save_path)
  
  data0 = data[data$TRT==0,]
  data1 = data[data$TRT==1,]
  
  fit_Y_1 = rfsrc(Y~Cov1+Cov2+Cov3+Cov4+Cov5+Cov6+Cov7+Cov8+Cov9+Cov10, 
                  data = data1, ntree = 2000, var.used = "all.trees")
  fit_Y_0 = rfsrc(Y~Cov1+Cov2+Cov3+Cov4+Cov5+Cov6+Cov7+Cov8+Cov9+Cov10, 
                  data = data0, ntree = 2000, var.used = "all.trees")
  
  df_pred_Y_trt_1 = predict(object=fit_Y_1, newdata = data_new)
  pred_Y_trt_1 = df_pred_Y_trt_1$predicted
  df_pred_Y_trt_0 = predict(object=fit_Y_0, newdata = data_new)
  pred_Y_trt_0 = df_pred_Y_trt_0$predicted
  
  df_trt[,seed] = pred_Y_trt_1 - pred_Y_trt_0
}


data_new$trt_effect = rowMeans(df_trt)

p = ggplot(data_new, aes(x=Cov1, y=Cov2, color=-trt_effect)) +
  geom_point(size=2) +
  scale_color_gradient(low="green", high="red") +
  theme_minimal() +
  labs(title="plot")
p





interp_res <- with(data_new, interp(Cov1, Cov2,z=trt_effect, duplicate="mean"))
filled.contour(interp_res, color.palette = terrain.colors,
               main="visualize")
