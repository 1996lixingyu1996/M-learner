##### Experiment 1 XGBoost######
## Mediator Model
##### Indirect experiment ########
library(xgboost)
f_exp1 <- function(seed){
  n= 1000
  set.seed(seed)
  
  data_save_path = paste0("/Users/xli36/Downloads/R_packages/myproject/expe2_new/seed_",seed,".csv")
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
  
  data_fea_X = as.matrix(data[,1:10])
  #data_fea_XM = as.matrix(data[,c(1:10,12)])
  
  df_pred_M_trt_1 = predict(object=fit_M_1, newdata = data_fea_X)
  pred_M_trt_1 = df_pred_M_trt_1#$predicted
  df_pred_M_trt_0 = predict(object=fit_M_0, newdata = data_fea_X)
  pred_M_trt_0 = df_pred_M_trt_0#$predicted
  
  data_trt1_m1 = data
  data_trt1_m1$M = pred_M_trt_1
  data_fea_XM1 = as.matrix(data_trt1_m1[,c(1:10,12)])
  Y_trt1_m1 = predict(object = fit_Y_1, newdata = data_fea_XM1)
  
  data_trt1_m0 = data
  data_trt1_m0$M = pred_M_trt_0
  data_fea_XM0 = as.matrix(data_trt1_m0[,c(1:10,12)])
  Y_trt1_m0 = predict(object = fit_Y_1, newdata = data_fea_XM0)
  
  # df_pred_Y_trt_1 = predict(object=fit_Y_1, newdata = as.matrix(data_fea))
  # pred_Y_trt_1 = df_pred_Y_trt_1#$predicted
  # df_pred_Y_trt_0 = predict(object=fit_Y_0, newdata = as.matrix(data_fea))
  # pred_Y_trt_0 = df_pred_Y_trt_0#$predicted
  
  
  
  
  #fit_M_1 = rfsrc(M~Cov1+Cov2+Cov3+Cov4+Cov5+Cov6+Cov7+Cov8+Cov9+Cov10, 
  #                data = data1, ntree = 2000, var.used = "all.trees")
  #fit_M_0 = rfsrc(M~Cov1+Cov2+Cov3+Cov4+Cov5+Cov6+Cov7+Cov8+Cov9+Cov10, 
  #                data = data0, ntree = 2000, var.used = "all.trees")
  
  ## 
  #lm_1 = lm(Y~Cov1+Cov2+Cov3+Cov4+Cov5+Cov6+Cov7+Cov8+Cov9+Cov10+M,data = data1)
  #rf_y_1 = rfsrc(Y~Cov1+Cov2+Cov3+Cov4+Cov5+Cov6+Cov7+Cov8+Cov9+Cov10+M,data = data1, ntree = 2000, var.used = "all.trees")
  
  # df_pred_M_trt_1 = predict(object=fit_M_1, newdata = data)
  # pred_M_trt_1 = df_pred_M_trt_1$predicted
  # df_pred_M_trt_0 = predict(object=fit_M_0, newdata = data)
  # pred_M_trt_0 = df_pred_M_trt_0$predicted
  
  # data_trt1_m1 = data
  # data_trt1_m1$M = pred_M_trt_1
  # Y_trt1_m1 = predict(object = rf_y_1, newdata = data_trt1_m1)
  
  # data_trt1_m0 = data
  # data_trt1_m0$M = pred_M_trt_0
  # Y_trt1_m0 = predict(object = rf_y_1, newdata = data_trt1_m0)
  
  indir_trt_effect = Y_trt1_m1 - Y_trt1_m0
  #data
  data$indir_trt_effect = indir_trt_effect
  
  #data$dif_indir_effect = data$true_indirect_effect - data$indir_trt_effect
  #MAE = 0#mean(abs(data$dif_indir_effect))
  #MSE = 0#mean((data$dif_indir_effect**2))
  dis3 = as.data.frame(matrix(rep(0,n*n),nrow = n))
  for (i in 1:n){
    dis3[i,] = rep(as.numeric(data[i,"indir_trt_effect"]),n) - as.numeric(data$indir_trt_effect)
  }
  dis3 = (dis3)**2
  tsne_result = Rtsne::Rtsne(dis3,dims=2,is_distance=TRUE,verbose=FALSE,max_iter = 2000, theta = 0)
  
  kmeans_result_2 = kmeans(tsne_result$Y, centers = 2, iter.max = 50, nstart = 10)
  kmeans_result_3 = kmeans(tsne_result$Y, centers = 3, iter.max = 50, nstart = 10)
  kmeans_result_4 = kmeans(tsne_result$Y, centers = 4, iter.max = 50, nstart = 10)
  kmeans_result_5 = kmeans(tsne_result$Y, centers = 5, iter.max = 50, nstart = 10)
  #kmeans_result_6 = kmeans(tsne_result$Y, centers = 6, iter.max = 50, nstart = 10)
  
  data$kmeans2 = as.factor(kmeans_result_2$cluster)
  data$kmeans3 = as.factor(kmeans_result_3$cluster)
  data$kmeans4 = as.factor(kmeans_result_4$cluster)
  data$kmeans5 = as.factor(kmeans_result_5$cluster)
  #data$kmeans6 = as.factor(kmeans_result_6$cluster)
  
  pval_flag_M = 1
  pval_flag_Y = 1
  data_flag = NULL
  result = list()
  for (j in 2:5){
    x_name = paste0("Cov",1:10)
    c_name = paste0("kmeans",j)  #"kmeans3"
    profiles <- tree_fit(Y=data[,c_name], X=data[,x_name], seed=1234)
    #length(profiles$trees)
    if(length(profiles$trees)>0){
      
      for(k in 1:length(profiles$trees)){
        data_tmp = predict_path(profiles$trees[[1]], newdata = data)
        fit_lm1 = lm(Y~leaf+TRT+leaf*TRT, data = data_tmp)
        fit_lm0 = lm(Y~leaf+TRT, data = data_tmp)
        pval <- as.numeric(na.omit(stats::anova(fit_lm0, fit_lm1)[[6]]))
        if(pval<pval_flag_Y){
          pval_flag_Y = pval
          #data_flag = data_tmp
          n_cluster_Y = j
          profile_index_Y = k
          profiles_save_Y = profiles
        }
        fit_lm1 = lm(M~leaf+TRT+leaf*TRT, data = data_tmp)
        fit_lm0 = lm(M~leaf+TRT, data = data_tmp)
        pval <- as.numeric(na.omit(stats::anova(fit_lm0, fit_lm1)[[6]]))
        if(pval<pval_flag_M){
          pval_flag_M = pval
          #data_flag = data_tmp
          n_cluster_M = j
          profile_index_M = k
          profiles_save_M = profiles
        }
      }
    }
    
    
    
    #plot_profile(profiles$trees[[1]])
    
  }
  
  save_path = paste0("/Users/xli36/Downloads/R_packages/myproject/expe2_new/seed_xgb_",seed,".rds")
  result[[1]] = pval_flag_M
  result[[2]] = n_cluster_M
  result[[3]] = profile_index_M#data_flag
  result[[4]] = profiles_save_M
  result[[5]] = pval_flag_Y
  result[[6]] = n_cluster_Y
  result[[7]] = profile_index_Y
  result[[8]] = profiles_save_Y
  names(result) = c("pval_M","n_cluster_M","profile_index_M","profiles_save_M","pval_flag_Y","n_cluster_Y","profile_index_Y","profiles_save_Y")
  rio::export(result, save_path)
}

#data = rio::import(paste0("/Users/xli36/Downloads/R_packages/myproject/expe1/seed_",1,".rds"))
library(Rtsne)
library(rio)
library(snowfall)
library(parallel)

my_new_folder = "/Users/xli36/Downloads/R_packages/myproject/expe2_new"
if (!dir.exists("my_new_folder")) {
  dir.create("my_new_folder")
}

sfInit(parallel = TRUE, cpus = (detectCores() - 4))
sfLibrary(dplyr)
sfLibrary(rio)
sfLibrary(rpart)
sfLibrary(xgboost)
sfLibrary(base)
sfLibrary(Rtsne)
sfSource("/Users/xli36/Desktop/rf_test/tree_fit_part.R")

result_vector = sfLapply(1:100, f_exp1)


sfStop()