##### Experiment 1-No Mediator ######

##### Indirect experiment ########

f_exp1 <- function(seed){
  
  set.seed(seed)
  ## generate data
  
  n <- 1000
  p <- 10
  X <- matrix(rnorm(n * p), n, p)
  W0 = c(rep(1,n/2),rep(0,n/2))
  W <- sample(W0,n,replace = FALSE)
  
  ## heterogeneity 1 #####
  
  #M = rep(0,n)
  Y = rep(0,n)
  #Y_CT = rep(0,n)
  #M_CT = rep(0,n)
  
  indi = rep(0,n)
  for (i in 1:n){
    if(X[i,1]>0&X[i,2]>0){
      Y[i] = 1 + 1*X[i,3] + 1*X[i,4] + 1*X[i,1]*W[i] + 1*X[i,2]*W[i] + rnorm(1,0,0.01)+0.5*X[i,1]+0.5*X[i,2]
      #M[i] = 1*X[i,1]*W[i] +1*X[i,2]*W[i] + rnorm(1,0,0.01) +1*X[i,3] +1*X[i,4] +0.5*X[i,1]+0.5*X[i,2] 
      #M_CT[i] = M[i] - 1*X[i,1]*W[i] +1*X[i,2]*W[i]
      #p <- 1 / (1 + exp(-logit_p))
      #= round(p)
      indi[i]  = 1
    }else{
      Y[i] = 1 + 1*X[i,3] + 1*X[i,4]  + rnorm(1,0,0.01)+0.5*X[i,1]+0.5*X[i,2]
      #M[i] = rnorm(1,0,0.01)+1*X[i,3] + 1*X[i,4] +0.5*X[i,1]+0.5*X[i,2] 
      #M_CT[i] = M[i]
      #p <- 1 / (1 + exp(-logit_p))
      #M[i] = round(p)
    }
    #Y[i] = 1  + rnorm(1,0,0.01)+ 0.5*X[i,3] +0.5*X[i,4]
    #Y_CT[i] = Y[i] 
  }
  
  data = as.data.frame(X)
  colnames(data) = paste0("Cov",1:10)
  data$TRT = W
  #data$M = M
  data$Y = Y
  #data$M_CT = M_CT
  #data$Y_CT = Y_CT
  #data$true_indirect_effect = data$Y - data$Y_CT
  #data$M = #(data$M)
  data$heter = as.factor(indi)
  data_heter = data[data$heter==1,]
  #lm_heter = glm(M~TRT, data = data_heter, family = binomial)
  #summary(lm_heter)
  
  data0 = data[data$TRT==0,]
  data1 = data[data$TRT==1,]
  
  fit_Y_1 = rfsrc(Y~Cov1+Cov2+Cov3+Cov4+Cov5+Cov6+Cov7+Cov8+Cov9+Cov10, 
                  data = data1, ntree = 2000, var.used = "all.trees")
  fit_Y_0 = rfsrc(Y~Cov1+Cov2+Cov3+Cov4+Cov5+Cov6+Cov7+Cov8+Cov9+Cov10, 
                  data = data0, ntree = 2000, var.used = "all.trees")
  
  ## 
  #lm_1 = lm(Y~Cov1+Cov2+Cov3+Cov4+Cov5+Cov6+Cov7+Cov8+Cov9+Cov10+M,data = data1)
  #rf_y_1 = rfsrc(Y~Cov1+Cov2+Cov3+Cov4+Cov5+Cov6+Cov7+Cov8+Cov9+Cov10+M,data = data1, ntree = 2000, var.used = "all.trees")
  
  df_pred_Y_trt_1 = predict(object=fit_Y_1, newdata = data)
  pred_Y_trt_1 = df_pred_Y_trt_1$predicted
  df_pred_Y_trt_0 = predict(object=fit_Y_0, newdata = data)
  pred_Y_trt_0 = df_pred_Y_trt_0$predicted
  
  data$trt_effect = pred_Y_trt_1 - pred_Y_trt_0
  #data_trt1_m1 = data
  #data_trt1_m1$M = pred_M_trt_1
  #Y_trt1_m1 = predict(object = rf_y_1, newdata = data_trt1_m1)
  
  #data_trt1_m0 = data
  #data_trt1_m0$M = pred_M_trt_0
  #Y_trt1_m0 = predict(object = rf_y_1, newdata = data_trt1_m0)
  
  #indir_trt_effect = Y_trt1_m1$predicted - Y_trt1_m0$predicted
  #data$indir_trt_effect = indir_trt_effect
  
  #data$dif_indir_effect = data$true_indirect_effect - data$indir_trt_effect
  MAE = mean(abs(data$trt_effect))
  MSE = mean((data$trt_effect**2))
  dis3 = as.data.frame(matrix(rep(0,n*n),nrow = n))
  for (i in 1:n){
    dis3[i,] = rep(as.numeric(data[i,"trt_effect"]),n) - as.numeric(data$trt_effect)
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
  
  pval_flag = 1
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
        if(pval<pval_flag){
          pval_flag = pval
          data_flag = data_tmp
          n_cluster = j
          profile_index = k
          profiles_save = profiles
        }
      }
      
    }
    #plot_profile(profiles$trees[[1]])
    
  }
  
  data_save_path = paste0("/seed_",seed,".csv")
  rio::export(data, data_save_path)
  save_path = paste0("/seed_",seed,".rds")
  result[[1]] = MSE
  result[[2]] = MAE
  result[[3]] = data_flag
  result[[4]] = pval_flag
  result[[5]] = n_cluster
  result[[6]] = profile_index
  result[[7]] = profiles_save
  names(result) = c("mse","mae","data","pval","n_leaf","profile_index","profiles")
  rio::export(result, save_path)
}


library(Rtsne)
library(rio)
library(snowfall)
library(parallel)

sfInit(parallel = TRUE, cpus = (detectCores() - 2))
sfLibrary(dplyr)
sfLibrary(rio)
sfLibrary(rpart)
sfLibrary(randomForestSRC)
sfLibrary(base)
sfLibrary(Rtsne)
sfSource("tree_fit_part.R")

result_vector = sfLapply(1:100, f_exp1)


sfStop()
