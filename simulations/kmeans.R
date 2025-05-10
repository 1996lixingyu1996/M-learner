##### Experiment 1 Kmeans ######
## Mediator Model
##### Indirect experiment ########

f_exp1 <- function(seed){
  #n= 1000
  set.seed(seed)
  
  data_save_path = paste0("~path/seed_",seed,".csv")
  data = rio::import(data_save_path)
  
  feature = data[,1:10]
  kmeans_result_2 = kmeans(feature, centers = 2, iter.max = 50, nstart = 10)
  kmeans_result_3 = kmeans(feature, centers = 3, iter.max = 50, nstart = 10)
  kmeans_result_4 = kmeans(feature, centers = 4, iter.max = 50, nstart = 10)
  kmeans_result_5 = kmeans(feature, centers = 5, iter.max = 50, nstart = 10)
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
          level_names = names(summary(data_tmp$leaf))
          #plot_profile(profile)
          med_pro = NULL
          med_p = NULL
          n_list = NULL
          for(name in level_names){
            data_tmp1 = data_tmp[data_tmp$leaf==name,]
            mediator_model <- lm(M ~ TRT , data = data_tmp1)
            outcome_model <- lm(Y ~ TRT + M, data = data_tmp1)
            mediation_result <- mediate(mediator_model, outcome_model, treat = "TRT", mediator = "M", boot = TRUE, sims = 1000)
            ans = summary(mediation_result)
            med_pro = c(ans$n0, med_pro)
            med_p = c(ans$n0.p, med_p)
            n_list = c(dim(data_tmp1)[1], n_list)
          }
          
          index = which(med_p==min(med_p))
          
          #df[seed,"p"] = min(med_p)[1]
          #df[seed,"prop"] = med_pro[index][1]
          #df[seed,"n"] = n_list[index][1]
        }
      }
    }
    
    
    
    #plot_profile(profiles$trees[[1]])
    
  }
  
  save_path = paste0("/seed_kmeans_",seed,".rds")
  result[[1]] = pval_flag_M
  result[[2]] = n_cluster_M
  result[[3]] = profile_index_M#data_flag
  result[[4]] = profiles_save_M
  result[[5]] = pval_flag_Y
  result[[6]] = n_cluster_Y
  result[[7]] = profile_index_Y
  result[[8]] = profiles_save_Y
  result[[9]] = min(med_p)[1]
  result[[10]] =  med_pro[index][1]
  result[[11]] = n_list[index][1]
  names(result) = c("pval_M","n_cluster_M","profile_index_M","profiles_save_M","pval_flag_Y","n_cluster_Y","profile_index_Y",
                    "profiles_save_Y","p_mediation","mediation_proportion","sample_size")
  rio::export(result, save_path)
}


library(Rtsne)
library(rio)
library(snowfall)
library(parallel)

my_new_folder = ""
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
sfLibrary(mediation)
sfSource("tree_fit_part.R")

result_vector = sfLapply(1:100, f_exp1)


sfStop()
