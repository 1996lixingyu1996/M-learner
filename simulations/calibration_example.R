############# Calibration with Mediator Random Forest ##########
library(tidyverse)
library(latex2exp)
###############  Random Forest ###########
# 1st load situation 1 p leaf

v1 = NULL
v2 = NULL
v3 = NULL
p_list = NULL
n_list = NULL
for (i in 1:100){
  seed = i
  folder_path = ""
  path = paste0(folder_path, "seed_xgb_",seed,".rds") # / seed_ for RF
  result = rio::import(path)
  profiles = result[[4]]
  index = result[[3]]
  profile = profiles$trees[[index]]
  var_importance = profile$variable.importance
  selected_var = names(var_importance)
  if("Cov1"%in%selected_var){
    v1 = c(v1,i)
  }
  if("Cov2"%in%selected_var){
    v2 = c(v2,i)
  }
  if("Cov2"%in%selected_var&"Cov1"%in%selected_var){
    v3 = c(v3,i)
  }
  p_list = c(p_list,result[[1]])
  n_list = c(n_list, length(selected_var))
  # plot_profile(profile)
}

df = data.frame(p = p_list, v1 = rep(0,100), v2 = rep(0,100), v3 = rep(0,100),n = rep(0,100), s = rep(0,100))
for (i in 1:100){
  if(i%in%v1){
    df[i,"v1"] = 1
  }
  if(i%in%v2){
    df[i,"v2"] = 1
  }
  if(i%in%v3){
    df[i,"v3"] = 1
  }
  df[i,"n"] = n_list[i]
  if(df[i,"p"]<0.0043){
    df[i,"s"] = 1
  }
}
df_s = df[df$s==1,]
sum(df_s$v1)
sum(df_s$v2)
sum(df_s$v3)
summary(as.factor(df_s$n))
dim(df_s)

p_list_no_mediator_sit1 = p_list  # Simple
p_list_no_mediator_sit2 = p_list  # 50% Mediation
p_list_no_mediator_sit3 = p_list  # No mediation effect Sit1
p_list_no_mediator_sit4 = p_list  # No mediation effect Sit2
p_list_no_mediator_sit5 = p_list  # Global
p_list_no_mediator_sit6 = p_list  # complex all
p_list_no_mediator_sit7 = p_list  # complex 50% mediation effect

p_df = data.frame(v1 = rep(1,100), v2 = rep(0,100),v3 = rep(1,100),v4 = rep(1,100),v5 = rep(1,100),v6 = rep(1,100),v7 = rep(1,100))
#p_df$v1 = rep(0,100)

#= data.frame()
p_df$v1 = p_list_no_mediator_sit1
p_df$v2 = p_list_no_mediator_sit2
p_df$v3 = p_list_no_mediator_sit3
p_df$v4 = p_list_no_mediator_sit4
p_df$v5 = p_list_no_mediator_sit5
p_df$v6 = p_list_no_mediator_sit6
p_df$v7 = p_list_no_mediator_sit7

rio::export(p_df,"")

#p_df = rio::import("/Users/xli36/Downloads/R_packages/myproject/figure/xgb_p.csv")

ecdf_1 = ecdf(p_df$v1)
ecdf_2 = ecdf(p_df$v2)
ecdf_3 = ecdf(p_df$v3)
ecdf_4 = ecdf(p_df$v4)
ecdf_5 = ecdf(p_df$v5)
ecdf_6 = ecdf(p_df$v6)
ecdf_7 = ecdf(p_df$v7)



df = data.frame(p = p_list, v1 = rep(0,100), v2 = rep(0,100), v3 = rep(0,100),n = rep(0,100), s = rep(0,100))
for (i in 1:100){
  if(i%in%v1){
    df[i,"v1"] = 1
  }
  if(i%in%v2){
    df[i,"v2"] = 1
  }
  if(i%in%v3){
    df[i,"v3"] = 1
  }
  df[i,"n"] = n_list[i]
  if(df[i,"p"]<p**){##(p** is threshold)
    df[i,"s"] = 1
  }
}
df_s = df[df$s==1,]
sum(df_s$v1)
sum(df_s$v2)
sum(df_s$v3)
summary(as.factor(df_s$n))


p_ecdf <- ggplot()+
  geom_function(fun = ecdf_1, lwd=1,aes(color = "Simple-All"))+
  geom_function(fun = ecdf_2, lwd=1,aes(color = "Simple-Part"))+
  geom_function(fun = ecdf_6, lwd=1,aes(color = "Complex-All"))+
  geom_function(fun = ecdf_7, lwd=1,aes(color = "Complex-Part"))+
  geom_function(fun = ecdf_3, lwd=1,aes(color = "Simple-Null1"))+
  geom_function(fun = ecdf_4, lwd=1,aes(color = "Simple-Null2"))+
  geom_function(fun = ecdf_5, lwd=1,aes(color = "Simple-Global"))+
  ggtitle("ECDF in different scenarios with XGB/RF learner")+theme(plot.title = element_text(hjust = 0.5))+
  guides(colour = guide_legend(title = ""))+
  xlab(TeX("$p_{leaf}$")) + ylab("Cumulative Probability")+xlim(0, 0.1)+
  theme_minimal()+ theme(plot.title = element_text(hjust = 0.5))





summary(p_list_no_mediator_sit4)
quantile(p_list_no_mediator_sit4,probs = c(0.1,0.05,0.01))


