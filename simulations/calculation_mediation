#### calculating mediation proportion in mediatioin model ######

df = data.frame(index=1:100,p=rep(0,100),prop=rep(0,100),n = rep(0,100))
for (seed in 1:100){
  #cat(seed)
  # read selected profile path
  save_path = paste0("",seed,".rds")
  result = rio::import(save_path)
  
  index_M = result[[3]]
  profile = result[[4]]$trees[[index_M]]
  
  data_path = paste0("/seed_",seed,".csv")  # data path
  data = rio::import(data_path)
  data = predict_path(profile, newdata=data)
  
  level_names = names(summary(data$leaf))
  #plot_profile(profile)
  med_pro = NULL
  med_p = NULL
  n_list = NULL
  for(name in level_names){
    data_tmp = data[data$leaf==name,]
    mediator_model <- lm(M ~ TRT , data = data_tmp)
    outcome_model <- lm(Y ~ TRT + M, data = data_tmp)
    mediation_result <- mediate(mediator_model, outcome_model, treat = "TRT", mediator = "M", boot = TRUE, sims = 1000)
    ans = summary(mediation_result)
    med_pro = c(ans$n0, med_pro)
    med_p = c(ans$n0.p, med_p)
    n_list = c(dim(data_tmp)[1], n_list)
  }
  
  index = which(med_p==min(med_p))
  
  df[seed,"p"] = min(med_p)[1]
  df[seed,"prop"] = med_pro[index][1]
  df[seed,"n"] = n_list[index][1]
}

colnames(df)
mean(df$prop)
sd(df$prop)
mean(df$n)
sd(df$n)
