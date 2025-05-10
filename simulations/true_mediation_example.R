### true mediation proportion ###
### Scenario 1 ###

df = data.frame(index=1:100,p=rep(0,100),prop=rep(0,100),n = rep(0,100))
for (seed in 1:100){
  cat(seed)
  data_path = paste0("~path/expe1_new/seed_",seed,".csv")
  data = rio::import(data_path)
  data_tmp = data[data$heter==1,]
  mediator_model <- lm(M ~ TRT , data = data_tmp)
  outcome_model <- lm(Y ~ TRT + M, data = data_tmp)
  mediation_result <- mediate(mediator_model, outcome_model, treat = "TRT", mediator = "M", boot = TRUE, sims = 1000)
  ans = summary(mediation_result)
  med_pro = c(ans$n0, med_pro)
  med_p = c(ans$n0.p, med_p)
  n_list = c(dim(data_tmp)[1], n_list)
  
  index = which(med_p==min(med_p))
  
  df[seed,"p"] = ans$n0.p
  df[seed,"prop"] = ans$n0
  df[seed,"n"] = dim(data_tmp)[1]#n_list[index][1]
}
df_path = paste0("~path/","expe1_true",".csv")
df = rio::import(df_path)
rio::export(df, df_path)




