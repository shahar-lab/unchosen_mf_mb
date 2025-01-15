# #common_MB (the combined model won't show an effect here, because the )
# df%>%filter(reoffer_ch_person==F,reoffer_common_product==T)%>%group_by(previous_common_reward)%>%summarise(mean(stay_common_product))
# 
# myprior=prior(normal(0,1),class=b)
# mb_common_b =
#   brm(
#     formula=stay_common_product~1+previous_common_reward*previous_common_exp_val+(previous_common_reward*previous_common_exp_val|subject),
#     data = df%>%filter(reoffer_ch_person==F,reoffer_common_product==T),
#     family = bernoulli(link = "logit"),
#     warmup = 1000,
#     iter = 2000,
#     chains = 4,
#     cores = 4,
#     seed = 123,
#     backend = "cmdstanr"
#   )
# save(mb_common_b,file=paste0(path$data,'/regression/model_based_common_none.rdata'))
# conditional_effects(mb_common_b)

#modulation by alpha_mb and omega
mb_common_modulation_b =
  brm(
    formula=stay_common_product~0+previous_common_reward*omega+previous_common_reward*alpha_mb+(previous_common_reward|subject),
    data = df%>%filter(reoffer_ch_person==F,reoffer_common_product==T),
    family = bernoulli(link = "logit"),
    warmup = 1000,
    iter = 2000,
    chains = 4,
    cores = 4,
    seed = 123,
    backend = "cmdstanr"
  )
save(mb_common_modulation_b,file=paste0(path$data,'/regression/model_based_common_modulation_none.rdata'))
conditional_effects(mb_common_modulation_b)

#modulation by alpha_mf and omega
myprior=prior(normal(0,1),class=b)
mf_modulation_b =
  brm(
    formula=stay_person~1+current_common_previous_reward*omega+current_common_previous_reward*alpha_mf+(current_common_previous_reward|subject),
    data = df%>%filter(reoffer_ch_person==T),
    family = bernoulli(link = "logit"),
    warmup = 1000,
    iter = 2000,
    chains = 4,
    cores = 4,
    seed = 123,
    backend = "cmdstanr"
  )
save(mf_modulation_b,file=paste0(path$data,'/regression/model_free_modulation_full.rdata'))
conditional_effects(mf_modulation_b)


mb_unique_modulation_b =
  brm(
    formula=stay_unique_product~0+previous_unique_reward*omega+previous_unique_reward*alpha_mb+(previous_unique_reward|subject),
    data = df%>%filter(reoffer_ch_person==F,reoffer_unique_ch_product==T),
    family = bernoulli(link = "logit"),
    warmup = 1000,
    iter = 2000,
    chains = 4,
    cores = 4,
    seed = 123,
    backend = "cmdstanr"
  )
save(mb_unique_modulation_b,file=paste0(path$data,'/regression/model_based_unique_modulation_full.rdata'))
conditional_effects(mb_unique_modulation_b)