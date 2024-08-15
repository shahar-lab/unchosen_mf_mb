rm(list=ls())
source('./functions/my_starter.R')
load(file="data/empirical_data/preprocessed_filtered/session3_4/df.rdata")
data_path="data/empirical_data/regression"

# baseline:mf_mb_signatures -----------------------------------------------
#All models will show these effects
#MF
df%>%filter(reoffer_ch_person==T)%>%group_by(current_common_previous_reward)%>%summarise(mean(stay_person))

myprior=prior(normal(0,1),class=b)
mf_b =
  brm(
    formula=stay_person~1+(1|subject),
    data = df%>%filter(reoffer_ch_person==T),
    family = bernoulli(link = "logit"),
    warmup = 1000,
    iter = 2000,
    chains = 4,
    cores = 4,
    seed = 123,
    backend = "cmdstanr"
  )
save(mf_b,file=paste0(data_path,'/model_free.rdata'))
conditional_effects(mf_b)
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
save(mf_modulation_b,file=paste0(data_path,'/regression/model_free_modulation.rdata'))
conditional_effects(mf_modulation_b)

#MB
#common_MB (the combined model won't show an effect here
df%>%filter(reoffer_ch_person==F,reoffer_common_product==T)%>%group_by(previous_common_reward)%>%summarise(mean(stay_common_product))

myprior=prior(normal(0,1),class=b)
mb_common_b =
  brm(
    formula=stay_common_product~1+previous_common_reward*previous_common_exp_val+(previous_common_reward*previous_common_exp_val|subject),
    data = df%>%filter(reoffer_ch_person==F,reoffer_common_product==T),
    family = bernoulli(link = "logit"),
    warmup = 1000,
    iter = 2000,
    chains = 4,
    cores = 4,
    seed = 123,
    backend = "cmdstanr"
  )
save(mb_common_b,file=paste0(data_path,'/regression/model_based_common.rdata'))
conditional_effects(mb_common_b)
#unique_mb
df%>%filter(reoffer_ch_person==F,reoffer_unch_person==F,reoffer_unique_ch_product==T)%>%group_by(previous_unique_reward)%>%summarise(mean(stay_unique_product))

mb_unique_b =
  brm(
    formula=stay_unique_product~0+previous_unique_reward+(previous_unique_reward|subject),
    data = df%>%filter(reoffer_ch_person==F,reoffer_unique_ch_product==T),
    family = bernoulli(link = "logit"),
    warmup = 1000,
    iter = 2000,
    chains = 4,
    cores = 4,
    seed = 123,
    backend = "cmdstanr"
  )
save(mb_unique_b,file=paste0(data_path,'/regression/model_based_unique.rdata'))
conditional_effects(mb_unique_b)
# MF_unch -----------------------------------------------------------------

df%>%filter(reoffer_unch_person==T,reoffer_ch_person==F)%>%group_by(previous_unique_reward)%>%summarise(mean(ch_prev_unchosen))

mf_unch_b =
  brm(
    formula=ch_prev_unchosen~previous_unique_reward+previous_common_reward+(previous_unique_reward+previous_common_reward||subject),
    data = df%>%filter(reoffer_unch_person==T,reoffer_ch_person==F),
    family = bernoulli(link = "logit"),
    warmup = 1000,
    iter = 2000,
    chains = 4,
    cores = 4,
    seed = 123,
    backend = "cmdstanr"
  )
save(mf_unch_b,file=paste0(data_path,'/regression/model_free_unch_mf.rdata'))
conditional_effects(mf_unch_b)


# MB_unch -----------------------------------------------------------------

df%>%filter(reoffer_unch_person==F,reoffer_ch_person==F,reoffer_unique_unch_product==T)%>%group_by(previous_unique_reward)%>%summarise(mean(ch_prev_unch_unique_product))

mb_unch_b =
  brm(
    formula=ch_prev_unch_unique_product~previous_unique_reward+scaled_expval_unique_unch+(previous_unique_reward||subject),
    data = df%>%filter(reoffer_unch_person==F,reoffer_ch_person==F,reoffer_unique_unch_product==T),
    family = bernoulli(link = "logit"),
    warmup = 1000,
    iter = 2000,
    chains = 4,
    cores = 4,
    seed = 123,
    backend = "cmdstanr"
  )
save(mb_unch_b,file=paste0(data_path,'/regression/model_based_unch.rdata'))
conditional_effects(mb_unch_b)

#modulation

#modulation by alpha_mb and omega
# mb_common_modulation_b =
#   brm(
#     formula=stay_common_product~0+previous_common_reward*omega+previous_common_reward*alpha_mb+(previous_common_reward|subject),
#     data = df%>%filter(reoffer_ch_person==F,reoffer_common_product==T),
#     family = bernoulli(link = "logit"),
#     warmup = 1000,
#     iter = 2000,
#     chains = 4,
#     cores = 4,
#     seed = 123,
#     backend = "cmdstanr"
#   )
# save(mb_common_modulation_b,file=paste0(data_path,'/regression/model_based_common_modulation.rdata'))
# conditional_effects(mb_common_modulation_b)
# 
# mb_unique_modulation_b =
#   brm(
#     formula=stay_unique_product~0+previous_unique_reward*omega+previous_unique_reward*alpha_mb+(previous_unique_reward|subject),
#     data = df%>%filter(reoffer_ch_person==F,reoffer_unique_ch_product==T),
#     family = bernoulli(link = "logit"),
#     warmup = 1000,
#     iter = 2000,
#     chains = 4,
#     cores = 4,
#     seed = 123,
#     backend = "cmdstanr"
#   )
# save(mb_unique_modulation_b,file=paste0(data_path,'/regression/model_based_unique_modulation.rdata'))
# conditional_effects(mb_unique_modulation_b)
#modulation by alpha_mf_unch
# mf_unch_modulation_b =
#   brm(
#     formula=ch_prev_unchosen~previous_unique_reward*c_mf_unch+(previous_unique_reward||subject),
#     data = df%>%filter(reoffer_unch_person==T,reoffer_ch_person==F),
#     family = bernoulli(link = "logit"),
#     warmup = 1000,
#     iter = 2000,
#     chains = 4,
#     cores = 4,
#     seed = 123,
#     backend = "cmdstanr"
#   )
# save(mf_unch_modulation_b,file=paste0(data_path,'/regression/model_free_unch_modulation.rdata'))
# conditional_effects(mf_unch_modulation_b)
#modulation by alpha_mb_unch
# mb_unch_modulation_b =
#   brm(
#     formula=ch_prev_unch_unique_product~previous_unique_reward*c_mb_unch+(previous_unique_reward||subject),
#     data = df%>%filter(reoffer_unch_person==F,reoffer_ch_person==F,reoffer_unique_unch_product==T),
#     family = bernoulli(link = "logit"),
#     warmup = 1000,
#     iter = 2000,
#     chains = 4,
#     cores = 4,
#     seed = 123,
#     backend = "cmdstanr"
#   )
# save(mb_unch_modulation_b,file=paste0(data_path,'/regression/model_based_unch_modulation.rdata'))
# conditional_effects(mb_unch_modulation_b)
