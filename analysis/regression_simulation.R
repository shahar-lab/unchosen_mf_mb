
rm(list=ls())
source('./functions/my_starter.R')
path = set_workingmodel()
load(paste0(path$data,"/simulate_empirical/avatars.rdata"))

#preprocessing
df=df%>%mutate(reoffer_ch_person=(lag(ch_person)==person1)|(lag(ch_person)==person2),
               reoffer_unch_person=(lag(unch_person)==person1)|(lag(unch_person)==person2),
               reoffer_common_product=(lag(common_product)==common_product)|(lag(common_product)==unique_ch_product)|(lag(common_product)==unique_unch_product),
               reoffer_unique_ch_product=(lag(unique_ch_product)==common_product)|(lag(unique_ch_product)==unique_ch_product)|(lag(unique_ch_product)==unique_unch_product),
               reoffer_unique_unch_product=(lag(unique_unch_product)==common_product)|(lag(unique_unch_product)==unique_ch_product)|(lag(unique_unch_product)==unique_unch_product),
               stay_person=lag(ch_person)==ch_person,
               ch_prev_unchosen=lag(unch_person)==ch_person,
               stay_common_product=(lag(common_product)==common_product)|(lag(common_product)==unique_ch_product),
               stay_unique_product=(lag(unique_ch_product)==common_product)|(lag(unique_ch_product)==unique_ch_product),
               ch_prev_unch_unique_product=(lag(unique_unch_product)==common_product)|(lag(unique_unch_product)==unique_ch_product),
               total_reward=common_reward+unique_reward,
               previous_total_reward=factor(lag(total_reward)),
               previous_common_reward=factor(lag(common_reward)),
               previous_unique_reward=factor(lag(unique_reward)),
               current_common_previous_reward=factor(if_else(common_product==lag(common_product),
                                                    previous_common_reward,previous_unique_reward)),
               previous_common_exp_val=lag(expval_common)-0.5,
               previous_unique_ch_exp_val=lag(expval_unique_ch)-0.5,
               
               scaled_expval_unique_unch=expval_unique_unch-0.5,
               previous_unique_unch_exp_val=lag(scaled_expval_unique_unch),
               diff_prev_exp_val=expval_unique_ch-expval_unique_unch)
# baseline:mf_mb_signatures -----------------------------------------------
#All models will show these effects
#MF
df%>%filter(reoffer_ch_person==T)%>%group_by(current_common_previous_reward)%>%summarise(mean(stay_person))

myprior=prior(normal(0,1),class=b)
mf_b =
  brm(
    formula=stay_person~1+current_common_previous_reward+(1+current_common_previous_reward|subject),
    data = df%>%filter(reoffer_ch_person==T),
    family = bernoulli(link = "logit"),
    warmup = 1000,
    iter = 2000,
    chains = 4,
    cores = 4,
    seed = 123,
    backend = "cmdstanr"
  )
save(mf_b,file=paste0(path$data,'/regression/model_free_simul.rdata'))
conditional_effects(mf_b)


# #MB

#unique_mb
df%>%filter(reoffer_ch_person==F,reoffer_unique_ch_product==T)%>%group_by(previous_unique_reward)%>%summarise(mean(stay_unique_product))

mb_unique_b =
  brm(
    formula=stay_unique_product~0+previous_unique_reward+previous_unique_ch_exp_val+(previous_unique_reward|subject),
    data = df%>%filter(reoffer_ch_person==F,reoffer_unique_ch_product==T),
    family = bernoulli(link = "logit"),
    warmup = 1000,
    iter = 2000,
    chains = 4,
    cores = 4,
    seed = 123,
    backend = "cmdstanr"
  )
save(mb_unique_b,file=paste0(path$data,'/regression/model_based_unique_simul.rdata'))
conditional_effects(mb_unique_b)

# MF_unch -----------------------------------------------------------------

df%>%filter(reoffer_unch_person==T,reoffer_ch_person==F)%>%group_by(previous_unique_reward)%>%summarise(mean(ch_prev_unchosen))

mf_unch_b =
  brm(
    formula=ch_prev_unchosen~previous_unique_reward+previous_common_reward+(previous_unique_reward+previous_common_reward||subject),
    data = df%>%filter(reoffer_unch_person==T),
    family = bernoulli(link = "logit"),
    warmup = 1000,
    iter = 2000,
    chains = 4,
    cores = 4,
    seed = 123,
    backend = "cmdstanr"
  )
save(mf_unch_b,file=paste0(path$data,'/regression/model_free_unch_simul.rdata'))
conditional_effects(mf_unch_b)

# #modulation by alpha_mf_unch
# mf_unch_modulation_b =
#   brm(
#     formula=ch_prev_unchosen~previous_unique_reward*c_mf_unch+(previous_unique_reward||subject),
#     data = df%>%filter(reoffer_unch_person==T),
#     family = bernoulli(link = "logit"),
#     warmup = 1000,
#     iter = 2000,
#     chains = 4,
#     cores = 4,
#     seed = 123,
#     backend = "cmdstanr"
#   )
# save(mf_unch_modulation_b,file=paste0(path$data,'/regression/model_free_unch_modulation_mf.rdata'))
# conditional_effects(mf_unch_modulation_b)

# MB_unch -----------------------------------------------------------------
# 
#df%>%filter(reoffer_unch_person==F,reoffer_ch_person==F,reoffer_unique_unch_product==T)%>%group_by(previous_unique_reward)%>%summarise(mean(ch_prev_unch_unique_product))

mb_unch_b =
  brm(
    formula=ch_prev_unch_unique_product~previous_unique_reward+previous_unique_unch_exp_val+(previous_unique_reward||subject),
    data = df%>%filter(reoffer_unch_person==F,reoffer_ch_person==F,reoffer_unique_unch_product==T),
    family = bernoulli(link = "logit"),
    warmup = 1000,
    iter = 2000,
    chains = 4,
    cores = 4,
    seed = 123,
    backend = "cmdstanr"
  )
save(mb_unch_b,file=paste0(path$data,'/regression/model_based_unch_simul.rdata'))
# conditional_effects(mb_unch_b)

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
# save(mb_unch_modulation_b,file=paste0(path$data,'/regression/model_based_unch_modulation_simul.rdata'))
# conditional_effects(mb_unch_modulation_b)






