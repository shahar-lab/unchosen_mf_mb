rm(list=ls())
source('./functions/my_packages.R')
source('./functions/my_starter.R')
#load empirical fit and data
fit=readRDS(paste0(path$data,'/modelfit_recovery.rds'))
#load simulated like
load('C:/Users/Ido/My Drive/R/unchosen_mf_mb/data/model_recovery/modelfit_like_per_trial_recovery_mb_mf.Rdata')
#load data
load(paste0(path$data,'/artificial_data.Rdata'))
#remove missing trials
# p_ch_action[p_ch_action==0]=NA
# p_ch_action = p_ch_action[, !colSums(is.na(p_ch_action))]

#transform to aligned df
# p_ch_action=as.data.frame(t(p_ch_action))

#calculate roc
p_ch_action=exp(like)
#print example
df_test=df%>%filter(first_trial_in_block==0)
roc=roc(rep(1,79600),p_ch_action[3000,])
#roc=roc(df_test$selected_offer,p_ch_action[3000,])
plot(roc)
auc(roc)
#iterate on all samples
Nsamples=nrow(p_ch_action)
auc_list=list()
auc_all_list = list()
#auc_unchosen_list=list()
#you can add a grouping variable to compare your fit for different conditions
# filter_variable=df$reoffer_ch==F&df$reoffer_unch==T
for (sample in 1:Nsamples){
  print(paste0("sample: ",sample))
  roc=roc(df_test%>%pull(selected_offer),p_ch_action[sample,])
  auc=auc(roc)
  auc_list=append(auc_list,auc)
  roc_all=roc(df_test%>%pull(selected_offer),p_ch_action[,sample])
  auc_all=auc(roc_all)
  auc_all_list=append(auc_all_list,auc_all)
}
posterior_unchosen_auc=unlist(auc_unchosen_list)
hist(posterior_unchosen_auc)
posterior_all_auc=unlist(auc_all_list)
hist(posterior_all_auc)

save(posterior_auc,file=paste0(path$data,'/auc.Rdata'))