#This code plot recovered parameters against the true parameters

rm(list=ls())
source('./functions/my_starter.R')
path = set_workingmodel()
#--------------------------------------------------------------------------------------------------------
#load recovered parameters
fit=readRDS(paste0(path$data,'/modelfit_empirical.rds'))

c_mf_unch=apply(fit$draws(variables ='c_mf_unch_sbj' ,format='draws_matrix'), 2, mean)
c_mb_unch=apply(fit$draws(variables ='c_mb_unch_sbj' ,format='draws_matrix'), 2, mean)

load("data/empirical_data/regression/model_free_unch.rdata")
load("data/empirical_data/regression/model_based_unch.rdata")

coefs_mf=coef(mf_unch_b)$subject[,1,2]
coefs_mb=coef(mb_unch_b)$subject[,1,2]


mf=data.frame(coefs_mf,c_mf_unch)
mb=data.frame(coefs_mb,c_mb_unch)

lm_fit <- lm(coefs_mf ~ c_mf_unch, data = mf)
mf_intercept <- coef(lm_fit)["(Intercept)"]

lm_fit <- lm(coefs_mb ~ c_mb_unch, data = mb)
mb_intercept <- coef(lm_fit)["(Intercept)"]


mf%>%ggplot(aes(x=c_mf_unch,y=coefs_mf))+geom_point()+geom_smooth(method="lm")+ylab("mf_unch_regression_coef")+xlab("c_mf_unch_model_parameter")+
  scale_x_continuous(limits=c(-0.9,0.7))+scale_y_continuous(limits=c(-.6,0.6))+geom_vline(xintercept = 0, color = "darkgreen") +  # Vertical line
  geom_hline(yintercept = 0, , color = "darkgreen")+geom_hline(yintercept = mf_intercept, , color = "red")+
  geom_abline(intercept = mf_intercept, slope = 1, linetype = "dashed", color = "blue")

  
mb%>%ggplot(aes(x=c_mb_unch,y=coefs_mb))+geom_point()+geom_smooth(method="lm")+ylab("mb_unch_regression_coef")+xlab("c_mb_unch_model_parameter")+
  scale_x_continuous(limits=c(-0.9,0.7))+scale_y_continuous(limits=c(-.6,0.6))+geom_vline(xintercept = 0, color = "darkgreen") +  # Vertical line
  geom_hline(yintercept = 0, , color = "darkgreen")+geom_hline(yintercept = mb_intercept, , color = "red")+
  geom_abline(intercept = mb_intercept, slope = 1, linetype = "dashed", color = "blue")


# simulated data ----------------------------------------------------------

#load simulated data from simulate_empirical
load(file=paste0(path$data,'/regression/model_free_unch_simul.rdata'))
load(file=paste0(path$data,'/regression/model_based_unch_simul.rdata'))

reg_mf_unch=coef(mf_unch_b)$subject[,1,2]
reg_mb_unch=coef(mb_unch_b)$subject[,1,2]

load(file=paste0(path$data,'/simulate_empirical/model_parameters.rdata'))
c_mf_unch_simul=model_parameters$artificial_individual_parameters[,3]
c_mb_unch_simul=model_parameters$artificial_individual_parameters[,4]

cor(reg_mf_unch,c_mf_unch_simul)
cor(reg_mb_unch,c_mb_unch_simul)

coefs_mf=coef(mf_unch_b)$subject[,1,2]
coefs_mb=coef(mb_unch_b)$subject[,1,2]

mf_simul=data.frame(coefs_mf=reg_mf_unch,c_mf_unch=c_mf_unch_simul)
mb_simul=data.frame(coefs_mb=reg_mb_unch,c_mb_unch=c_mb_unch_simul)

lm_simul_fit_mf=lm(coefs_mf ~ c_mf_unch, data = mf_simul)
mf_simul_intercept=coef(lm_simul_fit_mf)["(Intercept)"]

lm_simul_fit_mb=lm(coefs_mb ~ c_mb_unch, data = mb_simul)
mb_simul_intercept=coef(lm_simul_fit_mb)["(Intercept)"]

mf_simul%>%ggplot(aes(x=c_mf_unch,y=coefs_mf))+geom_point()+geom_smooth(method="lm")+ylab("mf_unch_regression_coef")+xlab("c_mf_unch_model_parameter")+
  scale_x_continuous(limits=c(-0.9,0.7))+scale_y_continuous(limits=c(-.6,0.6))+geom_vline(xintercept = 0,, color = "darkgreen") +  # Vertical line
  geom_hline(yintercept = 0, , color = "darkgreen")+geom_hline(yintercept = mf_simul_intercept, , color = "red")+
  geom_abline(intercept = mf_simul_intercept, slope = 1, linetype = "dashed", color = "blue")

mb_simul%>%ggplot(aes(x=c_mb_unch,y=coefs_mb))+geom_point()+geom_smooth(method="lm")+ylab("mf_unch_regression_coef")+xlab("c_mf_unch_model_parameter")+
  scale_x_continuous(limits=c(-0.9,0.7))+scale_y_continuous(limits=c(-.6,0.6))+geom_vline(xintercept = 0, color = "darkgreen") +  # Vertical line
  geom_hline(yintercept = 0, , color = "darkgreen")+geom_hline(yintercept = mb_simul_intercept, , color = "red")+
  geom_abline(intercept = mb_simul_intercept, slope = 1, linetype = "dashed", color = "blue")



# create avatars ----------------------------------------------------------

#get individual parameters from modelfit_empirical.rds
fit=readRDS(paste0(path$data,'/modelfit_empirical.rds'))
#extract all individual parameters
c_mf=apply(fit$draws(variables ='c_mf_sbj' ,format='draws_matrix'), 2, mean)
c_mb=apply(fit$draws(variables ='c_mb_sbj' ,format='draws_matrix'), 2, mean)
c_mf_unch=apply(fit$draws(variables ='c_mf_unch_sbj' ,format='draws_matrix'), 2, mean)
c_mb_unch=apply(fit$draws(variables ='c_mb_unch_sbj' ,format='draws_matrix'), 2, mean)
f_mf=apply(fit$draws(variables ='f_mf_sbj' ,format='draws_matrix'), 2, mean)
f_mb=apply(fit$draws(variables ='f_mb_sbj' ,format='draws_matrix'), 2, mean)
pr=apply(fit$draws(variables ='pr_sbj' ,format='draws_matrix'), 2, mean)
f_p=apply(fit$draws(variables ='f_pr_sbj' ,format='draws_matrix'), 2, mean)

#put in a data frame
params=data.frame(c_mf,c_mb,c_mf_unch,c_mb_unch,f_mf,f_mb,pr,f_p)

#get correlation matrix
cor(params)

#simulate data
cfg = list(
  Nsubjects        = length(params),
  Nblocks          = 2,
  Ntrials_perblock = 200,
  Npersons         = 5, #number of arms in the task
  Nproducts         = 5, #number of arms offered for selection each trial
  Nraffle          = 2,
  rndwlk1          = rndwlk1,
  rndwlk2          = rndwlk2,
  rndwlk3          = rndwlk3,
  rndwlk4          = rndwlk4
)
Nsubjects=nrow(params)
load("functions/rndwlk1.rdata")
load("functions/rndwlk2.rdata")
load("functions/rndwlk3.rdata")
load("functions/rndwlk4.rdata")
path = set_workingmodel()
source(paste0(path$model,'.r'))
df<-
  foreach(
    subject = 1:Nsubjects,
    .combine = rbind
  ) %do% {
    sim.block(subject=subject, 
              parameters=params[subject,],
              cfg=cfg)
  }
df=df_avatars
save(df,file=paste0(path$data,'/simulate_empirical/avatars.rdata'))

#load empirical regression of mf_unch and mb_unch
load("data/empirical_data/regression/model_free_unch.rdata")
load("data/empirical_data/regression/model_based_unch.rdata")

#get individual coefficients for each subject
coefs_mf=coef(mf_unch_b)$subject[,1,2]
coefs_mb=coef(mb_unch_b)$subject[,1,2]
#load simulated regression of mf_unch and mb_unch
load(file=paste0(path$data,'/regression/model_free_unch_simul.rdata'))
load(file=paste0(path$data,'/regression/model_based_unch_simul.rdata'))
#get individual coefficients for each subject
sim_coefs_mf=coef(mf_unch_b)$subject[,1,2]
sim_coefs_mb=coef(mb_unch_b)$subject[,1,2]

#plot mf_unch
mf=data.frame(coefs_mf,sim_coefs_mf)
lm_fit <- lm(coefs_mf ~ sim_coefs_mf, data = mf)

mf%>%ggplot(aes(x=sim_coefs_mf,y=coefs_mf))+geom_point()+geom_smooth(method="lm")+ylab("mf_unch_regression_coef")+xlab("mf_unch_regression_coef_simulated")

#plot mb_unch
mb=data.frame(coefs_mb,sim_coefs_mb)
lm_fit <- lm(coefs_mb ~ sim_coefs_mb, data = mb)
mb%>%ggplot(aes(x=sim_coefs_mb,y=coefs_mb))+geom_point()+geom_smooth(method="lm")+ylab("mb_unch_regression_coef")+xlab("mb_unch_regression_coef_simulated")
