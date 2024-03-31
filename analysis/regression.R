
rm(list=ls())
#load any of the four models 
path = set_workingmodel()
load(paste0(path$data,"/artificial_data.Rdata"))

#preprocessing
df=df%>%mutate(reoffer_ch_person=(lag(ch_person)==person1)|(lag(ch_person)==person2),
               reoffer_unch_person=(lag(unch_person)==person1)|(lag(unch_person)==person2),
               reoffer_common_object=(lag(common_object)==common_object)|(lag(common_object)==unique_ch_object)|(lag(common_object)==unique_unch_object),
               reoffer_unique_ch_object=(lag(unique_ch_object)==common_object)|(lag(unique_ch_object)==unique_ch_object)|(lag(unique_ch_object)==unique_unch_object),
               reoffer_unique_unch_object=(lag(unique_unch_object)==common_object)|(lag(unique_unch_object)==unique_ch_object)|(lag(unique_unch_object)==unique_unch_object),
               stay_person=lag(ch_person)==ch_person,
               ch_prev_unchosen=lag(unch_person)==ch_person,
               stay_common_object=(lag(common_object)==common_object)|(lag(common_object)==unique_ch_object),
               stay_unique_object=(lag(unique_ch_object)==common_object)|(lag(unique_ch_object)==unique_ch_object),
               ch_prev_unch_unique_object=(lag(unique_unch_object)==common_object)|(lag(unique_unch_object)==unique_ch_object),
               total_reward=common_reward+unique_reward,
               previous_total_reward=factor(lag(total_reward)),
               previous_common_reward=factor(lag(common_reward)),
               previous_unique_reward=factor(lag(unique_reward)))
# baseline:mf_mb_signatures -----------------------------------------------
#All models will show these effects
#MF
df%>%filter(reoffer_ch_person==T)%>%group_by(previous_total_reward)%>%summarise(mean(stay_person))
mf=glmer(stay_person~previous_total_reward+(previous_total_reward|subject),data=df%>%filter(reoffer_ch_person==T),family=binomial(link="logit"))
summary(mf)

#modulation by alpha_mf and omega
mf_modulation=glmer(stay_person~previous_total_reward*omega+previous_total_reward*alpha_mf+(previous_total_reward|subject),data=df%>%filter(reoffer_ch_person==T),family=binomial(link="logit"))
summary(mf_modulation)
plot(effects::effect("previous_total_reward:alpha_mf",mf_modulation))
plot(effects::effect("previous_total_reward:omega",mf_modulation))

#MB
#common_MB (the combined model won't show an effect here, because the )
df%>%filter(reoffer_ch_person==F,reoffer_common_object==T)%>%group_by(previous_common_reward)%>%summarise(mean(stay_common_object))
mb_common=glmer(stay_common_object~previous_common_reward+(previous_common_reward|subject),data=df%>%filter(reoffer_ch_person==F,reoffer_common_object==T),family=binomial(link="logit"))
summary(mb_common)

#unique_mb
df%>%filter(reoffer_ch_person==F,reoffer_unch_person==F,reoffer_unique_ch_object==T)%>%group_by(previous_unique_reward)%>%summarise(mean(stay_unique_object))
mb_unique=glmer(stay_unique_object~previous_unique_reward+(previous_unique_reward|subject),data=df%>%filter(reoffer_ch_person==F,reoffer_unique_ch_object==T),family=binomial(link="logit"))
summary(mb_unique)

#modulation by alpha_mb and omega
mb_common_modulation=glmer(stay_common_object~previous_common_reward*alpha_mb+previous_common_reward*omega+(previous_common_reward|subject),data=df%>%filter(reoffer_ch_person==F,reoffer_common_object==T),family=binomial(link="logit"))
summary(mb_common_modulation)
plot(effects::effect("previous_common_reward:alpha_mb",mb_common_modulation))
plot(effects::effect("previous_common_reward:omega",mb_common_modulation))

mb_unique_modulation=glmer(stay_unique_object~previous_unique_reward*alpha_mb+previous_unique_reward*omega+(previous_unique_reward|subject),data=df%>%filter(reoffer_ch_person==F,reoffer_unique_ch_object==T),family=binomial(link="logit"))
summary(mb_unique_modulation)
plot(effects::effect("previous_unique_reward:alpha_mb",mb_unique_modulation))
plot(effects::effect("previous_unique_reward:omega",mb_unique_modulation))

# MF_unch -----------------------------------------------------------------

df%>%filter(reoffer_unch_person==T,reoffer_ch_person==F)%>%group_by(previous_unique_reward)%>%summarise(mean(ch_prev_unchosen))
mf_unch=glmer(ch_prev_unchosen~previous_unique_reward+(previous_unique_reward|subject),data=df%>%filter(reoffer_unch_person==T,reoffer_ch_person==F),family=binomial(link="logit"))
summary(mf_unch)

#modulation by alpha_mf_unch
mf_unch_modulation=glmer(ch_prev_unchosen~previous_unique_reward*alpha_mf_unch+(previous_unique_reward|subject),data=df%>%filter(reoffer_unch_person==T,reoffer_ch_person==F),family=binomial(link="logit"))
summary(mf_unch_modulation)
plot(effects::effect("previous_unique_reward:alpha_mf_unch",mf_unch_modulation))


# MB_unch -----------------------------------------------------------------

df%>%filter(reoffer_unch_person==F,reoffer_ch_person==F,reoffer_unique_unch_object==T)%>%group_by(previous_unique_reward)%>%summarise(mean(ch_prev_unch_unique_object))
mb_unch=glmer(ch_prev_unch_unique_object~previous_unique_reward+(previous_unique_reward|subject),data=df%>%filter(reoffer_unch_person==F,reoffer_ch_person==F,reoffer_unique_unch_object==T,reoffer_unique_ch_object==F),family=binomial(link="logit"))
summary(mb_unch)

#modulation by alpha_mb_unch
mb_unch_modulation=glmer(ch_prev_unch_unique_object~previous_unique_reward*alpha_mb_unch+(previous_unique_reward|subject),data=df%>%filter(reoffer_unch_person==F,reoffer_ch_person==F,reoffer_unique_unch_object==T,reoffer_unique_ch_object==F),family=binomial(link="logit"))
summary(mb_unch_modulation)
plot(effects::effect("previous_unique_reward:alpha_mb_unch",mb_unch_modulation))








