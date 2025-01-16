rm(list = ls())
library(dplyr)
load("data/empirical_data/data_raw/decision_making/df.rdata")
# Filter and combine data -------------------------------------------------------------

filter_decision_making<- function(subj, df) {
  
  df=df%>%filter(subject_id==subj)%>%na.omit() %>%
    filter(rt > 300, rt < 4000)

  df = df %>%
    mutate(
      exclude_missing_data=if_else(any(session==4)&any(session==3),FALSE,TRUE),
      exclude_trial_omission = if_else(n() / 400 < 0.8, TRUE, FALSE),
      exclude_key_rep = if_else(mean(stay_key) > 0.7 | mean(stay_key) < 0.3, TRUE, FALSE),
      exclude_inattention = if_else(any(number_inattention > 1), TRUE, FALSE)
    )%>%select(subject_id,counterbalance_associations,counterbalance_reward,session,trial_num,everything())
  
  return(df)
}

load("data/empirical_data/data_raw/id_mapping.rdata")
print(paste0("Number of trials before trial exclusion is:",nrow(df)))
df <- unique(df$subject_id) %>%
  lapply(function(subject_id) filter_decision_making(subject_id, df)) %>%
  bind_rows()
print(paste0("Number of trials after trial exclusion is:",nrow(df)))
#view
filtered=df%>%group_by(subject)%>%summarise(mean(exclude_missing_data),mean(exclude_trial_omission),
                                            mean(exclude_key_rep),mean(exclude_inattention))
#filter
df=df%>%filter(exclude_missing_data==F,exclude_trial_omission==F,exclude_key_rep==F,exclude_inattention==F)

save(filtered,file="data/empirical_data/data_filtered/decision_making/filtered.rdata")
save(df,file="data/empirical_data/data_filtered/decision_making/df.rdata")
#for osf
df=df%>%select(-subject_id)
save(df,file="data/empirical_data/data_filtered/rdata_for_osf/decision_making/df.rdata")
write.csv(df,file="data/empirical_data/data_filtered/decision_making/df.csv",row.names=F)
