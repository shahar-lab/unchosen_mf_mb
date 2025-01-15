rm(list = ls())
load("data/empirical_data/data_raw/model_learning/df.rdata")
library(dplyr)

df=df%>%filter(exclude_number_inattention==F)
save(df,file="data/empirical_data/data_filtered/model_learning/df.rdata")
#for osf
df=df%>%select(-subject_id)
save(df,file="data/empirical_data/data_filtered/rdata_for_osf/model_learning/df.rdata")
write.csv(df,file="data/empirical_data/data_filtered/model_learning/df.csv",row.names=F)
