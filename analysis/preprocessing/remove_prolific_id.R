library(tidyverse)
rm(list = ls())


# remove PROLIFIC id ------------------------------------------------------

#model_learning
load("data/empirical_data/data_raw/model_learning/df.rdata")
all_ids=unique(df$subject_id)
id_mapping <- data.frame(
  subject_id = all_ids,
  subject = seq_along(all_ids) # Assign numeric IDs starting from 1
)
df <- left_join(df, id_mapping, by = "subject_id")
colnames(df)[colnames(df) == "subject.x"] <- "subject"
save(df,file="data/empirical_data/data_raw/model_learning/df.rdata")
save(id_mapping,file="data/empirical_data/data_raw/id_mapping.rdata")
#for osf
df=df%>%select(-subject_id)
save(df,file="data/empirical_data/data_raw/rdata_for_osf/model_learning/df.rdata")
write.csv(df,file="data/empirical_data/data_raw/model_learning/df.csv",row.names=F)


