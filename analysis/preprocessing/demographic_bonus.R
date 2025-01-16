#get bonus
rm(list = ls())
load("data/empirical_data/data_raw/decision_making/df.rdata")
load("data/empirical_data/data_raw/decision_making/session1/decision_making1.rdata")
load("data/empirical_data/data_raw/decision_making/session2/decision_making2.rdata")

session1=read.csv("data/empirical_data/data_collected/demographic/prolific_export_673c4a5b6c4543a3c77248dc.csv")
session2=read.csv("data/empirical_data/data_collected/demographic/prolific_export_6750098c566dbab2d26be1d4.csv")
session3=read.csv("data/empirical_data/data_collected/demographic/prolific_export_674e9fb23ea5bb36807ada2b.csv")
session4=read.csv("data/empirical_data/data_collected/demographic/prolific_export_674e9fb23ea5bb36807ada2b_only_batch3.csv")
session5=read.csv("data/empirical_data/data_collected/demographic/prolific_export_6750098c566dbab2d26be1d4 (4).csv")

common_subjects = intersect(unique(decision_making1$subject_id), unique(decision_making2$subject_id))
demographic=rbind(session1,session2,session3,session4,session5)%>%rename(subject_id=Participant.id)%>%
  left_join(id_mapping, by = "subject_id") %>%
  filter(subject_id %in% common_subjects) %>%
  distinct(subject_id, .keep_all = TRUE) %>% # Remove duplicates based on subject_id
  select(subject, Age, Sex, Language)

range(demographic$Age)
mean(demographic$Age)
sd(demographic$Age)

table(demographic$Sex)

save(demographic,file="data/empirical_data/data_raw/demographic/demographic.rdata")
write.csv(demographic,file="data/empirical_data/data_raw/demographic/demographic.csv")


# bonus -------------------------------------------------------------------

load("data/empirical_data/data_raw/id_mapping.rdata")
bonus=df%>%mutate(bonus=(as.integer(common_reward)-1)*0.00125+(as.integer(unique_reward)-1)*0.00125)%>%group_by(subject_id)%>%summarise(total_bonus=sum(bonus,na.rm=T))

subjects_1=session1%>%filter(Status=="APPROVED")%>%select(Participant.id)
bonus_1=bonus%>%filter(subject_id%in%subjects_1$Participant.id)
bonus_1$subject_id=paste0(bonus_1$subject_id,",")
bonus_1$total_bonus=round(bonus_1$total_bonus,2)
write.csv(bonus_1,file="data/empirical_data/data_collected/demographic/bonus_1.csv")

subjects_2=session2%>%filter(Status=="APPROVED")%>%select(Participant.id)
bonus_2=bonus%>%filter(subject_id%in%subjects_2$Participant.id)
bonus_2$subject_id=paste0(bonus_2$subject_id,",")
bonus_2$total_bonus=round(bonus_2$total_bonus,2)
write.csv(bonus_2,file="data/empirical_data/data_collected/demographic/bonus_2.csv")

subjects_3=session3%>%filter(Status=="APPROVED")%>%select(Participant.id)
bonus_3=bonus%>%filter(subject_id%in%subjects_3$Participant.id)
bonus_3$subject_id=paste0(bonus_3$subject_id,",")
bonus_3$total_bonus=round(bonus_3$total_bonus,2)
write.csv(bonus_3,file="data/empirical_data/data_collected/demographic/bonus_3.csv")

subjects_4=session4%>%filter(Status=="APPROVED")%>%select(Participant.id)
bonus_4=bonus%>%filter(subject_id%in%subjects_4$Participant.id)
bonus_4$subject_id=paste0(bonus_4$subject_id,",")
bonus_4$total_bonus=round(bonus_4$total_bonus,2)
write.csv(bonus_4,file="data/empirical_data/data_collected/demographic/bonus_4.csv")

subjects_5=session5%>%filter(Status=="APPROVED")%>%select(Participant.id)
bonus_5=bonus%>%filter(subject_id%in%subjects_5$Participant.id)
bonus_5$subject_id=paste0(bonus_5$subject_id,",")
bonus_5$total_bonus=round(bonus_5$total_bonus,2)
write.csv(bonus_5,file="data/empirical_data/data_collected/demographic/bonus_5.csv")
