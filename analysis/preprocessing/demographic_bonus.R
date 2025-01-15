#get bonus
rm(list = ls())
load("data/empirical_data/data_raw/df.rdata")

bonus=df%>%mutate(bonus=(as.integer(common_reward)-1)*0.00125+(as.integer(unique_reward)-1)*0.00125)%>%group_by(subject_id)%>%summarise(total_bonus=sum(bonus,na.rm=T))


session1=read.csv("data/empirical_data/data_collected/demographic/prolific_export_6750098c566dbab2d26be1d4.csv")
session2=read.csv("data/empirical_data/data_collected/demographic/prolific_export_674e9fb23ea5bb36807ada2b.csv")
session3=read.csv("data/empirical_data/data_collected/demographic/prolific_export_674e9fb23ea5bb36807ada2b_only_batch3.csv")
session4=read.csv("data/empirical_data/data_collected/demographic/prolific_export_6750098c566dbab2d26be1d4 (4).csv")

demographic=rbind(session1,session2,session3,session4)%>%rename(subject_id=Participant.id)
load("data/empirical_data/data_raw/id_mapping.rdata")
demographic=demographic%>%left_join(id_mapping, by = "subject_id")%>%
  select(subject,Age,Sex,Language)
write.csv(demographic,file="data/empirical_data/data_raw/demographic/demographic.rdata")
# subjects_1=session1%>%filter(Status=="APPROVED")%>%select(Participant.id)
# bonus_1=bonus%>%filter(subject_id%in%subjects_1$Participant.id)
# bonus_1$subject_id=paste0(bonus_1$subject_id,",")
# bonus_1$total_bonus=round(bonus_1$total_bonus,2)
# write.csv(bonus_1,file="data/empirical_data/data_collected/demographic/bonus_1.csv")
# 
# subjects_2=session2%>%filter(Status=="APPROVED")%>%select(Participant.id)
# bonus_2=bonus%>%filter(subject_id%in%subjects_2$Participant.id)
# bonus_2$subject_id=paste0(bonus_2$subject_id,",")
# bonus_2$total_bonus=round(bonus_2$total_bonus,2)
# write.csv(bonus_2,file="data/empirical_data/data_collected/demographic/bonus_2.csv")
# 
# subjects_3=session3%>%filter(Status=="APPROVED")%>%select(Participant.id)
# bonus_3=bonus%>%filter(subject_id%in%subjects_3$Participant.id)
# bonus_3$subject_id=paste0(bonus_3$subject_id,",")
# bonus_3$total_bonus=round(bonus_3$total_bonus,2)
# write.csv(bonus_3,file="data/empirical_data/data_collected/demographic/bonus_3.csv")

subjects_4=session4%>%filter(Status=="APPROVED")%>%select(Participant.id)
bonus_4=bonus%>%filter(subject_id%in%subjects_4$Participant.id)
bonus_4$subject_id=paste0(bonus_4$subject_id,",")
bonus_4$total_bonus=round(bonus_4$total_bonus,2)
write.csv(bonus_4,file="data/empirical_data/data_collected/demographic/bonus_4.csv")
