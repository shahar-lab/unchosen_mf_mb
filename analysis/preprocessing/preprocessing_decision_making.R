
library(tidyverse)
rm(list = ls())

# create raw file ----------------------------------------------------------------
process_decision_making1_2 <- function(file_path,session) {
  df <- read.csv(file_path)
  
  #Check attention by mouse movement
  if(any(colnames(df)=="event")){
    number_inattention = df %>%
      filter(trial_index >= 80) %>%
      mutate(prev_event = lag(event)) %>%
      filter(event == "blur" & prev_event != "blur") %>%
      summarise(number_inattention = n()) %>%
      pull(number_inattention)
  }else{
    number_inattention=0
    df$event=0
    df$trial=0
    df$time=0
  }
  if(mean(df$counterbalance_associations,na.rm=T)==0){
  person_products = matrix(c(1, 2,   # P1 has O1 and O2
                            2, 3,   # P2 has O2 and O3
                            3, 4,   # P3 has O3 and O4
                            4, 5,   # P4 has O4 and O5
                            5, 1),  # P5 has O5 and O1
                          nrow = 5, byrow = TRUE)
  }
  else{
  person_products = matrix(c(3, 5,   # P1 has O3 and O5
                            1, 4,   # P2 has O1 and O4
                            2, 5,   # P3 has O2 and O5
                            1, 3,   # P4 has O1 and O3
                            2, 4),  # P5 has O2 and O4
                          nrow = 5, byrow = TRUE)  
  }

  
  #check rt
  rt=as.numeric(df%>%filter(phase=="exp",trial_name=="choice")%>%pull(rt))
  #add variables

  df=df%>%filter(phase=="exp",trial_name=="show_reward2")%>%
    rename(ch_person = person_selected,ch_key=key_selected)%>%
    select(subject_id,counterbalance_associations,counterbalance_reward,trial_num,left_person,right_person,ch_person,ch_key,first_product,second_product,reward1,reward2,prob_product1,prob_product2,prob_product3,prob_product4,prob_product5)%>%
    mutate(session=session,trial_num=trial_num+1,block=ceiling(trial_num/50),rt=rt,
           ch_person=as.numeric(ch_person)+1,left_person=as.numeric(left_person)+1,right_person=as.numeric(right_person)+1,first_product=as.numeric(first_product)+1,second_product=as.numeric(second_product)+1,ch_key=as.numeric(ch_key)+1,
           unch_person=if_else(ch_person==right_person,left_person,right_person))%>%
           rowwise()%>%mutate(common_product=ifelse(!is.na(ch_person),intersect(person_products[ch_person,],person_products[unch_person,]),0), #intersect(x,y) finds all the shared rows
                              unique_ch_product=ifelse(!is.na(ch_person),setdiff(person_products[ch_person,],person_products[unch_person,]),0),#setdiff(x, y) finds all rows in x that aren't in y
                              unique_unch_product=ifelse(!is.na(ch_person),setdiff(person_products[unch_person,],person_products[ch_person,]),0))%>%
                    ungroup()%>%
           mutate(reoffer_ch_person=lag(ch_person)==right_person|lag(ch_person)==left_person,
           reoffer_unch_person=lag(unch_person)==right_person|lag(unch_person)==left_person,
           reoffer_common_product=(lag(common_product)==common_product)|(lag(common_product)==unique_ch_product)|(lag(common_product)==unique_unch_product), #three products are offered (common,uniq_ch,uniq_unch)
           reoffer_unique_ch_product=(lag(unique_ch_product)==common_product)|(lag(unique_ch_product)==unique_ch_product)|(lag(unique_ch_product)==unique_unch_product),
           reoffer_unique_unch_product=(lag(unique_unch_product)==common_product)|(lag(unique_unch_product)==unique_ch_product)|(lag(unique_unch_product)==unique_unch_product),
           stay_person=lag(ch_person)==ch_person,
           stay_common_product=(lag(common_product)==common_product)|(lag(common_product)==unique_ch_product),
           stay_unique_product=(lag(unique_ch_product)==common_product)|(lag(unique_ch_product)==unique_ch_product),
           ch_prev_unchosen=lag(unch_person)==ch_person, #is the previously unch person is now chosen?
           ch_prev_unch_unique_product=(lag(unique_unch_product)==common_product)|(lag(unique_unch_product)==unique_ch_product), #is the previously unch product is now chosen?
           ch_prev_ch_unique_product=(lag(unique_ch_product)==common_product)|(lag(unique_ch_product)==unique_ch_product), #is the previously unch product is now chosen?
           stay_key=ch_key==lag(ch_key),
           common_reward=factor(if_else(common_product==first_product,reward1,reward2)),
           unique_reward=factor(if_else(unique_ch_product==first_product,reward1,reward2)),
           total_reward=reward1+reward2,
           previous_total_reward=factor(lag(total_reward)),
           previous_common_reward=factor(lag(common_reward)),
           previous_unique_reward=factor(lag(unique_reward)),
           current_common_previous_reward=factor(if_else(common_product==lag(common_product), #only relevant when filtering reoffer_common=T
                                                         previous_common_reward,previous_unique_reward)),
           expval_common=case_when(common_product==1~prob_product1,
                                   common_product==2~prob_product2,
                                   common_product==3~prob_product3,
                                   common_product==4~prob_product4,
                                   common_product==5~prob_product5),
           expval_unique_ch=case_when(unique_ch_product==1~prob_product1,
                                      unique_ch_product==2~prob_product2,
                                      unique_ch_product==3~prob_product3,
                                   unique_ch_product==4~prob_product4,
                                   unique_ch_product==5~prob_product5),
           expval_unique_unch=case_when(unique_unch_product==1~prob_product1,
                                        unique_unch_product==2~prob_product2,
                                        unique_unch_product==3~prob_product3,
                                        unique_unch_product==4~prob_product4,
                                        unique_unch_product==5~prob_product5),
           previous_common_exp_val=lag(expval_common)-0.5,
           previous_unique_ch_exp_val=lag(expval_unique_ch)-0.5,
           scaled_expval_unique_unch=expval_unique_unch-0.5,
           previous_unique_unch_exp_val=lag(scaled_expval_unique_unch),
           scaled_expval_other=if_else(unique_ch_product==lag(unique_unch_product),expval_unique_unch-0.5,expval_unique_ch-0.5),
           accuracy=(expval_unique_ch>expval_unique_unch)*1,
           delta_exp_value=abs(expval_unique_ch-expval_unique_unch),
           number_inattention=number_inattention)

  return (df)
}

# Get all CSV files in the directory
files <- list.files("data/empirical_data/data_collected/decision_making1", pattern = "\\.csv$", full.names = TRUE)
# Process each file and combine results into a dataframe
decision_making1 <- do.call(rbind, lapply(files, function(file) process_decision_making1_2(file, session=3)))
save(decision_making1,file="data/empirical_data/data_raw/decision_making/session1/decision_making1.rdata")

files <- list.files("data/empirical_data/data_collected/decision_making2", pattern = "\\.csv$", full.names = TRUE)
# Process each file and combine results into a dataframe
decision_making2 <- do.call(rbind, lapply(files, function(file) process_decision_making1_2(file, session=4)))
save(decision_making2,file="data/empirical_data/data_raw/decision_making/session2/decision_making2.rdata")

df=rbind(decision_making1,decision_making2)

#for osf
load("data/empirical_data/data_raw/id_mapping.rdata")

#add subject instead of subject_id
colnames(df)[colnames(df) == "subject.x"] <- "subject"
df=df%>%left_join(id_mapping, by = "subject_id")%>%
  select(subject_id,subject,session,block,trial_num,counterbalance_reward,counterbalance_associations,everything())
save(df,file="data/empirical_data/data_raw/decision_making/df.rdata")

df=df%>%select(-subject_id)
save(df,file="data/empirical_data/data_raw/rdata_for_osf/decision_making/df.rdata")

