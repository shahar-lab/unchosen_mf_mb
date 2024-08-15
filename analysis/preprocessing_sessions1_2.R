
library(tidyverse)
#rm(list = ls())

# session1 ----------------------------------------------------------------
process_session1_2 <- function(file_path) {
  df <- read.csv(file_path)
  
  #Check attention by mouse movement
  if(any(colnames(df)=="event")){
    number_inattention = df %>%
      filter(trial_index >= 10) %>%
      mutate(prev_event = lag(event)) %>%
      filter(event == "blur" & prev_event != "blur") %>%
      summarise(number_inattention = n()) %>%
      pull(number_inattention)
  }else{
    number_inattention=0 
  }
  
  click_task=df%>%filter(trial_type=="html-keyboard-response")%>%
    filter(grepl("^0\\.0-8", internal_node_id))
  
  
  ordered_learning_task=df%>%filter(grepl("^0\\.0-12", internal_node_id))
  
  
  unordered_learning_task=df%>%filter(grepl("^0\\.0-16", internal_node_id))
  
  
  flipped_learning_task=df%>%filter(grepl("^0\\.0-20", internal_node_id))
  
  df=df%>%mutate(number_inattention=number_inattention,
                 exclude_number_inattention=if_else(number_inattention>1,T,F),
                 click_incorrect=nrow(click_task)-20,
                 ordered_incorrect=sum(ordered_learning_task%>%pull(correct_response),na.rm=T),
                 unordered_incorrect=sum(unordered_learning_task%>%pull(correct_response),na.rm=T),
                 flipped_incorrect=sum(flipped_learning_task%>%pull(correct_response),na.rm=T))
  
 
  return (df)
}
# Get all CSV files in the directory
files <- list.files("data/empirical_data/raw_data/session1", pattern = "\\.csv$", full.names = TRUE)
data1 <- do.call(rbind, lapply(files, process_session1_2))
save(data1,file="data/empirical_data/preprocessed_unfiltered/session1/data1.rdata")

files <- list.files("data/empirical_data/raw_data/session2", pattern = "\\.csv$", full.names = TRUE)
data2 <- do.call(rbind, lapply(files, process_session1_2))
save(data2,file="data/empirical_data/preprocessed_unfiltered/session2/data2.rdata")

#filter_combine

filter_session1_2 <- function(subject_id,data1,data2){
  df1 <- data1 %>%
    filter(subject_id == !!subject_id)%>%mutate(session=1)
  df2=data2%>%
    filter(subject_id == !!subject_id)%>%mutate(session=2)
  df=rbind(df1,df2)
  df <- df %>%
    filter(grepl("^0\\.0-8|^0\\.0-12|^0\\.0-16|^0\\.0-20", internal_node_id))%>%
    select(subject_id,counterbalance,trial_name,trial_num,rt,choice,
           is_correct,correct_response,counter_correct,counter_errors,
           number_inattention,exclude_number_inattention,click_incorrect,ordered_incorrect,unordered_incorrect,flipped_incorrect)
  return(df)
}
df <- unique(data1$subject_id) %>%
  lapply(function(subject_id) filter_session1_2(subject_id, data1,data2)) %>%
  bind_rows()

save(df,file="data/empirical_data/preprocessed_filtered/session1_2/df.rdata")
