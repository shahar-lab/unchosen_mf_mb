
library(tidyverse)
#rm(list = ls())

# model_learning1 ----------------------------------------------------------------
process_model_learning1_2 <- function(file_path) {
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
    df$event=0
    df$trial=0
    df$time=0
    number_inattention=0 
  }
  
  
  df=df%>%mutate(number_inattention=number_inattention,
                 exclude_number_inattention=if_else(number_inattention>1,T,F),
                 N_mistakes=sum(str_detect(df$stimulus, "Incorrect"), na.rm = TRUE))%>%
    select(subject_id,counterbalance_associations,N_mistakes,rt,stimulus,response,choice,is_correct,phase,trial_name,correct_response,
           counter_correct,counter_errors,number_inattention,exclude_number_inattention)
  
 
  return (df)
}
# Get all CSV files in the directory
files <- list.files("data/empirical_data/data_collected/model_learning1", pattern = "\\.csv$", full.names = TRUE)
data1 <- do.call(rbind, lapply(files, process_model_learning1_2))
save(data1,file="data/empirical_data/data_raw/model_learning/session1/data1.rdata")

files <- list.files("data/empirical_data/data_collected/model_learning2", pattern = "\\.csv$", full.names = TRUE)
data2 <- do.call(rbind, lapply(files, process_model_learning1_2))
save(data2,file="data/empirical_data/data_raw/model_learning/session2/data2.rdata")

#filter_combine

filter_model_learning1_2 <- function(subject_id,data1,data2){
  df1 <- data1 %>%
    filter(subject_id == !!subject_id)%>%mutate(session=1)
  df2=data2%>%
    filter(subject_id == !!subject_id)%>%mutate(session=2)
  df=rbind(df1,df2)
  df <- df %>%
    group_by(subject_id) %>%
    mutate(exclude_number_inattention = if_else(all(exclude_number_inattention == FALSE | is.na(exclude_number_inattention)), 
                                                FALSE, 
                                                TRUE)) %>%
    filter(!is.na(counterbalance_associations)) %>%
    group_by(session) %>%
    slice(1) %>%
    ungroup() %>%
    select(subject_id, session, counterbalance_associations, N_mistakes,
           number_inattention, exclude_number_inattention)
  
  
  
  
  return(df)
}
df <- unique(data1$subject_id) %>%
  lapply(function(subject_id) filter_model_learning1_2(subject_id, data1,data2)) %>%
  bind_rows()


save(df,file="data/empirical_data/data_raw/model_learning/df.rdata")