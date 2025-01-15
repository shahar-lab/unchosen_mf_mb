source('./functions/my_starter.R')
path = set_workingmodel()
load(paste0(path$data,"/artificial_data_500.Rdata"))

#preprocessing
df=df%>%mutate(reoffer_ch_person=(lag(ch_person)==person1)|(lag(ch_person)==person2),
               reoffer_unch_person=(lag(unch_person)==person1)|(lag(unch_person)==person2),
               reoffer_common_product=(lag(common_product)==common_product)|(lag(common_product)==unique_ch_product)|(lag(common_product)==unique_unch_product),
               reoffer_unique_ch_product=(lag(unique_ch_product)==common_product)|(lag(unique_ch_product)==unique_ch_product)|(lag(unique_ch_product)==unique_unch_product),
               reoffer_unique_unch_product=(lag(unique_unch_product)==common_product)|(lag(unique_unch_product)==unique_ch_product)|(lag(unique_unch_product)==unique_unch_product),
               stay_person=lag(ch_person)==ch_person,
               ch_prev_unchosen=lag(unch_person)==ch_person,
               stay_common_product=(lag(common_product)==common_product)|(lag(common_product)==unique_ch_product),
               stay_unique_product=(lag(unique_ch_product)==common_product)|(lag(unique_ch_product)==unique_ch_product),
               ch_prev_unch_unique_product=(lag(unique_unch_product)==common_product)|(lag(unique_unch_product)==unique_ch_product),
               total_reward=common_reward+unique_reward,
               scaled_expval_unique_unch=expval_unique_unch-0.5,
               previous_total_reward=factor(lag(total_reward)),
               previous_common_reward=factor(lag(common_reward)),
               previous_unique_reward=factor(lag(unique_reward)))
# Define your Nsubjects vector
Nsubjects <- c(25,50, 100, 150, 200, 300,400,500)
distinct_subjects <- distinct(df, subject)
# Loop through each value in Nsubjects
for (n in Nsubjects) {
  sampled_subjects <- sample_n(distinct_subjects, size = n)
  # Filter the dataframe for the current number of subjects
  df_filtered = df %>%
    filter(
      reoffer_unch_person == F,
      reoffer_ch_person == F,
      reoffer_unique_unch_product == T,
      subject %in% sampled_subjects$subject
    )
  
  # Run the brm model with the filtered data
  mb_unch_b <- brm(
    formula = ch_prev_unch_unique_product ~ previous_unique_reward +scaled_expval_unique_unch+ (1+previous_unique_reward+scaled_expval_unique_unch|| subject),
    data = df_filtered,
    family = bernoulli(link = "logit"),
    warmup = 1000,
    iter = 2000,
    chains = 4,
    cores = 4,
    seed = 123,
    backend = "cmdstanr"
  )
  
  df_filtered = df %>%
    filter(
      reoffer_unch_person == T,
      reoffer_ch_person == F,
      subject %in% sampled_subjects$subject
    )
  
  mf_unch_b =
    brm(
      formula=ch_prev_unchosen~previous_unique_reward+previous_common_reward+(1+previous_unique_reward+previous_common_reward||subject),
      data = df_filtered,
      family = bernoulli(link = "logit"),
      warmup = 1000,
      iter = 2000,
      chains = 4,
      cores = 4,
      seed = 123,
      backend = "cmdstanr"
    )
  # Construct the filename with the current value of n
  file_name <- paste0("mb_unch_n", n, ".Rdata") 
  # Save the model
  save(mb_unch_b, file = paste0(path$data,'/precision/',file_name))
  
  file_name <- paste0("mf_unch_n", n, ".Rdata")
  save(mf_unch_b, file = paste0(path$data,'/precision/',file_name))
}
