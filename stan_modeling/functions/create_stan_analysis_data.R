
library(tidyverse)
load(file="data/empirical_data/data_filtered/decision_making/df.rdata")


vars= c(
  'first_trial_in_block',
  'first_trial',
  'rt',
  'ch_person',
  'unch_person',
  'left_person',
  'right_person',
  'common_reward',
  'unique_reward',
  'common_product',
  'unique_ch_product',
  'unique_unch_product',
  'selected_offer')


filepath=paste0("data/empirical_data/data_filtered/decision_making/df.rdata")

empirical_convert_to_stan_format(filepath,vars)
