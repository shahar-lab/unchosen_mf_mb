#precision parameter recovery
rm(list = ls())
source('./functions/my_starter.R')
path = set_workingmodel()
Nsubjects <- c(50, 100, 200)

load(paste0(path$data,'/artificial_data_full.Rdata'))
for (n in Nsubjects) {
  distinct_subjects <- distinct(df, subject)
  sampled_subjects <- sample_n(distinct_subjects, size = n)
  # Filter the dataframe for the current number of subjects
  #load artificial data
  
  df_filtered = df %>%
    filter(
      subject %in% sampled_subjects$subject
    )
  #convert
  df_filtered$fold=df_filtered$block
  var_toinclude  = c(
    'first_trial_in_block',
    'trial',
    'person1',
    'person2',
    'ch_person',
    'unch_person',
    'common_object',
    'unique_ch_object',
    'unique_unch_object',
    'common_reward',
    'unique_reward',
    'selected_offer',
    'fold')
  data_for_stan_filtered<-make_mystandata(data                 = df_filtered, 
                                 subject_column       = df_filtered$subject,
                                 block_column         = df_filtered$block,
                                 var_toinclude        = var_toinclude,
                                 additional_arguments = list(
                                   Nraffle= 2,
                                   Npersons= 5,
                                   Nobjects= 5
                                 ))
  save(data_for_stan_filtered,file=paste0('data/stan_ready_data_files/artificial_standata_full_', n, '.Rdata'))
  
  #load model
  load(paste0(path$data,'/modelfit_compile.rdata'))

  
  #sample
  fit<- my_compiledmodel$sample(
    data            = data_for_stan_filtered, 
    iter_sampling   = 1000,
    iter_warmup     = 1000,
    chains          = 4,
    parallel_chains = 4,
    refresh         = 10)  
  
  
  fit$save_object(paste0(path$data,'/modelfit_recovery_',n,'.rds'))
    cat(paste0('[stan_modeling]:  "modelfit_recovery_",n,".rds" was saved at "',path$data,'"'))
  }