#parameter recovery
rm(list = ls())
source('./functions/my_starter.R')
path = set_workingmodel()

for (dataset in c('none','mf','mb')) {
  #load data
  load(paste0('data/stan_ready_data_files/artificial_standata_', path$name,'_', dataset,'.Rdata'))
  #load model
  load(paste0(path$data,'/modelfit_compile.rdata'))
  
  
  #sample
  fit<- my_compiledmodel$sample(
    data            = data_for_stan, 
    iter_sampling   = 1000,
    iter_warmup     = 1000,
    chains          = 4,
    parallel_chains = 4,
    refresh         = 10)  
  
  
  fit$save_object(paste0(path$data,'/modelfit_recovery_',dataset,'.rds'))
  cat(paste0('[stan_modeling]:  modelfit_recovery_',dataset,".rds was saved at ",path$data))
}
