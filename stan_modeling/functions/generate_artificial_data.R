### This function uses task configuration to simulate artificial data using model parameters and task configuration.
### the data is saved in a file ready for stan run, you can choose the data file with set_standata_file().
### to manually examine the data generated in this code, use "get_df" function

generate_artificial_data <- function(cfg) {
  
  # generate parameters
  simulate_parameters(path,cfg,plotme=T)

  # generate trial-by-trial data
  simulate_artifical_data(path,cfg)
  
  # convert to format that stan likes
  simulate_convert_to_standata(path,cfg,
                               
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
  )
  
}