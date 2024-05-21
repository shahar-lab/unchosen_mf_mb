# the idea is that stan will always estimate unbounded parameters.
# if we want to have the true parameters bounded in any way, we need to define a link function (transformation)
# here, you can define the mean and sd of the true parameters
# type of transformation:
# none 
# exp - you will get parameters in the mean and sd you define below, but bounded to be positive
# logit - you will get parameters in the mean and sd you define below, but bounded between 0 and 1. You should keep the mean between 0 and 1, 
#          and also make sure your sd is not to high, since the transformation generates Inf when reaching one or zero

model_parameters=list()
model_parameters$names                             =c('c_mf','c_mf_unch',"c_mb",'c_mb_unch','pr','f_mf','f_mb','f_p')
model_parameters$transformation                    =c('none','none','none','none','none','beta','beta','beta')
model_parameters$artificial_population_location    =c(0,0,0,0,0,0.5,0.5,0.5)
model_parameters$artificial_population_scale       =c(1,1,1,1,1,0.2,0.2,0.2)
# model_parameters$artificial_population_location    =c(0.47,-0.14,0.47,-0.14,0.13,0.31,0.31,0.06)
# model_parameters$artificial_population_scale       =c(0.33,0.16,0.33,0.16,0.09,0.18,0.18,0.04)
