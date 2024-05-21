# the idea is that stan will always estimate unbounded parameters.
# if we want to have the true parameters bounded in any way, we need to define a link function (transformation)
# here, you can define the mean and sd of the true parameters
# type of transformation:
# none 
# exp - you will get parameters in the mean and sd you define below, but bounded to be positive
# logit - you will get parameters in the mean and sd you define below, but bounded between 0 and 1. You should keep the mean between 0 and 1, 
#          and also make sure your sd is not to high, since the transformation generates Inf when reaching one or zero

model_parameters=list()
model_parameters$names                             =c('c_mf',"c_mb",'pr','f','f_p')
model_parameters$transformation                    =c('none','none','none','logit','logit')
model_parameters$artificial_population_location    =c(0.4,0.4,0.4,qlogis(0.8),qlogis(0.95))
model_parameters$artificial_population_scale       =c(0.5,0.5,0.5,0.3,0.3)
