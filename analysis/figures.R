rm(list=ls())
source('./functions/my_starter.R')
#load any of the four models 
path = set_workingmodel()
load(paste0(path$data,'/regression/model_based_unch_mf.rdata'))
load(paste0(path$data,'/regression/model_free_unch_mf.rdata'))
c <- conditional_effects(mb_unch_b)
d <- conditional_effects(mf_unch_b)

#creating plot
plot <- plot(c, plot = FALSE)[[1]]+ylim(0.35,0.55)+theme_bw()
plot

plot <- plot(d, plot = FALSE)[[1]]+ylim(0.35,0.55)+theme_bw()
plot
