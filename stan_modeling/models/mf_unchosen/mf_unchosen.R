#### simulate Rescorla-Wagner block for participant ----
sim.block = function(subject,parameters,cfg){ 
  print(paste('subject',subject))
  
#pre-allocation
  
  #set parameters
  alpha_mf = inv_logit_scaled(parameters['alpha_mf'])
  alpha_mf_unch = inv_logit_scaled(parameters['alpha_mf_unch'])
  alpha_mb = inv_logit_scaled(parameters['alpha_mb'])
  omega = inv_logit_scaled(parameters['omega'])
  beta  = parameters['beta']

  
  #set initial var
  Npersons           = cfg$Npersons
  Nobjects           = cfg$Nobjects
  Nraffle            = cfg$Nraffle
  Nblocks            = cfg$Nblocks
  Ntrials_perblock   = cfg$Ntrials_perblock
  expvalues          = cfg$rndwlk
  rownames(expvalues)=c('ev1','ev2','ev3','ev4','ev5')
  df                 =data.frame()
  
  person_objects = matrix(c(1, 2,   # P1 has O1 and O2
                            2, 3,   # P2 has O2 and O3
                            3, 4,   # P3 has O3 and O4
                            4, 5,   # P4 has O4 and O5
                            5, 1),  # P5 has O5 and O1
                          nrow = 5, byrow = TRUE)
  rownames(person_objects) = c("P1", "P2", "P3", "P4", "P5")
for (block in 1:Nblocks){
  
  Qmf      = as.matrix(t(rep(1,Npersons)))
  Qmb      = as.matrix(t(rep(0.5,Nobjects)))
  colnames(Qmf)     =sapply(1:Npersons, function(n) {paste('Qperson',n,sep="")})
  colnames(Qmb)     =sapply(1:Nobjects, function(n) {paste('Qobject',n,sep="")})
  for (trial in 1:Ntrials_perblock){

    #computer offer
    shares_object = list(c(1,2),c(2,3),c(3,4),c(4,5),c(1,5)) #only pairs who share an object are offered
    raffle    = unlist(sample(shares_object,1,prob=rep(1/length(shares_object),length(shares_object))))
    raffle    =sample(unlist(raffle)) #mix order
    
    #players choice
    Qmf_offered=Qmf[raffle]
    Qmb_offered=c(sum(Qmb[person_objects[raffle[1],]]),sum(Qmb[person_objects[raffle[2],]]))
    Qnet=(1-omega)*Qmf_offered+omega*(Qmb_offered)
    p         = exp(beta*Qnet) / sum(exp(beta*Qnet))
    ch_person = sample(raffle,1,prob=p)
    unch_person  = raffle[ch_person!=raffle]
    unique_ch_object = setdiff(person_objects[ch_person,], person_objects[unch_person,])
    unique_unch_object = setdiff(person_objects[unch_person,], person_objects[ch_person,])
    common_object = intersect(person_objects[ch_person,], person_objects[unch_person,])
    #outcome 
    common_reward = sample(0:1,1,prob=c(1-expvalues[common_object,trial],expvalues[common_object,trial]))
    unique_reward = sample(0:1,1,prob=c(1-expvalues[unique_ch_object,trial],expvalues[unique_ch_object,trial]))
    
    #save trial's data
    
      #create data for current trials
      dfnew=data.frame(
            subject              = subject,
            block                = block,
            trial                = trial,
            first_trial_in_block = (trial==1)*1,
            ch_person            = ch_person,
            selected_offer       = (ch_person==raffle[2])*1,
            unch_person          = unch_person,
            person1              = raffle[1],
            person2              = raffle[2],
            common_object        = common_object,
            unique_ch_object     = unique_ch_object,
            unique_unch_object   = unique_unch_object,
            expval_ch            = expvalues[unique_ch_object,trial],
            expval_unch          = expvalues[unique_unch_object,trial],
            common_reward        = common_reward,
            unique_reward        = unique_reward,
            alpha_mf             = alpha_mf,
            alpha_mf_unch        = alpha_mf_unch,
            alpha_mb             = alpha_mb,
            omega                = omega,
            beta                 = beta
            )
      
      dfnew=cbind(dfnew,Qmf)
      dfnew=cbind(dfnew,Qmb)
      dfnew=cbind(dfnew,t(t(expvalues)[trial,]))
      
      #bind to the overall df
      df=rbind(df,dfnew)
       
    #updating Qvalues
    #mf
    Qmf[ch_person] = Qmf[ch_person] + alpha_mf*(common_reward+unique_reward - Qmf[ch_person])
    Qmf[unch_person] = Qmf[unch_person] + alpha_mf_unch*((1-common_reward)+(1-unique_reward) - Qmf[unch_person])
    
    #mb
    Qmb[common_object] = Qmb[common_object] +alpha_mb*(common_reward-Qmb[common_object])
    Qmb[unique_ch_object] = Qmb[unique_ch_object] +alpha_mb*(unique_reward-Qmb[unique_ch_object])
  }
}     
  return (df)
}