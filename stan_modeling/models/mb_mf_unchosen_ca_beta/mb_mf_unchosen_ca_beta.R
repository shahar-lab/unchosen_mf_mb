#### simulate Rescorla-Wagner block for participant ----
sim.block = function(subject,parameters,cfg){ 
  print(paste('subject',subject))
  
#pre-allocation
  
  #set parameters
  c_mf = parameters['c_mf']
  c_mb = parameters['c_mb']
  pr   = parameters['pr']
  f_mf  = parameters['f_mf']
  f_mb  = parameters['f_mb']
  f_p  = parameters['f_p']
  c_mf_unch = parameters['c_mf_unch']
  c_mb_unch = parameters['c_mb_unch']

  
  #set initial var
  Nsubjects          = cfg$Nsubjects
  Npersons           = cfg$Npersons
  Nproducts           = cfg$Nproducts
  Nraffle            = cfg$Nraffle
  Nblocks            = cfg$Nblocks
  Ntrials_perblock   = cfg$Ntrials_perblock
  expvalues1          = cfg$rndwlk1
  expvalues2          = cfg$rndwlk2
  expvalues3          = cfg$rndwlk3
  expvalues4          = cfg$rndwlk4
  #expvalues=t(data.frame(a=rep(0.3,Ntrials_perblock),b=rep(0.3,Ntrials_perblock),c=rep(0.3,Ntrials_perblock),d=rep(0.3,Ntrials_perblock),e=rep(0.3,Ntrials_perblock)))
  
  #rownames(expvalues)=c('ev1','ev2','ev3','ev4','ev5')
  df                 =data.frame()
  
  person_products = matrix(c(1, 2,   # P1 has O1 and O2
                            2, 3,   # P2 has O2 and O3
                            3, 4,   # P3 has O3 and O4
                            4, 5,   # P4 has O4 and O5
                            5, 1),  # P5 has O5 and O1
                          nrow = 5, byrow = TRUE)
  rownames(person_products) = c("P1", "P2", "P3", "P4", "P5")
for (block in 1:Nblocks){
  if(subject<=(Nsubjects/2)){
  if(block==1){
    expvalues=expvalues1
  }
  else{
    expvalues=expvalues2
  }
  }
  else{
    if(block==1){
      expvalues=expvalues3
    }
    else{
      expvalues=expvalues4
    }
  }
  #each subject gets their own order.
  expvalues = expvalues[sample(nrow(expvalues)), ]
  Qmf      = as.matrix(t(rep(0,Npersons)))
  pers     = as.matrix(t(rep(0,Npersons)))
  Qmb      = as.matrix(t(rep(0,Nproducts)))
  colnames(Qmf)     =sapply(1:Npersons, function(n) {paste('Qperson',n,sep="")})
  colnames(pers)    =sapply(1:Npersons, function(n) {paste('PERSperson',n,sep="")})
  colnames(Qmb)     =sapply(1:Nproducts, function(n) {paste('Qproduct',n,sep="")})
  for (trial in 1:Ntrials_perblock){

    #computer offer
    shares_product = list(c(1,2),c(2,3),c(3,4),c(4,5),c(1,5)) #only pairs who share an product are offered
    raffle    = sample(shares_product,1,prob=rep(1/length(shares_product),length(shares_product)))
    raffle    =sample(unlist(raffle)) #mix order
    
    #players choice
    Qmf_offered=Qmf[raffle]
    pers_offered=pers[raffle]
    Qmb_offered=c(sum(Qmb[person_products[raffle[1],]]),sum(Qmb[person_products[raffle[2],]]))
    Qnet=Qmf_offered+Qmb_offered+pers_offered
    p         = exp(Qnet) / sum(exp(Qnet))
    ch_person = sample(raffle,1,prob=p)
    unch_person  = raffle[ch_person!=raffle]
    unique_ch_product = setdiff(person_products[ch_person,], person_products[unch_person,])
    unique_unch_product = setdiff(person_products[unch_person,], person_products[ch_person,])
    common_product = intersect(person_products[ch_person,], person_products[unch_person,])
    #outcome 
    common_reward = sample(c(-1,1),1,prob=c(1-expvalues[common_product,trial],expvalues[common_product,trial]))
    unique_reward = sample(c(-1,1),1,prob=c(1-expvalues[unique_ch_product,trial],expvalues[unique_ch_product,trial]))
    
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
            common_product        = common_product,
            unique_ch_product     = unique_ch_product,
            unique_unch_product   = unique_unch_product,
            expval_unique_ch     = expvalues[unique_ch_product,trial],
            expval_unique_unch   = expvalues[unique_unch_product,trial],
            expval_common        = expvalues[common_product,trial],
            common_reward        = common_reward,
            unique_reward        = unique_reward,
            c_mf                = c_mf,
            c_mf_unch           = c_mf_unch,
            c_mb_unch           = c_mb_unch,
            c_mb                = c_mb,
            pr                  = pr,
            f_mf                = f_mf,
            f_mb                = f_mb,
            f_p                  = f_p,
            Qmf_1                = Qmf_offered[1],
            Qmf_2                = Qmf_offered[2],
            Q_mf_ch              = Qmf[ch_person],
            Q_mf_unch            = Qmf[unch_person]
            )
      
      dfnew=cbind(dfnew,Qmf)
      dfnew=cbind(dfnew,Qmb)
      dfnew=cbind(dfnew,t(t(expvalues)[trial,]))
      
      #bind to the overall df
      df=rbind(df,dfnew)
       
    #updating Qvalues
    #mf
    Qmf            = (1-f_mf)*Qmf
    Qmf[ch_person] = Qmf[ch_person]+c_mf*(common_reward+unique_reward)

    #mf unch
    Qmf[unch_person] = Qmf[unch_person] + c_mf_unch*(common_reward+unique_reward)
    
    #mb
    Qmb                =(1-f_mb)*Qmb
    Qmb[common_product] = Qmb[common_product] +c_mb*(common_reward)
    Qmb[unique_ch_product] = Qmb[unique_ch_product] +c_mb*(unique_reward)
    
    #mb unch
    Qmb[unique_unch_product] = Qmb[unique_unch_product] + c_mb_unch*(unique_reward)
    #perseveration
    pers = (1-f_p)*pers #forgetting
    pers[ch_person] =pers[ch_person]+pr #perseveration
  }
}     
  return (df)
}