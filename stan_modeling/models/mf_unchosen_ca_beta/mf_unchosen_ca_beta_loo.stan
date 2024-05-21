data {

  //General fixed parameters for the experiment/models

  

  int<lower=1> Nsubjects;

  

  int<lower=1> Nblocks;

  

  int<lower=1> Ntrials;

  

  array[Nsubjects] int<lower=1> Ntrials_per_subject;

  

  int<lower=4> Npersons;

  

  int<lower=2> Nobjects;

  

  int<lower=2> Nraffle;

  array[Nsubjects,Ntrials] int<lower = 0> fold;
  real testfold;

  //Behavioral data:

  

  array[Nsubjects, Ntrials] int<lower=0> ch_person;

  

  array[Nsubjects, Ntrials] int<lower=0> unch_person;

  

  array[Nsubjects, Ntrials] int<lower=0> common_object;

  

  array[Nsubjects, Ntrials] int<lower=0> unique_ch_object;

  

  array[Nsubjects, Ntrials] int<lower=0> unique_unch_object;

  

  array[Nsubjects, Ntrials] int common_reward;

  

  array[Nsubjects, Ntrials] int unique_reward;

  

  array[Nsubjects, Ntrials] int<lower=0> person1;

  

  array[Nsubjects, Ntrials] int<lower=0> person2;

  

  array[Nsubjects, Ntrials] int<lower=0> selected_offer;

  

  array[Nsubjects, Ntrials] int<lower=0> first_trial_in_block;

}

transformed data {

  int Nparameters_standard = 4;
  int Nparameters_transformed = 3;
  real eps=1e-8;
}

parameters {

  //population level parameters 

  vector[Nparameters_standard] population_locations;
  vector<lower=eps, upper=1-eps>[Nparameters_transformed] population_locations_transformed;

  vector<lower=eps>[Nparameters_standard] population_scales;
  vector<lower=eps,upper=300>[Nparameters_transformed] population_scales_transformed;

//individual level

  //standard
  vector[Nsubjects] c_mf;

  vector[Nsubjects] c_mb;

  vector[Nsubjects] c_mf_unch;
  
  vector[Nsubjects] pr;
  
  //transformed
  vector<lower=0, upper=1>[Nsubjects] f_mf;
  
  vector<lower=0, upper=1>[Nsubjects] f_mb;
  
  vector<lower=0, upper=1>[Nsubjects] f_p;

}

transformed parameters {

  vector[Nraffle] Qmb_offered;

  vector[Nraffle] Qnet;

  vector[Npersons] Qmf;
  
  vector[Npersons] pers;

  vector[Nobjects] Qmb;

  matrix[Ntrials, Nsubjects] Qnet_diff;

    //trial by trial loop
for (subject in 1 : Nsubjects) {
    for (trial in 1 : Ntrials_per_subject[subject]) {
     if(fold[subject,trial]!=testfold){
      //reset Qvalues (first trial only)

      if (first_trial_in_block[subject, trial] == 1) {

        Qmf = rep_vector(0, Npersons);

        pers = rep_vector(0,Npersons);

        Qmb = rep_vector(0, Nobjects);

      }

      //calculate probability for each action

      if (ch_person[subject, trial] == person1[subject, trial]) {

        Qmb_offered[1] = Qmb[common_object[subject, trial]]

                         + Qmb[unique_ch_object[subject, trial]];

        Qmb_offered[2] = Qmb[common_object[subject, trial]]

                         + Qmb[unique_unch_object[subject, trial]];

      } else {

        Qmb_offered[1] = Qmb[common_object[subject, trial]]

                         + Qmb[unique_unch_object[subject, trial]];

        

        Qmb_offered[2] = Qmb[common_object[subject, trial]]

                         + Qmb[unique_ch_object[subject, trial]];

      }


      Qnet[1] = Qmf[person1[subject, trial]]

                +  Qmb_offered[1]+pers[person1[subject,trial]];

    
      Qnet[2] = Qmf[person2[subject, trial]]

                + Qmb_offered[2]+pers[person2[subject,trial]];

    
      Qnet_diff[trial, subject] = Qnet[2] - Qnet[1]; // this is the value based upon we will calculate the likelihood .

      //update Qvalues

      //Qmf, forgetting and then updating
      Qmf = (1-f_mf[subject])*Qmf;
      Qmf[ch_person[subject, trial]] = Qmf[ch_person[subject, trial]]
                                          + c_mf[subject]*(common_reward[subject, trial] + unique_reward[subject, trial]);

      Qmf[unch_person[subject, trial]] = Qmf[unch_person[subject, trial]]

                                         +c_mf_unch[subject]*(common_reward[subject, trial] + unique_reward[subject, trial]);

      //Qmb, forgetting and then updating
      Qmb = (1-f_mb[subject])*Qmb;
      Qmb[common_object[subject, trial]] = Qmb[common_object[subject, trial]]

                                           +c_mb[subject] * common_reward[subject, trial];

      Qmb[unique_ch_object[subject, trial]] = Qmb[unique_ch_object[subject, trial]]

                                           +c_mb[subject] * unique_reward[subject, trial];

      //perseveration, forgetting and then updating
      pers=(1-f_p[subject])*pers;
      pers[ch_person[subject, trial]]=pers[ch_person[subject, trial]]+pr[subject];
    }

  }
}
}
model {

  // population level  


  population_locations  ~ normal(0,2);
  population_scales     ~ lognormal(0,1);


  population_locations_transformed  ~ beta(1.5,1.5);
  population_scales_transformed     ~ gamma(2,0.05);
  
  for (subject in 1 : Nsubjects) {


    for (trial in 1 : Ntrials_per_subject[subject]) {
 if(fold[subject,trial]!=testfold){
    
    if(first_trial_in_block[subject, trial] == 1){
    //update parameter values
    //normal
    target+= normal_lpdf(c_mf[subject]|population_locations[1] , population_scales[1]);
    target+= normal_lpdf(c_mb[subject]|population_locations[2] , population_scales[2]);
    target+= normal_lpdf(c_mf_unch[subject]|population_locations[3] , population_scales[3]);
    target+= normal_lpdf(pr[subject]|population_locations[4] , population_scales[4]);
    
    //transformed
    target+= beta_proportion_lpdf(f_mf[subject]|population_locations_transformed[1], population_scales_transformed[1]);
    target+= beta_proportion_lpdf(f_mb[subject]|population_locations_transformed[2], population_scales_transformed[2]);
    target+= beta_proportion_lpdf(f_p[subject]|population_locations_transformed[3], population_scales_transformed[3]);
    }
    else{
    //update likelihood
    target += bernoulli_logit_lpmf(selected_offer[subject, trial] | Qnet_diff[trial, subject]);
    }
    }

  }

}
}

generated quantities {

  matrix[Ntrials,Nsubjects]  log_lik;
  vector[Nraffle] Qmb_offered_g;

  vector[Nraffle] Qnet_g;
  
  vector[Npersons] Qmf_g;
  vector[Npersons] pers_g;
  vector[Nobjects] Qmb_g;
  matrix[Ntrials, Nsubjects] Qnet_diff_g;
//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//Likelihood function per subject per trial
log_lik=rep_matrix(0,Ntrials,Nsubjects);
    for (subject in 1:Nsubjects) {
 
        for (trial in 1:Ntrials_per_subject[subject]){
         
         if(fold[subject,trial] == testfold) {
 
      //reset Qvalues (first trial only)
      if (first_trial_in_block[subject,trial] == 1) {
        Qmf_g = rep_vector(0, Npersons);
        
        pers_g = rep_vector(0, Npersons);

        Qmb_g = rep_vector(0, Nobjects);
      }
        if (ch_person[subject, trial] == person1[subject, trial]) {

        Qmb_offered_g[1] = Qmb_g[common_object[subject, trial]]

                         + Qmb_g[unique_ch_object[subject, trial]];

        Qmb_offered_g[2] = Qmb_g[common_object[subject, trial]]

                         + Qmb_g[unique_unch_object[subject, trial]];

      } else {

        Qmb_offered_g[1] = Qmb_g[common_object[subject, trial]]

                         + Qmb_g[unique_unch_object[subject, trial]];

        Qmb_offered_g[2] = Qmb_g[common_object[subject, trial]]

                         + Qmb_g[unique_ch_object[subject, trial]];

      }
      
      Qnet_g[1] = Qmf_g[person1[subject, trial]]

                + Qmb_offered_g[1]+pers_g[person1[subject,trial]];

      Qnet_g[2] = Qmf_g[person2[subject, trial]]

                + Qmb_offered_g[2]+pers_g[person2[subject,trial]];

      Qnet_diff_g[trial, subject] = Qnet_g[2] - Qnet_g[1];
      
      if (first_trial_in_block[subject, trial] != 1){
      log_lik[trial,subject] = bernoulli_logit_lpmf(selected_offer[subject, trial] | Qnet_diff_g[trial, subject]);
}
      
           //update Qvalues

      //Qmf, forgetting and then updating
      Qmf_g = (1-f_mf[subject])*Qmf_g;
      Qmf_g[ch_person[subject, trial]] = Qmf_g[ch_person[subject, trial]]
                                          + c_mf[subject]*(common_reward[subject, trial] + unique_reward[subject, trial]);

      Qmf_g[unch_person[subject, trial]] = Qmf_g[unch_person[subject, trial]]

                                         +c_mf_unch[subject]*(common_reward[subject, trial] + unique_reward[subject, trial]);

     
      //Qmb, forgetting and then updating
      Qmb_g = (1-f_mb[subject])*Qmb_g;
      Qmb_g[common_object[subject, trial]] = Qmb_g[common_object[subject, trial]]

                                           +c_mb[subject] * common_reward[subject, trial];

      Qmb_g[unique_ch_object[subject, trial]] = Qmb_g[unique_ch_object[subject, trial]]

                                           +c_mb[subject] * unique_reward[subject, trial];

      //perseveration, forgetting and then updating
      pers_g=(1-f_p[subject])*pers_g;
      pers_g[ch_person[subject, trial]]=pers_g[ch_person[subject, trial]]+pr[subject];
        }
        }
    }
}


