data {

  //General fixed parameters for the experiment/models

  

  int<lower=1> Nsubjects;

  

  int<lower=1> Nblocks;

  

  int<lower=1> Ntrials;

  

  array[Nsubjects] int<lower=1> Ntrials_per_subject;

  

  int<lower=4> Npersons;

  

  int<lower=2> Nobjects;

  

  int<lower=2> Nraffle;

  

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

  int Nparameters_standard = 3;
  int Nparameters_transformed = 3;

}

parameters {

  //population level parameters 

  vector[Nparameters_standard] population_locations;
  vector<lower=0, upper=1>[Nparameters_transformed] population_locations_transformed;

  vector<lower=0>[Nparameters_standard] population_scales;
  vector<lower=0>[Nparameters_transformed] population_scales_transformed;

//individual level

  //standard
  vector[Nsubjects] c_mf;

  vector[Nsubjects] c_mb;
  
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

model {

  // population level  


  population_locations  ~ normal(0,2);
  population_scales     ~ normal(0,2);


  population_locations_transformed  ~ beta(2,2);
  population_scales_transformed     ~ gamma(2,0.1);
  
  for (subject in 1 : Nsubjects) {
    //update parameter values
    
    //normal
    target+= normal_lpdf(c_mf[subject]|population_locations[1] , population_scales[1]);
    target+= normal_lpdf(c_mb[subject]|population_locations[2] , population_scales[2]);
    target+= normal_lpdf(pr[subject]|population_locations[3] , population_scales[3]);
    
    //transformed
    target+= beta_proportion_lpdf(f_mf[subject]|population_locations_transformed[1], population_scales_transformed[1]);
    target+= beta_proportion_lpdf(f_mb[subject]|population_locations_transformed[2], population_scales_transformed[2]);
    target+= beta_proportion_lpdf(f_p[subject]|population_locations_transformed[3], population_scales_transformed[3]);
    
    //update likelihood
    for (trial in 1 : Ntrials_per_subject[subject]) {

      target += bernoulli_logit_lpmf(selected_offer[subject, trial] | Qnet_diff[trial, subject]);

    }

  }

}




