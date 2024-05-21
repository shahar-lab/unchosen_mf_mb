data {

  //General fixed parameters for the experiment/models

  

  int<lower=1> Nsubjects;

  

  int<lower=1> Nblocks;

  

  int<lower=1> Ntrials;

  

  array[Nsubjects] int<lower=1> Ntrials_per_subject;

  

  int<lower=4> Narms;



  int<lower=2> Nraffle;

  

  //Behavioral data:

  

  array[Nsubjects, Ntrials] int<lower=0> choice;

  

  array[Nsubjects, Ntrials] int<lower=0> unchosen;

  

  array[Nsubjects, Ntrials] int reward;

  

  array[Nsubjects, Ntrials] int<lower=0> offer1;

  

  array[Nsubjects, Ntrials] int<lower=0> offer2;

  

  array[Nsubjects, Ntrials] int<lower=0> selected_offer;

  

  array[Nsubjects, Ntrials] int<lower=0> first_trial_in_block;

}

transformed data {

  int Nparameters_standard = 3;
  int Nparameters_transformed = 2;

}

parameters {

  //population level parameters 

  vector[Nparameters_standard] population_locations;
  vector<lower=0, upper=1>[Nparameters_transformed] population_locations_transformed;

  vector<lower=0>[Nparameters_standard] population_scales;
  vector<lower=0>[Nparameters_transformed] population_scales_transformed;

//individual level

  //standard
  vector[Nsubjects] c;

  vector[Nsubjects] c_unch;
  
  vector[Nsubjects] pr;
  
  //transformed
  vector<lower=0, upper=1>[Nsubjects] f;
  
  vector<lower=0, upper=1>[Nsubjects] f_p;

}

transformed parameters {
vector[Narms] Qvalues;
  vector[Nraffle] Qoffered;

  vector[Nraffle] Qnet;

  vector[Narms] pers;
  real reward_conv;
  matrix[Ntrials, Nsubjects] Qnet_diff;

    //trial by trial loop
for (subject in 1 : Nsubjects) {
    for (trial in 1 : Ntrials_per_subject[subject]) {

      //reset Qvalues (first trial only)

      if (first_trial_in_block[subject, trial] == 1) {

        Qvalues = rep_vector(0, Narms);

        pers = rep_vector(0,Narms);


      }

      //calculate probability for each action

     

      Qnet[1] = Qvalues[offer1[subject, trial]]+pers[offer1[subject,trial]];

      Qnet[2] = Qvalues[offer2[subject, trial]]+pers[offer2[subject,trial]];
    
      Qnet_diff[trial, subject] = Qnet[2] - Qnet[1]; // this is the value based upon we will calculate the likelihood .

      //update Qvalues
      if(reward[subject, trial]==1){
        reward_conv=1;
      }
      else{
        reward_conv=-1;
      }
      //Qmf, forgetting and then updating
      Qvalues= (1-f[subject])*Qvalues;
      Qvalues[choice[subject, trial]] = Qvalues[choice[subject, trial]]
                                          + c[subject]*(reward_conv);

     
      Qvalues[unchosen[subject, trial]] = Qvalues[unchosen[subject, trial]]

                                         +c_unch[subject]*(reward_conv);

      //perseveration, forgetting and then updating
      pers=(1-f_p[subject])*pers;
      pers[choice[subject, trial]]=pers[choice[subject, trial]]+pr[subject];
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
    target+= normal_lpdf(c[subject]|population_locations[1] , population_scales[1]);
    target+= normal_lpdf(c_unch[subject]|population_locations[2] , population_scales[2]);
    target+= normal_lpdf(pr[subject]|population_locations[3] , population_scales[3]);
    
    //transformed
    target+= beta_proportion_lpdf(f[subject]|population_locations_transformed[1], population_scales_transformed[1]);
    target+= beta_proportion_lpdf(f_p[subject]|population_locations_transformed[2], population_scales_transformed[2]);
    
    //update likelihood
    for (trial in 1 : Ntrials_per_subject[subject]) {

      target += bernoulli_logit_lpmf(selected_offer[subject, trial] | Qnet_diff[trial, subject]);

    }

  }

}




