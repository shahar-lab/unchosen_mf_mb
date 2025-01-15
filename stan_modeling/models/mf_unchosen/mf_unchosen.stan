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
  
  array[Nsubjects, Ntrials] int<lower=0> common_reward;
  
  array[Nsubjects, Ntrials] int<lower=0> unique_reward;
  
  array[Nsubjects, Ntrials] int<lower=0> person1;
  
  array[Nsubjects, Ntrials] int<lower=0> person2;
  
  array[Nsubjects, Ntrials] int<lower=0> selected_offer;
  
  array[Nsubjects, Ntrials] int<lower=0> first_trial_in_block;
}
transformed data {
  int<lower=1> Nparameters = 5;
}
parameters {
  //population level parameters 
  
  vector[Nparameters] population_locations;
  
  vector<lower=0>[Nparameters] population_scales;
  
  //individuals level
  
  vector[Nsubjects] alpha_mf_random_effect;
  
  vector[Nsubjects] alpha_mf_unch_random_effect;
  
  vector[Nsubjects] alpha_mb_random_effect;
  
  vector[Nsubjects] omega_random_effect;
  
  vector[Nsubjects] beta_random_effect;
}
transformed parameters {
  vector<lower=0, upper=1>[Nsubjects] alpha_mf;
  
  vector<lower=0, upper=1>[Nsubjects] alpha_mf_unch;
  
  vector<lower=0, upper=1>[Nsubjects] alpha_mb;
  
  vector<lower=0, upper=1>[Nsubjects] omega;
  
  vector[Nsubjects] beta;
  
  vector[Nraffle] Qmb_offered;
  
  vector[Nraffle] Qnet;
  
  real PE_mf;
  
  real PE_mf_unch;
  
  real PE_mb_common;
  
  real PE_mb_unique;
  
  vector[Npersons] Qmf;
  
  vector[Nobjects] Qmb;
  
  matrix[Ntrials, Nsubjects] Qnet_diff;
  
  //RL
  
  for (subject in 1 : Nsubjects) {
    //internal variabels
    
    //set indvidual parameters
    
    alpha_mf[subject] = inv_logit(population_locations[1]
                                  + population_scales[1]
                                    * alpha_mf_random_effect[subject]);
    
    alpha_mf_unch[subject] = inv_logit(population_locations[2]
                                       + population_scales[2]
                                         * alpha_mf_unch_random_effect[subject]);
    
    alpha_mb[subject] = inv_logit(population_locations[3]
                                  + population_scales[3]
                                    * alpha_mb_random_effect[subject]);
    
    omega[subject] = inv_logit(population_locations[4]
                               + population_scales[4]
                                 * omega_random_effect[subject]);
    
    beta[subject] = (population_locations[5]
                     + population_scales[5] * beta_random_effect[subject]);
    
    //likelihood estimation
    
    for (trial in 1 : Ntrials_per_subject[subject]) {
      //reset Qvalues (first trial only)
      
      if (first_trial_in_block[subject, trial] == 1) {
        Qmf = rep_vector(1, Npersons);
        
        Qmb = rep_vector(0.5, Nobjects);
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
      
      Qnet[1] = (1 - omega[subject]) * Qmf[person1[subject, trial]]
                + omega[subject] * Qmb_offered[1];
      
      Qnet[2] = (1 - omega[subject]) * Qmf[person2[subject, trial]]
                + omega[subject] * Qmb_offered[2];
      
      Qnet_diff[trial, subject] = Qnet[2] - Qnet[1];
      
      //update Qvalues
      
      PE_mf = common_reward[subject, trial] + unique_reward[subject, trial]
              - Qmf[ch_person[subject, trial]];
      
      PE_mf_unch = (1 - common_reward[subject, trial])
                   + (1 - unique_reward[subject, trial])
                   - Qmf[unch_person[subject, trial]];
      
      Qmf[ch_person[subject, trial]] = Qmf[ch_person[subject, trial]]
                                       + alpha_mf[subject] * PE_mf;
      
      Qmf[unch_person[subject, trial]] = Qmf[unch_person[subject, trial]]
                                         + alpha_mf_unch[subject]
                                           * PE_mf_unch;
      
      PE_mb_common = common_reward[subject, trial]
                     - Qmb[common_object[subject, trial]];
      
      Qmb[common_object[subject, trial]] = Qmb[common_object[subject, trial]]
                                           + alpha_mb[subject] * PE_mb_common;
      
      PE_mb_unique = unique_reward[subject, trial]
                     - Qmb[unique_ch_object[subject, trial]];
      
      Qmb[unique_ch_object[subject, trial]] = Qmb[unique_ch_object[subject, trial]]
                                              + alpha_mb[subject]
                                                * PE_mb_unique;
      
      //appened to external variabels
      
      //Qdiff_external[trial,subject] = Qdiff;
      
      //Qval1_external[trial,subject] = Qval[1];
      
      //Qval2_external[trial,subject] = Qval[2];
      
      //Qval3_external[trial,subject] = Qval[3];
      
      //Qval4_external[trial,subject] = Qval[4];
      
      //PE_external[trial,subject]    = PE;
    }
  }
}
model {
  // population level  
  
  population_locations ~ normal(0, 2);
  
  population_scales ~ normal(0, 2);
  
  // indvidual level  
  
  alpha_mf_random_effect ~ std_normal();
  
  alpha_mf_unch_random_effect ~ std_normal();
  
  alpha_mb_random_effect ~ std_normal();
  
  omega_random_effect ~ std_normal();
  
  beta_random_effect ~ std_normal();
  
  for (subject in 1 : Nsubjects) {
    for (trial in 1 : Ntrials_per_subject[subject]) {
      target += bernoulli_logit_lpmf(selected_offer[subject, trial] | beta[subject]
                                                                    * Qnet_diff[trial, subject]);
    }
  }
}


