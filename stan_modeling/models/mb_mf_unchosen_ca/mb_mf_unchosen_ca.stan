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

  int<lower=1> Nparameters = 8;

}

parameters {

  //population level parameters 

  

  vector[Nparameters] population_locations;

  

  vector<lower=0>[Nparameters] population_scales;

  

  //individuals level

  

  vector[Nsubjects] c_mf_random_effect;

  

  vector[Nsubjects] c_mf_unch_random_effect;

  

  vector[Nsubjects] c_mb_unch_random_effect;

  

  vector[Nsubjects] c_mb_random_effect;
  
  vector[Nsubjects] pr_random_effect;
  
  vector[Nsubjects] f_mf_random_effect;
  
  vector[Nsubjects] f_mb_random_effect;

vector[Nsubjects] f_p_random_effect;
}

transformed parameters {

  vector[Nsubjects] c_mf;

  

  vector[Nsubjects] c_mf_unch;

  

  vector[Nsubjects] c_mb_unch;


  vector[Nsubjects] c_mb;

  
  vector<lower=0, upper=1>[Nsubjects] f_mf;
  
  vector<lower=0, upper=1>[Nsubjects] f_mb;


  vector<lower=0, upper=1>[Nsubjects] f_p;
  

  vector[Nsubjects] pr;



  vector[Nraffle] Qmb_offered;

  

  vector[Nraffle] Qnet;

  


  vector[Npersons] Qmf;
  
  vector[Npersons] pers;


  vector[Nobjects] Qmb;

  

  matrix[Ntrials, Nsubjects] Qnet_diff;

  

  //RL

  

  for (subject in 1 : Nsubjects) {

    //internal variabels

    

    //set indvidual parameters

    

    c_mf[subject] = (population_locations[1]

                                  + population_scales[1]

                                    * c_mf_random_effect[subject]);

    

    c_mf_unch[subject] = (population_locations[2]

                                       + population_scales[2]

                                         * c_mf_unch_random_effect[subject]);

    

    c_mb_unch[subject] = (population_locations[3]

                                       + population_scales[3]

                                         * c_mb_unch_random_effect[subject]);

    

    c_mb[subject] = (population_locations[4]

                                  + population_scales[4]

                                    * c_mb_random_effect[subject]);

    

    f_mf[subject] = inv_logit(population_locations[5]

                               + population_scales[5]

                                 * f_mf_random_effect[subject]);
                                 
    f_mb[subject] = inv_logit(population_locations[6]

                               + population_scales[6]

                                 * f_mb_random_effect[subject]);

    

    f_p[subject] = inv_logit(population_locations[7]

                     + population_scales[7] * f_p_random_effect[subject]);

        pr[subject] = (population_locations[8]

                                  + population_scales[8]

                                    * pr_random_effect[subject]);

    //likelihood estimation

    

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

      

      Qnet_diff[trial, subject] = Qnet[2] - Qnet[1];

      

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


      Qmb[unique_unch_object[subject, trial]] = Qmb[unique_unch_object[subject, trial]]

                                                + c_mb_unch[subject]* unique_reward[subject, trial];
      //perseveration
      pers=(1-f_p[subject])*pers;
      pers[ch_person[subject, trial]]=pers[ch_person[subject, trial]]+pr[subject];
    }

  }

}

model {

  // population level  

  

  population_locations ~ normal(0, 2);

  

  population_scales ~ normal(0, 2);

  

  // indvidual level  

  

  c_mf_random_effect ~ std_normal();

  

  c_mf_unch_random_effect ~ std_normal();


  c_mb_unch_random_effect ~ std_normal();

  
  c_mb_random_effect ~ std_normal();

  f_mf_random_effect ~ std_normal();
  f_mb_random_effect ~ std_normal();
  f_p_random_effect ~ std_normal();

  pr_random_effect ~ std_normal();

  

  

  for (subject in 1 : Nsubjects) {

    for (trial in 1 : Ntrials_per_subject[subject]) {

      target += bernoulli_logit_lpmf(selected_offer[subject, trial] | Qnet_diff[trial, subject]);

    }

  }

}




