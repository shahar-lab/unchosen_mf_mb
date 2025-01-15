data {

  //General fixed parameters for the experiment/models

  int<lower=1> Ndata;

  int<lower=1> Nsubjects;

  array [Ndata] int<lower=1, upper=Nsubjects> subject_trial; // Which subject performed each trial


  int<lower=4> Npersons;

  

  int<lower=2> Nproducts;

  

  int<lower=2> Nraffle;

  

  //Behavioral data:

  

  array[Ndata] int<lower=0> ch_person;

  

  array[Ndata] int<lower=0> unch_person;

  

  array[Ndata] int<lower=0> common_product;

  

  array[Ndata] int<lower=0> unique_ch_product;

  

  array[Ndata] int<lower=0> unique_unch_product;

  

  array[Ndata] int common_reward;

  

  array[Ndata] int unique_reward;

  

  array[Ndata] int<lower=0> left_person;

  

  array[Ndata] int<lower=0> right_person;

  

  array[Ndata] int<lower=0> selected_offer;

  

  array[Ndata] int<lower=0> first_trial_in_block;

}

transformed data {

  real eps=1e-8;
}

parameters {

  //population level parameters 
  // Group-level (population) parameters
  real <lower=eps, upper=1-eps>mu_f_mf;        // Mean forgetting_mf across subjects
  real <lower=eps, upper=1-eps>mu_f_mb;        // Mean forgetting_mb across subjects
  real <lower=eps, upper=1-eps>mu_f_p;          // Mean forgetting_pers across subjects
  
  real mu_c_mf;        // Mean updating_mf across subjects
  real mu_c_mb;        // Mean updating_mb across subjects
  real mu_c_mf_unch;          // Mean updating_mf_unch across subjects
  real mu_c_mb_unch;      // Mean updating_mb_unch across subjects
  real mu_pr;                //mean perseveration
  
  // Group-level standard deviations (for subject-level variability)
  real<lower=eps,upper=300> precision_f_mf;       // Variability in threshold
  real<lower=eps,upper=300> precision_f_mb;        // Variability in scaling gamma
  real<lower=eps,upper=300> precision_f_pr;          // Variability in non-decision time
  
  real<lower=0> sigma_c_mf;          // Variability in mf_updating
  real<lower=0> sigma_c_mb;          // Variability in mb_updating
  real<lower=0> sigma_c_mf_unch;        // Variability mf_unch_updating
  real<lower=0> sigma_c_mb_unch;        // Variability mb_unch_updating
  real<lower=0> sigma_pr;            // Variability pers
//individual level

  //standard
  vector[Nsubjects] c_mf_sbj;

  vector[Nsubjects] c_mb_sbj;

  vector[Nsubjects] c_mf_unch_sbj;

  vector[Nsubjects] c_mb_unch_sbj;
  
  vector[Nsubjects] pr_sbj;
  
  //transformed
  vector<lower=eps, upper=1-eps>[Nsubjects] f_mf_sbj;
  
  vector<lower=eps, upper=1-eps>[Nsubjects] f_mb_sbj;
  
  vector<lower=eps, upper=1-eps>[Nsubjects] f_pr_sbj;

}

transformed parameters {
  vector [Ndata]c_mf_t;
	vector [Ndata]c_mb_t;					  
  vector [Ndata]c_mf_unch_t;
	vector [Ndata]c_mb_unch_t;
	vector [Ndata]pr_t;
	vector [Ndata]f_mf_t;					  
  vector [Ndata]f_mb_t;
	vector [Ndata]f_pr_t;
	
  vector[Nraffle] Qmb_offered;

  vector[Nraffle] Qnet;

  vector[Npersons] Qmf;
  
  vector[Npersons] pers;

  vector[Nproducts] Qmb;

  vector[Ndata] Qnet_diff;

    //trial by trial loop
for (trial in 1 : Ndata) {

  c_mf_t[trial]=c_mf_sbj[subject_trial[trial]];
	c_mb_t[trial]=c_mb_sbj[subject_trial[trial]];					  
  c_mf_unch_t[trial]=c_mf_unch_sbj[subject_trial[trial]];
	c_mb_unch_t[trial]=c_mb_unch_sbj[subject_trial[trial]];
	pr_t[trial]=pr_sbj[subject_trial[trial]];
	f_mf_t[trial]=f_mf_sbj[subject_trial[trial]];					  
  f_mb_t[trial]=f_mb_sbj[subject_trial[trial]];
	f_pr_t[trial]=f_pr_sbj[subject_trial[trial]];
      //reset Qvalues (first trial only)

      if (first_trial_in_block[trial] == 1) {

        Qmf = rep_vector(0, Npersons);

        pers = rep_vector(0,Npersons);

        Qmb = rep_vector(0, Nproducts);

      }

      //calculate probability for each action

      if (ch_person[trial] == left_person[trial]) {

        Qmb_offered[1] = Qmb[common_product[trial]]

                         + Qmb[unique_ch_product[trial]];

        Qmb_offered[2] = Qmb[common_product[trial]]

                         + Qmb[unique_unch_product[trial]];

      } else {

        Qmb_offered[1] = Qmb[common_product[trial]]

                         + Qmb[unique_unch_product[trial]];

        

        Qmb_offered[2] = Qmb[common_product[trial]]

                         + Qmb[unique_ch_product[trial]];

      }


      Qnet[1] = Qmf[left_person[trial]]

                +  Qmb_offered[1]+pers[left_person[trial]];

    
      Qnet[2] = Qmf[right_person[trial]]

                + Qmb_offered[2]+pers[right_person[trial]];

    
      Qnet_diff[trial] = Qnet[2] - Qnet[1]; // this is the value based upon we will calculate the likelihood .

      //update Qvalues

      //Qmf, forgetting and then updating
      Qmf = (1-f_mf_t[trial])*Qmf;
      Qmf[ch_person[trial]] = Qmf[ch_person[trial]]
                                          + c_mf_t[trial]*(common_reward[trial] + unique_reward[trial]);

      Qmf[unch_person[trial]] = Qmf[unch_person[trial]]

                                         +c_mf_unch_t[trial]*(common_reward[trial] + unique_reward[trial]);

      //Qmb, forgetting and then updating
      Qmb = (1-f_mb_t[trial])*Qmb;
      Qmb[common_product[trial]] = Qmb[common_product[trial]]

                                           +c_mb_t[trial] * common_reward[trial];

      Qmb[unique_ch_product[trial]] = Qmb[unique_ch_product[trial]]

                                           +c_mb_t[trial] * unique_reward[trial];


      Qmb[unique_unch_product[trial]] = Qmb[unique_unch_product[trial]]

                                                + c_mb_unch_t[trial]* unique_reward[trial];
      //perseveration, forgetting and then updating
      pers=(1-f_pr_t[trial])*pers;
      pers[ch_person[trial]]=pers[ch_person[trial]]+pr_t[trial];
    }

  }


model {

  // population level  

  // Priors for normal group-level parameters
  mu_c_mf ~ normal(0, 1);
  mu_c_mb ~ normal(0, 1);
  mu_c_mf_unch ~ normal(0, 1);
  mu_c_mb_unch ~ normal(0, 1);
  
  // Priors for normal group-level standard deviations
  sigma_c_mf ~ lognormal(0, 1);
  sigma_c_mb ~ lognormal(0, 1);
  sigma_c_mf_unch ~ lognormal(0, 1);
  sigma_c_mb_unch ~ lognormal(0, 1);
  
  // Priors for beta group-level parameters
  f_mf_sbj  ~ beta(1.5,1.5);
  f_mb_sbj  ~ beta(1.5,1.5);
  f_pr_sbj  ~ beta(1.5,1.5);
  
  // Priors for beta group-level precision
  precision_f_mf     ~ gamma(2,0.05);
  precision_f_mb     ~ gamma(2,0.05);
  precision_f_pr     ~ gamma(2,0.05);
  
  for (subject in 1 : Nsubjects) {
    //update parameter values
    
    //normal
    target+= normal_lpdf(c_mf_sbj[subject]|mu_c_mf , sigma_c_mf);
    target+= normal_lpdf(c_mb_sbj[subject]|mu_c_mb , sigma_c_mb);
    target+= normal_lpdf(c_mf_unch_sbj[subject]|mu_c_mf_unch , sigma_c_mf_unch);
    target+= normal_lpdf(c_mb_unch_sbj[subject]|mu_c_mb_unch , sigma_c_mb_unch);
    target+= normal_lpdf(pr_sbj[subject]|mu_pr, sigma_pr);
    
    //transformed
    target+= beta_proportion_lpdf(f_mf_sbj[subject]|mu_f_mf,precision_f_mf);
    target+= beta_proportion_lpdf(f_mb_sbj[subject]|mu_f_mb, precision_f_mb);
    target+= beta_proportion_lpdf(f_pr_sbj[subject]|mu_f_p, precision_f_pr);
  }
    //update likelihood
    for (trial in 1 : Ndata) {

      target += bernoulli_logit_lpmf(selected_offer[trial] | Qnet_diff[trial]);

    }

  

}




