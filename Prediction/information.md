### Project Overview

#### Goals:
__The predictive modeling group (MHDS + OMH) have been meeting since July to build models and tools for predicting OTNY client outcomes at different follow-up periods.__

The group focused on building models to predict:

- __Education / Work__

- __Hospitalization__

The group has also worked on predicting discharge (results not shown here)

#### Data & Variables Used:

- The overall data was split into two parts: a training dataset used to build models and a test dataset that has been put in a "lock box" to be used later to eventually assess how well the models do.

- The training dataset used in this dashboard has 1038 total clients at baseline.

- 50 commonly used baseline variables were used to predict Education/Work and Hospitalization Status: Age (`age`), Race/Ethnicity (`race`), Primary Language (`primlang`), English Fluency (`fluent_eng`), Sexual Orientation (`orientation`), Highest Education (`highest_edu`), Insurance Type (`ins`), Legal Issues (`beh_legal_new`), Ever Competitively Employed (`competitive_emp`), Current Competitive Employment (`cur_comp_emp`), GAF Symptom Scores (`GAF_Sym`), GAF Occupational Scores (`GAF_OC`), GAF Social Functioning Scores (`GAF_SF`), Quality of Life (`qoflife`), Lives with Family (`lives_family`), Family Contact (`FamContact`), Homelessness (`homeless`), Support Person Status (`support_person`), Time to OTNY (`TimetoOTNY`), Time to First Service (`TimetoServ1`), Service Contact Type (`serv_contact_type`), Age of Onset (`AgeofOnset`), Violent or Aggressive Ideation (`vi_agg_ideation_beh`), Suicidal Ideation or Attempt (`any_suicidal`), Self-injury (`self_injury`), Any Substance Use (`substance_use`), Alcohol Use (`alcohol`), Marijuana Use (`mj`), Tobacco Use (`tobacco`), Primary Diagnosis (`primary_dx_new`), Ever Prescribed Antipsychotic (`ever_antipsych`), Ever Psych Hospitalization (`ever_psych_hosp`), Number of prior Hospitalizations (`num_prior_hosp`), Psychiatric Medication Prescription (`psych_meds`), Medication Adherence (`MedsTaken`), and ER Visit (`er_visit`).

- In addition to baseline variables, the following variables at follow-up points were used for prediction models: Education/Work Status (`out_edu_work`), Hospitalization Status (`hosp_psych`), GAF Symptom Scores (`GAF_Sym`), GAF Occupational Scores (`GAF_OC`), GAF Social Functioning Scores (`GAF_SF`), Quality of Life (`qoflife`), Lives with Family (`lives_family`), Family Contact (`FamContact`), Homelessness (`homeless`), Support Person Status (`support_person`), Violent or Aggressive Ideation (`vi_agg_ideation_beh`), Suicidal Ideation or Attempt (`any_suicidal`), Self-injury (`self_injury`), Any Substance Use (`substance_use`), Alcohol Use (`alcohol`), Marijuana Use (`mj`), Tobacco Use (`tobacco`), Primary Diagnosis (`primary_dx_new`), Psychiatric Medication Prescription (`psych_meds`), Medication Adherence (`MedsTaken`), ER Visit (`er_visit`), and SEES Seen (`sees_seen`). 

#### Modeling Details

- The `caret` package in R was used to train all models.

- The resampling strategy used to tune models was 5-fold cross-validation repeated 5 times over a search grid of relevant tuning parameters.

- The reported cross-validated AUC, for the best models are from this same repeated 5-fold cross-validation resampling strategy. 

- Balanced random forest models have been used for every time point.

#### Area-level Census Variables

In order to capture information about the region around each OTNY site that might be relevant to discharge, a number of summary variables based on 2014-2018 American Community Survey (ACS) variables were added to the model. These variables were obtained at the census tract level using the `tidycensus` package and the US Census API.

The mean value of each variable in census tracts within 5km, 10km, and 20km of each site was calculated and added to the model.

The added variables were:

- Median Household Income: `hh_med_income`

- Per Capita Income: `percapita_income`

- Percent of Households receiving Public Assistance: `percent_hh_public_assist`

- Percent of Families with a Child Below Poverty Line: `percent_fam_wchild_pov`

- Percent of Population over 25 with a Bachelor's Degree: `percent_pop25_bach`

- Percent of Owner Occupied Housing: `percent_owner_occ_h`

- Percent of Vacant Housing: `percent_vac_h`

- Median Gross Rent: `med_gross_rent`

- Percent of Households with a Mortgage: `percent_hh_mort`

- Percent of Population White: `percent_white`

- Gini Income Inequality Index: `gini_inc_ineq`

The idea of using these variables was motivated by previous work looking at area-level measures of SES predicting colonoscopy screening adherence by [Wheeler et al.](https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0179272).

Summaries of each of these `11` variables were considered within 5, 10, and 20km of OTNY sites, for a total of `33` variables added to the model.
