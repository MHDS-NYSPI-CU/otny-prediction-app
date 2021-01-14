library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)
library(purrr)
library(caret)
library(tibble)
library(ggalluvial)
library(shiny)
library(stringr)
library(pROC)
library(yardstick)
library(viridis)
library(ggrepel)

pred_options <- c("Baseline",
                  "3-Month",
                  "6-Month",
                  "9-Month",
                  "12-Month"
                  )

pred_options_2 <- c("3-Month",
                    "6-Month",
                    "9-Month",
                    "12-Month",
                    "15-Month"
)

pred_df <- tibble(preds = pred_options, 
                  pred_nums = c(0, 3, 6, 9, 12))

pred_df_2 <- tibble(preds = pred_options_2, 
                  pred_nums = c(3, 6, 9, 12, 15))

edu_work_models <- read_rds("data/edu_work_models_s.rds")

hosp_models <- read_rds("data/hosp_models_s.rds")


##### training dat


training_dat <- read_rds("data/reduced_training_dat.rds")

cat_vars <- c("race", "primlang", "orientation",
              "highest_edu", "ins", "BEH_LEGAL_NEW",
              "out_edu", "out_work", "competitive_emp",
              "out_edu_work", "qoflife", "lives_family", 
              "FamContact", "homeless", "support_person", 
              "support_employment", "support_lang", "support_eng",
              "monetary_ent", "fam_inv_pref", "RelativePsych", 
              "vi_agg_ideation_beh", "any_suicidal", "self_injury",
              "substance_use", "alcohol", "mj",
              "tobacco", "primary_dx_new", "psych_meds",
              "MedsTaken", "serv_contact_type", "ADM_PCSOURCE1P_NEW",
              "fam_ptc_involvement", "er_visit", "hosp_psych",
              "ever_psych_hosp", "ever_antipsych", "siteid")

get_columns <- function(df, v, var_list){
  predictor_df <- df %>%
    filter(INT1 == (v/3) + 1) %>%
    mutate_at(.vars = vars(one_of(cat_vars)), function(x) as.factor(x)) %>%
    select(SCREENID_DID, 
           one_of(var_list)) %>%
    rename_at(vars(one_of(var_list)), function(x) str_c(x, "_", v, "mo"))
  return(predictor_df)
}

get_fup_data_forward <- function(df, out_v, pred_v, var_list){
  work_name <- str_c("work_edu_", out_v, "mo")
  hosp_name <- str_c("hosp_psych_", out_v, "mo")
  baseline_dat <- df %>%
    filter(INT1 == 1) %>%
    select(-cur_comp_emp, -num_hosps_beh, -num_prior_hosp, -support_lang, -support_employment, 
           -support_eng) %>%
    mutate_at(.vars = vars(one_of(cat_vars)), function(x) as.factor(x))
  if(pred_v >= 3){
    visits <- seq(from = 3, to = pred_v, by = 3)
    predictor_df <- map(visits, function(x) get_columns(training_dat, x, long_vars)) %>%
      reduce(left_join, by = "SCREENID_DID")
  }else{
    predictor_df <- baseline_dat
  }
  out_df <- df %>%
    filter(INT1 == (out_v/3) + 1) %>%
    mutate(out_edu_work = case_when(out_edu_work == 0 ~ "Negative",
                                    out_edu_work == 1 ~ "Positive",
                                    TRUE ~ NA_character_),
           hosp_psych = case_when(hosp_psych == 0 ~ "Negative",
                                  hosp_psych == 1 ~ "Positive",
                                  TRUE ~ NA_character_)) %>%
    mutate(out_edu_work = factor(out_edu_work, levels = c("Negative", "Positive")),
           hosp_psych = factor(hosp_psych, levels = c("Negative", "Positive"))) %>%
    select(SCREENID_DID, 
           !!work_name := out_edu_work,
           !!hosp_name := hosp_psych) 
  if(pred_v >= 3){
    return_df <- left_join(baseline_dat, predictor_df, by = "SCREENID_DID") %>%
      left_join(out_df, by = "SCREENID_DID")
  } else(
    return_df <- left_join(baseline_dat, out_df, by = "SCREENID_DID")
  )
  return(return_df)
}

long_vars <- c("out_edu_work", "hosp_psych",
               "GAF_SF", "GAF_OC", "GAF_Sym",
               "qoflife", "lives_family", "FamContact",
               "homeless", "support_person",
               "vi_agg_ideation_beh", "any_suicidal",
               "self_injury", "substance_use", 
               "alcohol", "mj", "tobacco",
               "primary_dx_new", "psych_meds", 
               "MedsTaken", "er_visit", "sees_seen")


####### GET 3-LEVEL OUT_EDU

long_dat_edu <- training_dat %>%
  mutate(fup = INT1 - 1) %>%
  filter(fup <= 8) %>%
  select(SCREENID_DID, fup, out_edu_work)

d_lookup <- training_dat %>%
  filter(INT1 == 18) %>%
  select(SCREENID_DID) %>%
  mutate(e_discharge = 1)

join_dat_edu <- long_dat_edu %>%
  left_join(d_lookup, by = "SCREENID_DID")

long_final_edu <- join_dat_edu %>%
  pivot_wider(names_from = fup, 
              values_from = out_edu_work,
              values_fill = list(out_edu_work = 99),
              names_prefix = "fup_") %>%
  mutate(fup_1 = if_else(fup_1 == 99 & e_discharge == 1, "Discharged", as.character(fup_1)),
         fup_2 = if_else(fup_2 == 99 & e_discharge == 1, "Discharged", as.character(fup_2)),
         fup_3 = if_else(fup_3 == 99 & e_discharge == 1, "Discharged", as.character(fup_3)),
         fup_4 = if_else(fup_4 == 99 & e_discharge == 1, "Discharged", as.character(fup_4)),
         fup_5 = if_else(fup_5 == 99 & e_discharge == 1, "Discharged", as.character(fup_5)),
         fup_6 = if_else(fup_6 == 99 & e_discharge == 1, "Discharged", as.character(fup_6)),
         fup_7 = if_else(fup_7 == 99 & e_discharge == 1, "Discharged", as.character(fup_7)),
         fup_8 = if_else(fup_8 == 99 & e_discharge == 1, "Discharged", as.character(fup_8)),
         fup_0 = as.character(fup_0)) %>%
  pivot_longer(cols = fup_0:fup_8,
               values_to = "out_edu_work",
               names_to = "fup",
               names_prefix = "fup_") %>%
  mutate(out_edu_work = case_when(out_edu_work == "0" ~ "Negative",
                                  out_edu_work == "1" ~ "Positive",
                                  out_edu_work == "Discharged" ~ "Discharged",
                                  TRUE ~ "Missing")) %>%
  mutate(out_edu_work = factor(out_edu_work, levels = c("Negative", "Positive", "Discharged", "Missing"))) %>%
  mutate(INT1 = as.numeric(fup) + 1) %>%
  select(SCREENID_DID, INT1, out_edu_work)

first_discharge_df <- long_final_edu %>%
  group_by(SCREENID_DID) %>%
  filter(out_edu_work == "Discharged") %>%
  mutate(first_discharge = min(INT1)) %>%
  select(SCREENID_DID, first_discharge) %>%
  distinct()

final_edu <- long_final_edu %>%
  left_join(first_discharge_df, by = "SCREENID_DID") %>%
  # mutate(out_edu_work = ifelse(out_edu_work == "Discharged" & INT1 > first_discharge))
  mutate(out_edu_work = ifelse(!is.na(first_discharge) & INT1 > first_discharge,
                               NA,
                               out_edu_work)) %>%
  mutate(out_edu_work = case_when(out_edu_work == 1 ~ "Negative",
                                  out_edu_work == 2 ~ "Positive",
                                  out_edu_work == 3 ~ "Discharged",
                                  TRUE ~ "Missing")) %>%
  mutate(out_edu_work = factor(out_edu_work, levels = c("Positive", "Negative", "Discharged", "Missing"))) %>%
  select(-first_discharge)

################## GET 3-level HOSP

long_dat_hosp <- training_dat %>%
  mutate(fup = INT1 - 1) %>%
  filter(fup <= 8) %>%
  select(SCREENID_DID, fup, hosp_psych)

d_lookup <- training_dat %>%
  filter(INT1 == 18) %>%
  select(SCREENID_DID) %>%
  mutate(e_discharge = 1)

join_dat_hosp <- long_dat_hosp %>%
  left_join(d_lookup, by = "SCREENID_DID")

long_final_hosp <- join_dat_hosp %>%
  pivot_wider(names_from = fup, 
              values_from = hosp_psych,
              values_fill = list(hosp_psych = 99),
              names_prefix = "fup_") %>%
  mutate(fup_1 = if_else(fup_1 == 99 & e_discharge == 1, "Discharged", as.character(fup_1)),
         fup_2 = if_else(fup_2 == 99 & e_discharge == 1, "Discharged", as.character(fup_2)),
         fup_3 = if_else(fup_3 == 99 & e_discharge == 1, "Discharged", as.character(fup_3)),
         fup_4 = if_else(fup_4 == 99 & e_discharge == 1, "Discharged", as.character(fup_4)),
         fup_5 = if_else(fup_5 == 99 & e_discharge == 1, "Discharged", as.character(fup_5)),
         fup_6 = if_else(fup_6 == 99 & e_discharge == 1, "Discharged", as.character(fup_6)),
         fup_7 = if_else(fup_7 == 99 & e_discharge == 1, "Discharged", as.character(fup_7)),
         fup_8 = if_else(fup_8 == 99 & e_discharge == 1, "Discharged", as.character(fup_8)),
         fup_0 = as.character(fup_0)) %>%
  pivot_longer(cols = fup_0:fup_8,
               values_to = "hosp_psych",
               names_to = "fup",
               names_prefix = "fup_") %>%
  mutate(hosp_psych = case_when(hosp_psych == "0" ~ "Negative",
                                hosp_psych == "1" ~ "Positive",
                                hosp_psych == "Discharged" ~ "Discharged",
                                TRUE ~ NA_character_)) %>%
  mutate(hosp_psych = factor(hosp_psych, levels = c("Negative", "Positive", "Discharged"))) %>%
  mutate(INT1 = as.numeric(fup) + 1) %>%
  select(SCREENID_DID, INT1, hosp_psych)

first_discharge_df <- long_final_hosp %>%
  group_by(SCREENID_DID) %>%
  filter(hosp_psych == "Discharged") %>%
  mutate(first_discharge = min(INT1)) %>%
  select(SCREENID_DID, first_discharge) %>%
  distinct()

final_hosp <- long_final_hosp %>%
  left_join(first_discharge_df, by = "SCREENID_DID") %>%
  mutate(hosp_psych = ifelse(hosp_psych == "Discharged" & INT1 > first_discharge,
                             NA,
                             hosp_psych)) %>%
  mutate(hosp_psych = case_when(hosp_psych == 1 ~ "Negative",
                                hosp_psych == 2 ~ "Positive",
                                hosp_psych == 3 ~ "Discharged",
                                TRUE ~ "Missing")) %>%
  mutate(hosp_psych = factor(hosp_psych, levels = c("Positive", "Negative", "Discharged", "Missing"))) %>%
  select(-first_discharge)


percent_info <- final_hosp %>% 
  group_by(INT1, hosp_psych) %>%
  summarize(n = n()) %>%
  group_by(INT1) %>%
  mutate(percent = round(100*(n/sum(n)), 1)) %>% 
  select(-n) %>%
  mutate(percent_label = str_c(hosp_psych, " (",percent,"%)")) %>%
  select(-percent) %>%
  arrange(INT1, desc(hosp_psych))

hosp_outs <- ggplot(data = final_hosp,
                    aes(x = INT1, stratum = hosp_psych, alluvium = SCREENID_DID, fill = hosp_psych)) +
  geom_stratum() +
  geom_flow() +
  geom_text(aes(x = INT1, stratum = hosp_psych),
            stat = "stratum", label = percent_info$percent_label) +
  theme_bw() +
  scale_fill_manual(values = c("Positive" = "#7fc97f",
                               "Negative" = "#beaed4",
                               "Discharged" = '#fdc086',
                               "Missing" = "grey80")) +
  scale_x_continuous(breaks = 1:9, labels = c("Baseline", "3", "6", "9", "12", "15", "18", "21", "24")) +
  labs(y = "", x = "Follow-Up Time (Months)", title = "Hospitalization in First Two Years") +
  theme(legend.position = "none")

percent_info <- final_edu %>% 
  group_by(INT1, out_edu_work) %>%
  summarize(n = n()) %>%
  group_by(INT1) %>%
  mutate(percent = round(100*(n/sum(n)), 1)) %>% 
  select(-n) %>%
  mutate(percent_label = str_c(out_edu_work, " (",percent,"%)")) %>%
  select(-percent) %>%
  arrange(INT1, desc(out_edu_work))

edu_outs <- ggplot(data = final_edu,
                   aes(x = INT1, stratum = out_edu_work, alluvium = SCREENID_DID, fill = out_edu_work)) +
  geom_stratum() +
  geom_flow() +
  geom_text(aes(x = INT1, stratum = out_edu_work),
            stat = "stratum", label = percent_info$percent_label) +
  theme_bw() +
  scale_fill_manual(values = c("Positive" = "#7fc97f",
                               "Negative" = "#beaed4",
                               "Discharged" = '#fdc086',
                               "Missing" = "grey80")) +
  scale_x_continuous(breaks = 1:9, labels = c("Baseline", "3", "6", "9", "12", "15", "18", "21", "24")) +
  labs(y = "", x = "Follow-Up Time (Months)", title = "Education/Work in First Two Years") +
  theme(legend.position = "none")

# get_available_ids <- function(p_fup, mod_name){
#   if(mod_name == "edu_work"){
#     df <- edu_work_models %>%
#       filter(predictor_fup %in% min(p_fup, 12)) %>%
#       filter(outcome_fup %in% c(p_fup + 3))
#   } else if (mod_name == "hosp"){
#     df <- hosp_models %>%
#       filter(predictor_fup %in% min(p_fup, 12)) %>%
#       filter(outcome_fup %in% c(p_fup + 3))
#   }
#   ids <- unique(df$caret[[1]]$trainingData$SCREENID_DID)
#   return(ids)
# }

get_available_ids <- function(p_fup, mod_name){
  if(mod_name == "edu_work"){
    
    avail_ids <- final_edu %>%
      filter(out_edu_work != "Discharged", out_edu_work != "Missing") %>%
      group_by(SCREENID_DID) %>%
      summarize(max_obs = max(INT1)) %>%
      select(SCREENID_DID, max_obs) %>%
      mutate(max_fup = (max_obs -1)*3) %>%
      filter(max_fup == p_fup) %>%
      pull(SCREENID_DID)

  } else if (mod_name == "hosp"){
    avail_ids <- final_hosp %>%
      filter(hosp_psych != "Discharged", hosp_psych != "Missing") %>%
      group_by(SCREENID_DID) %>%
      summarize(max_obs = max(INT1)) %>%
      select(SCREENID_DID, max_obs) %>%
      mutate(max_fup = (max_obs -1)*3) %>%
      filter(max_fup == p_fup) %>%
      pull(SCREENID_DID)
  }
  
  pred_data_avail_ids <- get_fup_data_forward(training_dat, 99, p_fup, long_vars) %>%
    select(-work_edu_99mo, -hosp_psych_99mo) %>%
    select(-INT1, -siteid) %>%
    drop_na() %>%
    pull(SCREENID_DID)
  
  avail_ids <- intersect(avail_ids, pred_data_avail_ids)
  
  return(avail_ids)
}

get_available_ids_long <- function(mod_name){
  if(mod_name == "edu_work"){
    
    avail_ids <- final_edu %>%
      filter(out_edu_work != "Discharged", out_edu_work != "Missing") %>%
      group_by(SCREENID_DID) %>%
      summarize(total_obs = n()) %>%
      filter(total_obs == 9) %>%
      pull(SCREENID_DID)
    
  } else if (mod_name == "hosp"){
    avail_ids <- final_hosp %>%
      filter(hosp_psych != "Discharged", hosp_psych != "Missing") %>%
      group_by(SCREENID_DID) %>%
      summarize(total_obs = n()) %>%
      filter(total_obs == 9)
      pull(SCREENID_DID)
  }
  
  pred_data_avail_ids <- get_fup_data_forward(training_dat, 99, 12, long_vars) %>%
    select(-work_edu_99mo, -hosp_psych_99mo) %>%
    select(-INT1, -siteid) %>%
    drop_na() %>%
    pull(SCREENID_DID)
  
  avail_ids <- intersect(avail_ids, pred_data_avail_ids)
  
  return(avail_ids)
}


get_predictor_data <- function(p_fup, id, mod_name){
  pred_data <- get_fup_data_forward(training_dat, 99, p_fup, long_vars) %>%
    select(-work_edu_99mo, -hosp_psych_99mo) %>%
    select(-INT1, -siteid) %>%
    filter(SCREENID_DID == id)
  return(pred_data)
}

out <- 6
mod_name = "edu_work"
id <- "000953282"






get_cumu_preds <- function(out, id, mod_name){
  dat_needed <- seq(from = 0, to = out-1, by = 3)
  dat_list <- map(dat_needed, function(x) get_predictor_data(p_fup = x, id = id, mod_name = mod_name))
  
  if(mod_name == "edu_work"){
  predictions <- map2_dfr(dat_needed, dat_list, function(x,y){
    models <- edu_work_models %>%
      filter(outcome_fup == out) %>%
      filter(predictor_fup == x) %>%
      mutate(final_models = map(caret, function(a) a$finalModel)) %>%
      mutate(predictions = map_dbl(caret, function(b) predict(b, newdata = y, type = "prob")[,"Positive"])) 
  })
  }else if(mod_name == "hosp"){
    predictions <- map2_dfr(dat_needed, dat_list, function(x,y){
      models <- hosp_models %>%
        filter(outcome_fup == out) %>%
        filter(predictor_fup == x) %>%
        mutate(final_models = map(caret, function(a) a$finalModel)) %>%
        mutate(predictions = map_dbl(caret, function(b) predict(b, newdata = y, type = "prob")[,"Positive"])) 
    })
  }
  return(predictions %>% select(-caret, -final_models))
}

get_1y_predictions <- function(p_fup, p_data, mod_name){
  #temp_ids <- get_available_ids(12, mod_name)
  p_data <- p_data %>%
    mutate(SCREENID_DID = "000953282")

  if(mod_name == "edu_work"){
  models <- edu_work_models %>%
    filter(predictor_fup == p_fup) %>%
    filter(outcome_fup %in% c(p_fup + 3,
                              p_fup + 6, 
                              p_fup + 9,
                              p_fup + 12)) %>%
    mutate(final_models = map(caret, function(x) x$finalModel)) %>%
    mutate(predictions = map_dbl(caret, function(x) predict(x, newdata = p_data, type = "prob")[,"Positive"])) 
  } else if(mod_name == "hosp"){
    models <- hosp_models %>%
      filter(predictor_fup == p_fup) %>%
      filter(outcome_fup %in% c(p_fup + 3,
                                p_fup + 6, 
                                p_fup + 9,
                                p_fup + 12)) %>%
      mutate(final_models = map(caret, function(x) x$finalModel)) %>%
      mutate(predictions = map_dbl(caret, function(x) predict(x, newdata = p_data, type = "prob")[,"Positive"])) 
  }
  return(models)
}

##### FUNCTIONS


plot_roc_pos <- function(caret_obj, ...) {
  f_auc <- get_avg_auc_pos(caret_obj)
  # f_acc <- get_avg_accuracy(caret_obj)
  # f_kap <- get_avg_kap(caret_obj)
  caret_obj$pred <- caret_obj$pred %>%
    mutate(obs = if_else(obs == "Positive", 1, 0)) %>%
    mutate(obs = factor(obs, levels = c("0", "1")))
  roc_obj <- roc(
    response = caret_obj$pred$obs, 
    predictor = caret_obj$pred$Positive
  )
  pROC::ggroc(roc_obj, ...) +
    annotate(geom = "text", x = 0.25, y = 0.25, label = str_c("AUC = ", round(f_auc, 4))) +
    #annotate(geom = "text", x = 0.25, y = 0.20, label = str_c("Accuracy = ", round(f_acc, 4))) +
    #annotate(geom = "text", x = 0.25, y = 0.15, label = str_c("Kappa = ", round(f_kap, 4))) +
    theme_minimal() + 
    #ggtitle("ROC Curve") + 
    geom_segment(aes(x = 1, xend = 0, y = 0, yend = 1), color="grey", linetype="dashed") +
    labs(x = "Specificity", y = "Sensitivity")
}

plot_roc_dis <- function(caret_obj, ...) {
  f_auc <- get_avg_auc_dis(caret_obj)
  # f_acc <- get_avg_accuracy(caret_obj)
  # f_kap <- get_avg_kap(caret_obj)
  caret_obj$pred <- caret_obj$pred %>%
    mutate(obs = if_else(obs == "Discharged", 1, 0)) %>%
    mutate(obs = factor(obs, levels = c("0", "1")))
  roc_obj <- roc(
    response = caret_obj$pred$obs, 
    predictor = caret_obj$pred$Discharged
  )
  pROC::ggroc(roc_obj, ...) +
    annotate(geom = "text", x = 0.25, y = 0.25, label = str_c("AUC = ", round(f_auc, 4))) +
    #annotate(geom = "text", x = 0.25, y = 0.20, label = str_c("Accuracy = ", round(f_acc, 4))) +
    #annotate(geom = "text", x = 0.25, y = 0.15, label = str_c("Kappa = ", round(f_kap, 4))) +
    theme_minimal() + 
   # ggtitle("ROC Curve") + 
    geom_segment(aes(x = 1, xend = 0, y = 0, yend = 1), color="grey", linetype="dashed") +
    labs(x = "Specificity", y = "Sensitivity")
}

get_avg_auc_pos <- function(caret_obj){
  caret_obj$pred %>%
    group_by(Resample) %>%
    roc_auc(factor(obs, levels = c("Positive", "Negative")), Positive) %>%
    summarize(mean_auc = mean(.estimate)) %>%
    pull(mean_auc)
}







get_avg_auc_dis <- function(caret_obj){
  caret_obj$pred %>%
    group_by(Resample) %>%
    roc_auc(factor(obs, levels = c("Positive", "Negative")), Discharged) %>%
    summarize(mean_auc = mean(.estimate)) %>%
    pull(mean_auc)
}

make_predicted_histo_pos <- function(caret_obj, outcome){
  predictions <- caret_obj$pred %>%
    group_by(obs, rowIndex) %>%
    summarize(mean_prob = mean(Positive))
  
  vir_2 <- viridis(2)
  ggplot(data = predictions) +
    geom_histogram(data = predictions %>% filter(obs == "Negative"), aes(x = mean_prob, y = (..count..)/sum(..count..), fill = obs), 
                   position = "identity", alpha = 0.6, color = "white", binwidth = 0.05) +
    geom_histogram(data = predictions %>% filter(obs == "Positive"), aes(x = mean_prob, y = (..count..)/sum(..count..), fill = obs), 
                   position = "identity", alpha = 0.6, color = "white", binwidth = 0.05) +
    theme_minimal() +
    labs(x = "Predicted Probability Positive", fill = "Observed", y = "Percent") +
    scale_x_continuous(limits = c(-0.1,1.1), breaks = seq(from = 0, to = 1, by = 0.2)) +
    scale_fill_manual(values = c("Positive" = "#f1a340", "Negative" = vir_2[1])) +
    theme(legend.position = "bottom")
}

make_var_imp <- function(model, top){
  ggplot(varImp(model, scale = FALSE), top = top) +
    theme_minimal()
}

create_monthly_pred_pos <- function(caret_obj, month){
  preds <- caret_obj$pred %>%
    group_by(rowIndex) %>%
    summarize(pred_prob = mean(Positive))
  original_dat <- caret_obj$trainingData %>%
    mutate(rowIndex = row_number()) %>%
    select(SCREENID_DID, rowIndex, outcome = .outcome)
  comb_df <- left_join(original_dat, preds, by = "rowIndex") %>%
    select(-rowIndex) %>%
    mutate(month = month)
  return(comb_df)
}

create_monthly_pred_dis <- function(caret_obj, month){
  preds <- caret_obj$pred %>%
    group_by(rowIndex) %>%
    summarize(pred_prob = mean(Discharged))
  original_dat <- caret_obj$trainingData %>%
    mutate(rowIndex = row_number()) %>%
    select(SCREENID_DID, rowIndex, outcome = .outcome)
  comb_df <- left_join(original_dat, preds, by = "rowIndex") %>%
    select(-rowIndex) %>%
    mutate(month = month)
  return(comb_df)
}

make_predicted_histo_dis <- function(caret_obj, outcome){
  predictions <- caret_obj$pred %>%
    group_by(obs, rowIndex) %>%
    summarize(mean_prob = mean(Discharged))
  
  vir_2 <- viridis(2)
  ggplot(data = predictions) +
    geom_histogram(data = predictions %>% filter(obs == "Discharged"), aes(x = mean_prob, y = (..count..)/sum(..count..), fill = obs), 
                   position = "identity", alpha = 0.6, color = "white", binwidth = 0.05) +
    geom_histogram(data = predictions %>% filter(obs == "Not"), aes(x = mean_prob, y = (..count..)/sum(..count..), fill = obs), 
                   position = "identity", alpha = 0.6, color = "white", binwidth = 0.05) +
    theme_minimal() +
    labs(x = "Predicted Probability Discharged", fill = "Observed", y = "Percent") +
    scale_x_continuous(limits = c(-0.1,1.1), breaks = seq(from = 0, to = 1, by = 0.2)) +
    scale_fill_manual(values = c("Discharged" = "#f1a340", "Not" = vir_2[1])) +
    theme(legend.position = "bottom")
}

make_var_imp <- function(model, top){
  ggplot(varImp(model, scale = FALSE), top = top) +
    theme_minimal()
}


###### Creating plots

perf_list_edu <- pmap(list(edu_work_models$predictor_fup,
                       edu_work_models$outcome_fup,
                       edu_work_models$caret), function(x,y,z){
                         p <- plot_roc_pos(z)
                         p2 <-make_var_imp(z, 20)
                         p3 <- make_predicted_histo_pos(z, str_c("Edu/Work ", "Predictors = ", x, " Outcome = ", y))
                         return(list(p, p2, p3))
                       })

perf_list_hosp <- pmap(list(hosp_models$predictor_fup,
                            hosp_models$outcome_fup,
                            hosp_models$caret), function(x,y,z){
                              p <- plot_roc_pos(z)
                              p2 <-make_var_imp(z, 20)
                              p3 <- make_predicted_histo_pos(z, str_c("Hospitalization ", "Predictors = ", x, " Outcome = ", y))
                              return(list(p, p2, p3))
                            })


# OUT 3
edu_pred0_out3 <- perf_list_edu[[1]]
# OUT 6
edu_pred0_out6 <- perf_list_edu[[2]]
edu_pred3_out6 <- perf_list_edu[[3]]
# OUT 9
edu_pred0_out9 <- perf_list_edu[[4]]
edu_pred3_out9 <- perf_list_edu[[5]]
edu_pred6_out9 <- perf_list_edu[[6]]
# OUT 12
edu_pred0_out12 <- perf_list_edu[[7]]
edu_pred3_out12 <- perf_list_edu[[8]]
edu_pred6_out12 <- perf_list_edu[[9]]
edu_pred9_out12 <- perf_list_edu[[10]]
# OUT 15
edu_pred0_out15 <- perf_list_edu[[11]]
edu_pred3_out15 <- perf_list_edu[[12]]
edu_pred6_out15 <- perf_list_edu[[13]]
edu_pred9_out15 <- perf_list_edu[[14]]
edu_pred12_out15 <- perf_list_edu[[15]]
# OUT 18
edu_pred0_out18 <- perf_list_edu[[16]]
edu_pred3_out18 <- perf_list_edu[[17]]
edu_pred6_out18 <- perf_list_edu[[18]]
edu_pred9_out18 <- perf_list_edu[[19]]
edu_pred12_out18 <- perf_list_edu[[20]]
# OUT 21
edu_pred0_out21 <- perf_list_edu[[21]]
edu_pred3_out21 <- perf_list_edu[[22]]
edu_pred6_out21 <- perf_list_edu[[23]]
edu_pred9_out21 <- perf_list_edu[[24]]
edu_pred12_out21 <- perf_list_edu[[25]]
# OUT 24
edu_pred0_out24 <- perf_list_edu[[26]]
edu_pred3_out24 <- perf_list_edu[[27]]
edu_pred6_out24 <- perf_list_edu[[28]]
edu_pred9_out24 <- perf_list_edu[[29]]
edu_pred12_out24 <- perf_list_edu[[30]]


# HOSP

# OUT 3
hosp_pred0_out3 <- perf_list_hosp[[1]]
# OUT 6
hosp_pred0_out6 <- perf_list_hosp[[2]]
hosp_pred3_out6 <- perf_list_hosp[[3]]
# OUT 9
hosp_pred0_out9 <- perf_list_hosp[[4]]
hosp_pred3_out9 <- perf_list_hosp[[5]]
hosp_pred6_out9 <- perf_list_hosp[[6]]
# OUT 12
hosp_pred0_out12 <- perf_list_hosp[[7]]
hosp_pred3_out12 <- perf_list_hosp[[8]]
hosp_pred6_out12 <- perf_list_hosp[[9]]
hosp_pred9_out12 <- perf_list_hosp[[10]]
# OUT 15
hosp_pred0_out15 <- perf_list_hosp[[11]]
hosp_pred3_out15 <- perf_list_hosp[[12]]
hosp_pred6_out15 <- perf_list_hosp[[13]]
hosp_pred9_out15 <- perf_list_hosp[[14]]
hosp_pred12_out15 <- perf_list_hosp[[15]]
# OUT 18
hosp_pred0_out18 <- perf_list_hosp[[16]]
hosp_pred3_out18 <- perf_list_hosp[[17]]
hosp_pred6_out18 <- perf_list_hosp[[18]]
hosp_pred9_out18 <- perf_list_hosp[[19]]
hosp_pred12_out18 <- perf_list_hosp[[20]]
# OUT 21
hosp_pred0_out21 <- perf_list_hosp[[21]]
hosp_pred3_out21 <- perf_list_hosp[[22]]
hosp_pred6_out21 <- perf_list_hosp[[23]]
hosp_pred9_out21 <- perf_list_hosp[[24]]
hosp_pred12_out21 <- perf_list_hosp[[25]]
# OUT 24
hosp_pred0_out24 <- perf_list_hosp[[26]]
hosp_pred3_out24 <- perf_list_hosp[[27]]
hosp_pred6_out24 <- perf_list_hosp[[28]]
hosp_pred9_out24 <- perf_list_hosp[[29]]
hosp_pred12_out24 <- perf_list_hosp[[30]]
