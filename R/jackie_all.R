
jackie_all <- function(x) {
  
  library(tidyverse)
  library(pbapply)
  library(glue)
  library(crayon)
  source("R/benefit_transfer.R")
  source("R/economic_estimate.R")
  source("R/jackie_all.R")
  
  surveys <- read_csv("data/scuba_diving_operators_v_1_11102022_encr.csv")
  names(surveys) <- str_replace_all(names(surveys), "_", ".")
  

  surveyed <- surveys %>% 
    filter(!is.na(name.sur)) %>% 
    pull(ID) %>% 
    sample(length(.)*0.05, replace = F) %>% unique()
  
  sample <- surveys %>%
    filter(!ID %in% surveyed) %>% 
            mutate(
      trip.type =
        case_when(
          Q63 == "Yes" & Q74 == "Yes" ~ "DT_LB",
          Q63 == "Yes" ~ "LB_only",
          Q63 == "No" ~ "DT_only",
          day.trip == "Yes" & live.aboard == "Yes" ~ "DT_LB",
          day.trip == "Yes" & live.aboard == "No" ~ "DT_only",
          day.trip == "no" & live.aboard == "yes" ~ "LB_only",
          day.trip == "no" & live.aboard == "no" ~ "neither",
          TRUE ~ "unknown"
        )
    ) %>%
    filter(trip.type != "neither") %>%
    mutate(
      splitter =
        case_when(
          trip.type == "DT_only" | trip.type == "unknown" ~ "DT",
          trip.type == "LB_only" | trip.type == "DT_LB" ~ "LB"
        )
    ) %>%
    split(. , .$splitter)
  
  
  sample_bf <- lapply(sample, benefit_transfer)
  
  estimate_res <- lapply(sample_bf, economic_estimate)
  
  finale <- bind_rows(estimate_res) %>%
    mutate_at(vars(matches("TR_")), replace_na, 0) %>% 
    mutate(
      Total_annual_revenue_all = TR_LB_value + TR_scuba_gear_value +
        TR_DT_snorkel_gear_value + TR_DT_scuba_value +
        TR_DT_snorkel_value + TR_DT_scuba_gear_value +
        TR_DT_snorkel_gear_value + TR_cert_value,
      Total_annual_revenue_all_q1 = TR_LB_q1 + TR_scuba_gear_q1 +
        TR_DT_snorkel_gear_q1 + TR_DT_scuba_q1 +
        TR_DT_snorkel_q1 + TR_DT_scuba_gear_q1 +
        TR_DT_snorkel_gear_q1 + TR_cert_q1,
      Total_annual_revenue_all_q3 = TR_LB_q3 + TR_scuba_gear_q3 +
        TR_DT_snorkel_gear_q3 + TR_DT_scuba_q3 +
        TR_DT_snorkel_q3 + TR_DT_scuba_gear_q3 +
        TR_DT_snorkel_gear_q3 + TR_cert_q3
    )
  
}