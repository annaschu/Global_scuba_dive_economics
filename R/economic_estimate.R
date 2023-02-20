
economic_estimate <- function(x) {
  if(unique(x$splitter) == "DT") {
    res <- x %>%
      dtplyr::lazy_dt() %>% 
      mutate(TR_cert_value = Q32_final_est * Q34_USD_final_q1, 
             TR_cert_q1 = Q32_final_q1 * Q34_USD_final_q1, 
             TR_cert_q3 = Q32_final_q3 * Q34_USD_final_q3) %>%  ## Certifications 
      mutate(TR_DT_scuba_value = Q117_USD_final_est * scuba_clients_mid_final_est, 
             TR_DT_scuba_q1 = Q117_USD_final_q1 * scuba_clients_mid_final_q1, 
             TR_DT_scuba_q3 = Q117_USD_final_q3 * scuba_clients_mid_final_q3) %>%  ## Day trips scuba
      mutate(TR_DT_snorkel_value = Q118_USD_final_est * snorkeling_clients_mid_final_est, 
             TR_DT_snorkel_q1 = Q118_USD_final_q1 * snorkeling_clients_mid_final_q1, 
             TR_DT_snorkel_q3 = Q118_USD_final_q3 * snorkeling_clients_mid_final_q3) %>% ## Day trips snorkel
      mutate(TR_DT_scuba_gear_value = scuba_clients_mid_final_est * (Q121_final_est/100) * Q123_USD_final_est,
             TR_DT_scuba_gear_q1 = scuba_clients_mid_final_q1 * (Q121_final_q1/100) * Q123_USD_final_q1, 
             TR_DT_scuba_gear_q3 = scuba_clients_mid_final_q3 * (Q121_final_q3/100) * Q123_USD_final_q3) %>% ## Gear value
      mutate(TR_DT_snorkel_gear_value = snorkeling_clients_mid_final_est * (Q122_final_est/100) * Q124_USD_final_est,
             TR_DT_snorkel_gear_q1 = snorkeling_clients_mid_final_q1 * (Q122_final_q1/100) * Q124_USD_final_q1, 
             TR_DT_snorkel_gear_q3 = snorkeling_clients_mid_final_q3 * (Q122_final_q3/100) * Q124_USD_final_q3) %>%   ## Snorkel rental
      as.data.frame()

  } else { 
    res <- x %>% 
      dtplyr::lazy_dt() %>% 
      mutate(TR_cert_value = Q32_final_est * Q34_USD_final_q1, 
             TR_cert_q1 = Q32_final_q1 * Q34_USD_final_q1, 
             TR_cert_q3 = Q32_final_q3 * Q34_USD_final_q3) %>%  ## Certifications 
      mutate(TR_LB_value = Q65_final_est * Q66_final_est * Q67_USD_final_est, 
             TR_LB_q1 = Q65_final_q1 * Q66_final_q1 * Q67_USD_final_q1, 
             TR_LB_q3 = Q65_final_q3 * Q66_final_q3 * Q67_USD_final_q3) %>%  ## Liveabords trips scuba
      mutate(TR_scuba_gear_value = (Q65_final_est * Q66_final_est) * (Q68_final_est/100) * Q70_USD_final_est,
             TR_scuba_gear_q1 = (Q65_final_q1 * Q66_final_q1) * (Q68_final_q1/100) * Q70_USD_final_q1,
             TR_scuba_gear_q3 = (Q65_final_q3 * Q66_final_q3) * (Q68_final_q3/100) * Q70_USD_final_q3) %>% 
      mutate(TR_snorkel_gear_value = (Q65_final_est * Q66_final_est) * (Q69_final_est/100) * Q71_USD_final_est,
             TR_snorkel_gear_q1 = (Q65_final_q1 * Q66_final_q1) * (Q69_final_q1/100) * Q71_USD_final_q1,
             TR_snorkel_gear_q3 = (Q65_final_q3 * Q66_final_q3) * (Q69_final_q3/100) * Q71_USD_final_q3) %>% 
    as.data.frame()
  }
}

  