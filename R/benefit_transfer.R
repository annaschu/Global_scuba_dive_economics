benefit_transfer <- function(x) {
  x %>%
    dtplyr::lazy_dt() %>% 
    dplyr::select(ID,Country,business.name, name.op,Q0,name.sur,Q14,Q32,Q34.USD,Q49,Q50,
                  Q63,Q64,Q65,Q66,Q67.USD,Q68,Q69,Q74,Q70.USD,Q71.USD,
                  Q117.USD,Q118.USD,Q121,Q122,Q123.USD,Q124.USD,
                  scuba.clients.mid,snorkeling.clients.mid,Region,Subregion,
                  Currency,data.counts.OP, trip.type, splitter) %>%
    group_by(Country) %>%
    mutate(across(.cols = c('Q32','Q34.USD', 'Q49', 'Q50','Q64',
                            'Q65','Q66','Q67.USD','Q68','Q69','Q70.USD','Q71.USD',
                            'Q117.USD','Q118.USD','Q121','Q122','Q123.USD','Q124.USD',
                            'scuba.clients.mid', 'snorkeling.clients.mid'),
                  .fns = list("country_median" = ~ median(.x, na.rm = TRUE), 
                              "country_q1" = ~ quantile(.x, .25, na.rm = TRUE),
                              "country_q3" = ~ quantile(.x, .75, na.rm = TRUE)))) %>%
    group_by(Subregion) %>% 
    mutate(across(.cols = c('Q32','Q34.USD', 'Q49', 'Q50','Q64','Q65',
                            'Q66','Q67.USD','Q68','Q69','Q70.USD','Q71.USD',
                            'Q117.USD','Q118.USD','Q121','Q122','Q123.USD','Q124.USD',
                            'scuba.clients.mid', 'snorkeling.clients.mid'),
                  .fns =  list("subregion_median" = ~ median(.x, na.rm = TRUE), 
                               "subregion_q1" = ~ quantile(.x, .25, na.rm = TRUE),
                               "subregion_q3" = ~ quantile(.x, .75, na.rm = TRUE)))) %>%
    group_by(Region) %>% 
    mutate(across(.cols = c('Q32','Q34.USD', 'Q49', 'Q50','Q64',
                            'Q65','Q66','Q67.USD','Q68','Q69','Q70.USD','Q71.USD',
                            'Q117.USD','Q118.USD','Q121','Q122','Q123.USD','Q124.USD',
                            'scuba.clients.mid', 'snorkeling.clients.mid'),
                  .fns =  list("region_median" = ~ median(.x, na.rm = TRUE), 
                               "region_q1" = ~ quantile(.x, .25, na.rm = TRUE),
                               "region_q3" = ~ quantile(.x, .75, na.rm = TRUE)))) %>% 
    ungroup() %>% 
    mutate(across(.cols = c('Q32','Q34.USD', 'Q49', 'Q50','Q64',
                            'Q65','Q66','Q67.USD',
                            'Q68','Q69','Q70.USD','Q71.USD',
                            'Q117.USD','Q118.USD','Q121','Q122','Q123.USD','Q124.USD',
                            'scuba.clients.mid', 'snorkeling.clients.mid'),
                  .fns =  list("global_median" = ~ median(.x, na.rm = TRUE), 
                               "global_q1" = ~ quantile(.x, .25, na.rm = TRUE),
                               "global_q3" = ~ quantile(.x, .75, na.rm = TRUE)))) %>% 
    mutate(ID = as.character(ID)) %>% 
    as.data.frame() %>% 
    pivot_longer(cols = where(is.numeric), 
                 names_to =  c("question", "group", "estimate"),
                 values_to = "value", names_sep = "_") %>% 
    dtplyr::lazy_dt() %>% 
    group_by(Country, Region, Subregion, Currency, question) %>% 
    arrange(Country, Region, Subregion, Currency, question) %>% 
    fill(value, .direction = 'up') %>% 
    filter(estimate %in% c(NA, "q1", "q3")) %>% 
    filter(is.na(group)| group == "country") %>%
    mutate(estimate = replace_na(estimate, "final_est")) %>% 
    mutate(estimate = case_when(
      estimate == "q1" ~ "final_q1", 
      estimate == "q3" ~ "final_q3", 
      TRUE ~ "final_est"
    )) %>% 
    mutate(result = paste0(question, "_", estimate)) %>%
    ungroup() %>% 
    as.data.frame() %>% 
    mutate(result = str_replace_all(result, "\\.", "_")) %>% 
    ungroup() %>% 
    group_by_if(is.character) %>%
    summarise(value = mean(value)) %>% 
    ungroup() %>% 
    select(-group, -estimate, -question) %>% 
    pivot_wider(names_from = result, values_from = value) %>% 
    as.data.frame() %>% 
    select(-Q0_final_est)
}
