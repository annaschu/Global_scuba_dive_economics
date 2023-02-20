
# loading packages --------------------------------------------------------

library(tidyverse)
library(pbapply)
library(parallel)
library(glue)
library(crayon)


# Loading custom functions ------------------------------------------------
source("R/benefit_transfer.R")
source("R/economic_estimate.R")
source("R/jackie_all.R")



# Setting environmental variables -----------------------------------------


## Creates an outputs directory to store results
dir.create("results_iteration/", showWarnings = F)


## Number of iteration for the jackknife
iterations_n <- 10


## Number of clusters for the parallel processing, you can set the percentage of power you want to deploy, eg. for 50% use 0.5

ncl <- detectCores() * 0.8


## Loading data
surveys <- read_csv("data/scuba_diving_operators_v_1_11102022_encr.csv")
names(surveys) <- str_replace_all(names(surveys), "_", ".")


# Jackknife iterations in parallel processing with results on a country level -----------------------------

cl <- makeCluster(ncl)
results_jackknifed <-
  suppressMessages({
    suppressWarnings(pbapply::pblapply(
      1:iterations_n,
      jackie_all,
      cl = cl
    ))
  })

stopCluster(cl)



# Saving the final table with the averages of all the 999 iterations --------


final_jackknifed_table <- results_jackknifed %>% 
  bind_rows() %>% 
  group_by_if(is.character) %>% 
  summarise_if(is.numeric, mean)

## in rds format
saveRDS(final_jackknifed_table, "results_iteration/jackknifed_table.RDS")
## in csv format
write_csv(final_jackknifed_table, "results_iteration/jackknifed_table.csv")

# this takes all the tables in the results_jackknifed list and calculate a separate revenue for each interation

final_estimates <- furrr::future_map_dfr(results_jackknifed, function(x) {
  x %>%
    group_by(Region, Subregion, Country) %>%
    summarise(
      Total_annual_revenue_all = sum(Total_annual_revenue_all),
      Total_annual_revenue_all_q1 = sum(Total_annual_revenue_all_q1),
      Total_annual_revenue_all_q3 = sum(Total_annual_revenue_all_q3)
    )
  }) %>% 
  group_by(Region, Subregion, Country) %>%
  mutate(iter = 1:n())

## This creates the plot
iteration_plot_global <- final_estimates %>% 
  group_by(iter) %>% 
  summarise_if(is.numeric, sum) %>% 
  ggplot(aes(x = iter, y = Total_annual_revenue_all/1000000)) +
  geom_point(pch = 21, fill = "gray60", alpha = .4) +
  geom_line(aes(y = Total_annual_revenue_all_q1/1000000), col = "gray80") +
  geom_line(aes(y = Total_annual_revenue_all_q3/1000000), col = "gray80") +
  labs(x = "Iteration #", y = "Total annual revenue in Millions of USD") +
  theme_light()


## This saves the plot
save(iteration_plot_global, 
     file = "results_iteration/iteration_plot_global.rdata")

## This saves the figure
ggsave(plot = iteration_plot_global, 
       "results_iteration/iteration_plot_global.png", 
       dpi = 800, width = 10, height = 5)

## This creates the plot for regional estimates
iteration_plot_regional <- final_estimates %>% 
  group_by(Region, iter) %>% 
  summarise_if(is.numeric, sum) %>% 
  ggplot(aes(x = iter, y = Total_annual_revenue_all/1000000)) +
  geom_point(pch = 21, fill = "gray60", alpha = .4) +
  geom_line(aes(y = Total_annual_revenue_all_q1/1000000), col = "gray80") +
  geom_line(aes(y = Total_annual_revenue_all_q3/1000000), col = "gray80") +
  labs(x = "Iteration #", y = "Total annual revenue in Millions of USD") +
  facet_grid(Region~., scales = "free_y") +
  theme_light() +
  theme(text = element_text(size = 20))

## This saves the plot for regional estimates
save(iteration_plot_regional, 
     file = "results_iteration/iteration_plot_regional.rdata")

## This saves the figure for regional estimates
ggsave(plot = iteration_plot_regional, 
       "results_iteration/iteration_plot_regional.png", 
       dpi = 800, width = 18, height = 15)


# Uploading to slack ------------------------------------------------------

# creating estimates outputs
final_result <- final_estimates %>%  
  group_by(iter) %>%  
  summarise_if(is.numeric, sum) %>% 
  ungroup() %>% 
  summarise_all(mean)
  
final_result_median <- round(final_result$Total_annual_revenue_all/1000000, 0)
final_result_q1 <- round(final_result$Total_annual_revenue_all_q1/1000000, 0)
final_result_q3 <- round(final_result$Total_annual_revenue_all_q3/1000000, 0)

