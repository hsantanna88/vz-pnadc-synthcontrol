################################################################################

# Hugo Sant'Anna Rodrigues
#
# @Organization - University of Georgia
# @Project      - Venezuelan Refugees in Roraima
# @Date         - May 2022
# @Description  - Main script for performing Synth Control in PNADC

################################################################################



################################################################################
# Prepare the Script
# -- loading libraries and setting important parameters
################################################################################

library(tidyverse)    # Tidyverse package.
library(tidylog)      # Verbose tidyverse functions
library(fixest)       # Main regression package
library(modelsummary) # Pretty tables
library(tidysynth)    # Synthetic Control package
library(patchwork)    # Beautiful subplots
library(ragg)         # For scaling figures

# Small function to negate the boolean operator %in%
`%!in%` <- Negate(`%in%`)                       

################################################################################
# Function to load the synth control
################################################################################

synth_fun <- function(x) {
    
    synth_obj <- x %>%
        # synthetic control object
        synthetic_control(outcome            = y,            # Outcome
                          unit               = state,        # Unit index in the panel data
                          time               = period,       # Time index in the panel data
                          i_unit             = "Roraima",    # Unit where the intervention occurred
                          i_time             = 2015.00,      # Time period when the intervention occurred
                          generate_placebos  = T             # Generate placebo synthetic controls (for inference)
        ) %>%
        # Covariates to fit
        generate_predictor(time_window       = seq(2012, 2014.75, 0.25),   
                           white             = mean(white, na.rm = T),
                           mixed             = mean(mixed, na.rm = T),
                           young             = mean(young, na.rm = T),
                           education         = mean(education, na.rm = T),
                           literacy          = mean(literacy, na.rm = T)) %>%
        # Ys to fit
        generate_predictor(time_window       = 2014.75,  
                           y_2014_4          = y) %>%
        generate_predictor(time_window       = 2014.5,  
                           y_2014_3          = y) %>%
        generate_predictor(time_window       = 2014.25,  
                           y_2014_2          = y) %>%
        generate_predictor(time_window       = 2014,  
                           y_2014_1          = y) %>%
        generate_predictor(time_window       = 2013.75,  
                           y_2013_4          = y) %>%
        generate_predictor(time_window       = 2013.5,  
                           y_2013_3          = y) %>%
        generate_predictor(time_window       = 2013.25,  
                           y_2013_2          = y) %>%
        generate_predictor(time_window       = 2013, 
                           y_2013_1          = y) %>%
        generate_predictor(time_window       = 2012.75,  
                           y_2012_4          = y) %>%
        generate_predictor(time_window       = 2012.5,  
                           y_2012_3          = y) %>%
        generate_predictor(time_window       = 2012.25,  # notice I skipped the first period for y
                           y_2012_2          = y) %>%
        # Generate the fitted weights for the synthetic control
        generate_weights(optimization_window = seq(2012, 2014.75, 0.25), # time to use in the optimization task
                         margin_ipop         = .02, sigf_ipop = 7, bound_ipop = 6  # opt options
        ) %>%
        # Generate the synthetic control
        generate_control()
    
    return(synth_obj)
}

    
################################################################################
# Array of dataset names and the synth generator loop
################################################################################

var_list <- ls(pattern = "pnad_")  # Veeeeery simple pattern detector
                                   # It grabs the vars with similar names 
                                   #   in the environment.

# Make sure that your environment has "pnad_" variables only related to
#    the aggregation step (2_pnad_aggregator)

synth_results <- list()

for(i in var_list) {
    print(paste0("Starting ", i))
    synth_results[[i]] <- synth_fun(get(i))

}



var_list <- ls(pattern = "pnad_")

title_list <- var_list %>% 
    str_replace("pnad_", "") %>% 
    str_replace("_", " ") %>% 
    str_to_title()

title_list[6] <- "Formal Wage"
title_list[9] <- "Informal Wage"

title_list <- c(title_list[6],
                title_list[9],
                title_list[11],
                title_list[1],
                title_list[2],
                title_list[3],
                title_list[4])



var_list <-   c(var_list[6],
                var_list[9],
                var_list[11],
                var_list[1],
                var_list[2],
                var_list[3],
                var_list[4])

################################################################################
# 
# Output the results
#
################################################################################


################################################################################
# Trends
################################################################################

plots <- list()

for (i in var_list) {
    
    plots[[i]] <- synth_results[[i]] %>% plot_trends()+
        ylab("Percentage")+
        ggtitle(paste0("Trends: ",title_list[match(i, var_list)]))+
        theme(text = element_text(size = 14)) 
}

p_trend <- (plots[[1]] + plots[[2]] + plots[[3]])/ (plots[[4]] + plots[[5]] + plots[[6]]) +
    plot_annotation(tag_levels = 'A') 


ggsave("../Output/s_trends.png", unit = "in",
       height = 8, width = 11, dpi = 300, scaling = 0.8)


agg_png

################################################################################
# MSPE
################################################################################

plots <- list()

for (i in var_list) {
    
    plots[[i]] <- synth_results[[i]] %>% plot_mspe_ratio()+
        #ylab("Percentage")+
        ggtitle(paste0("Weights: ",title_list[match(i, var_list)]))+
        theme(text = element_text(size = 12)) 
}

p_mspe <- (plots[[1]] + plots[[2]])/(plots[[3]] + plots[[4]])/(plots[[5]] + plots[[6]]) +
    plot_annotation(tag_levels = 'A')
p_mspe



ggsave("../Output/s_mspe.png", unit = "in",
       height = 11, width = 11, dpi = 300, scaling = 0.8)


################################################################################
# Weights
################################################################################

plots <- list()

for (i in var_list) {
    
    plots[[i]] <- synth_results[[i]] %>% plot_weights()+
        #ylab("Percentage")+
        ggtitle(paste0("Weights: ",title_list[match(i, var_list)]))
}

p_weights <- (plots[[1]] + plots[[2]])/(plots[[3]] + plots[[4]])/(plots[[5]] + plots[[6]]) +
    plot_annotation(tag_levels = 'A')
p_weights

ggsave("../Output/s_weights.png", unit = "in",
       height = 11, width = 11, dpi = 300, scaling = 0.8)


################################################################################
# Placebos
################################################################################

plots <- list()

for (i in var_list) {
    
    plots[[i]] <- synth_results[[i]] %>% plot_placebos()+
        #ylab("Percentage")+
        ggtitle(paste0("Weights: ",title_list[match(i, var_list)]))
}

p_placebos <- (plots[[1]] + plots[[2]])/(plots[[3]] + plots[[4]])/(plots[[5]] + plots[[6]]) +
    plot_annotation(tag_levels = 'A')
p_placebos

ggsave("../Output/s_placebos.png", unit = "in",
       height = 8, width = 11, dpi = 300, scaling = 0.8)