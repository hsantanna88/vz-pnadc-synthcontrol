################################################################################
# Hugo Sant'Anna Rodrigues
#
# @Organization - University of Georgia
# @Project      - Venezuelan Refugees in Roraima
# @Date         - May 2022
# @Description  - Preparing PNADC for Synth control
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

# Small function to negate the boolean operator %in%
`%!in%` <- Negate(`%in%`)     

################################################################################
# Getting the data
################################################################################

pnad <- read_csv("../Data/PNADC/pnad.csv")


################################################################################
# Aggregating Formal Wages
################################################################################

pnad_formal <- pnad %>%
    filter(state %!in% c("São Paulo", "Amazonas", 
                         "Rio de Janeiro", "Distrito Federal")) %>% 
    filter(year < 2018) %>% 
    filter(formal == 1) %>%
    group_by(period, state) %>%
    mutate(labor_income = log(deflator * inc_main_ref_cash + 1)) %>% 
    summarise(
        y             = mean(labor_income, na.rm = T), 
        quarter       = mean(as.integer(quarter)),
        education     = mean(education_year, na.rm = T),
        white         = mean(white, na.rm = T),
        mixed         = mean(mixed, na.rm = T),
        hschool       = mean(hschool, na.rm = T),
        old           = mean(old, na.rm = T),
        young         = mean(young, na.rm = T),
        literacy      = mean(literacy_bin, na.rm = T)
    ) %>%
    ungroup() %>% 
    mutate(state = as.character(state))


################################################################################
# Aggregating Informal Wages
################################################################################

pnad_informal <- pnad %>%
    filter(state %!in% c("São Paulo", "Amazonas", 
                         "Rio de Janeiro", "Distrito Federal")) %>% 
    filter(year < 2018) %>% 
    filter(informal == 1) %>%
    group_by(period, state) %>%
    mutate(labor_income = log(deflator * inc_main_ref_cash + 1)) %>% 
    summarise(
        y             = mean(labor_income, na.rm = T), 
        quarter       = mean(as.integer(quarter)),
        education     = mean(education_year, na.rm = T),
        white         = mean(white, na.rm = T),
        mixed         = mean(mixed, na.rm = T),
        hschool       = mean(hschool, na.rm = T),
        old           = mean(old, na.rm = T),
        young         = mean(young, na.rm = T),
        literacy      = mean(literacy_bin, na.rm = T)
    ) %>%
    ungroup() %>% 
    mutate(state = as.character(state))


################################################################################
# Aggregating LFP Data
################################################################################

pnad_lfp <- pnad %>%
    filter(state %!in% c("São Paulo", "Amazonas",
                         "Rio de Janeiro", "Distrito Federal")) %>% 
    filter(year < 2018) %>% 
    filter(age >= 14) %>%               # getting only working age people
    mutate(lfp = ifelse(!is.na(occupation) | searching_job == "Sim", 1, 0)) %>% 
    group_by(period, state) %>%
    summarise(
        y             = mean(lfp, na.rm = T), 
        quarter       = mean(as.integer(quarter)),
        education     = mean(education_year, na.rm = T),
        white         = mean(white, na.rm = T),
        mixed         = mean(mixed, na.rm = T),
        hschool       = mean(hschool, na.rm = T),
        old           = mean(old, na.rm = T),
        young         = mean(young, na.rm = T),
        literacy      = mean(literacy_bin, na.rm = T)
    ) %>%
    ungroup() %>% 
    mutate(state = as.character(state))

################################################################################
# Aggregating female LFP Data
################################################################################

pnad_female_lfp <- pnad %>%
    filter(state %!in% c("São Paulo", "Amazonas", 
                         "Rio de Janeiro", "Distrito Federal")) %>% 
    filter(year < 2018) %>% 
    filter(age >= 14) %>%               # getting only working age people
    filter(male == 0) %>%               # female only
    mutate(lfp = ifelse(!is.na(occupation) | searching_job == "Sim", 1, 0)) %>% 
    group_by(period, state) %>%
    summarise(
        y             = mean(lfp, na.rm = T), 
        quarter       = mean(as.integer(quarter)),
        education     = mean(education_year, na.rm = T),
        white         = mean(white, na.rm = T),
        mixed         = mean(mixed, na.rm = T),
        hschool       = mean(hschool, na.rm = T),
        old           = mean(old, na.rm = T),
        young         = mean(young, na.rm = T),
        literacy      = mean(literacy_bin, na.rm = T)
    ) %>%
    ungroup() %>% 
    mutate(state = as.character(state))


################################################################################
# Aggregating entrepreneurs Data
################################################################################

pnad_entrepreneur <- pnad %>%
    filter(state %!in% c("São Paulo", "Amazonas", 
                         "Rio de Janeiro", "Distrito Federal")) %>% 
    filter(year < 2018) %>% 
    filter(age >= 14) %>%               # getting only working age people
    mutate(
        entrepreneur = ifelse(
            occupation == "Conta própria" | occupation == "Empregador", 1, 0
        ),
        lfp          = ifelse(!is.na(occupation) | searching_job == "Sim", 1, 0)
        ) %>% 
    group_by(period, state) %>%
    summarise(
        lfp                        = mean(lfp, na.rm = T),
        y                          = mean(entrepreneur, na.rm = T),
        quarter                    = mean(as.integer(quarter)),
        education                  = mean(education_year, na.rm = T),
        white                      = mean(white, na.rm = T),
        mixed                      = mean(mixed, na.rm = T),
        hschool                    = mean(hschool, na.rm = T),
        old                        = mean(old, na.rm = T),
        young                      = mean(young, na.rm = T),
        literacy                   = mean(literacy_bin, na.rm = T)
    ) %>%
    ungroup() %>% 
    mutate(
        y     = y/lfp,
        state = as.character(state))


################################################################################
# Aggregating female entrepreneurs
################################################################################

pnad_female_entrepreneur <- pnad %>%
    filter(state %!in% c("São Paulo", "Amazonas", "Rio de Janeiro", "Distrito Federal")) %>% 
    filter(year < 2018) %>% 
    filter(age >= 14) %>%       # getting only working age people
    filter(male == 0) %>%       # female only
    mutate(
        entrepreneur = ifelse(
            occupation == "Conta própria" | occupation == "Empregador", 1, 0
        ),
        lfp          = ifelse(!is.na(occupation) | searching_job == "Sim", 1, 0)
    ) %>% 
    group_by(period, state) %>%
    summarise(
        lfp                 = mean(lfp, na.rm = T),
        y                   = mean(entrepreneur, na.rm = T),
        quarter             = mean(as.integer(quarter)),
        education           = mean(education_year, na.rm = T),
        white               = mean(white, na.rm = T),
        mixed               = mean(mixed, na.rm = T),
        hschool             = mean(hschool, na.rm = T),
        old                 = mean(old, na.rm = T),
        young               = mean(young, na.rm = T),
        literacy            = mean(literacy_bin, na.rm = T)
    ) %>%
    ungroup() %>% 
    mutate(
        y     = y/lfp,
        state = as.character(state))




################################################################################
# Aggregating Employment
################################################################################

pnad_employment <- pnad %>%
    filter(state %!in% c("São Paulo", "Amazonas", "Rio de Janeiro", "Distrito Federal")) %>% 
    filter(year < 2018) %>% 
    filter(age >= 14) %>%       # getting only working age people
    mutate(
        employed     = ifelse(is.na(occupation), 0, 1),
        lfp          = ifelse(!is.na(occupation) | searching_job == "Sim", 1, 0)
    ) %>% 
    group_by(period, state) %>%
    summarise(
        lfp                 = mean(lfp, na.rm = T),
        y                   = mean(employed, na.rm = T),
        quarter             = mean(as.integer(quarter)),
        education           = mean(education_year, na.rm = T),
        white               = mean(white, na.rm = T),
        mixed               = mean(mixed, na.rm = T),
        hschool             = mean(hschool, na.rm = T),
        old                 = mean(old, na.rm = T),
        young               = mean(young, na.rm = T),
        literacy            = mean(literacy_bin, na.rm = T)
    ) %>%
    ungroup() %>% 
    mutate(
        y     = y/lfp,
        state = as.character(state))


################################################################################
# Aggregating female Employment
################################################################################

pnad_female_employment <- pnad %>%
    filter(state %!in% c("São Paulo", "Amazonas", "Rio de Janeiro", "Distrito Federal")) %>% 
    filter(year < 2018) %>% 
    filter(age >= 14) %>%       # getting only working age people
    filter(male == 0) %>%       # female only
    mutate(
        employed     = ifelse(is.na(occupation), 0, 1),
        lfp          = ifelse(!is.na(occupation) | searching_job == "Sim", 1, 0)
    ) %>% 
    group_by(period, state) %>%
    summarise(
        lfp                 = mean(lfp, na.rm = T),
        y                   = mean(employed, na.rm = T),
        quarter             = mean(as.integer(quarter)),
        education           = mean(education_year, na.rm = T),
        white               = mean(white, na.rm = T),
        mixed               = mean(mixed, na.rm = T),
        hschool             = mean(hschool, na.rm = T),
        old                 = mean(old, na.rm = T),
        young               = mean(young, na.rm = T),
        literacy            = mean(literacy_bin, na.rm = T)
    ) %>%
    ungroup() %>% 
    mutate(
        y     = y/lfp,
        state = as.character(state))




################################################################################
# Aggregating Informal Employment
################################################################################

pnad_informal_employment <- pnad %>%
    filter(state %!in% c("São Paulo", "Amazonas", "Rio de Janeiro", "Distrito Federal")) %>% 
    filter(year < 2018) %>% 
    filter(age >= 14) %>%       # getting only working age people
    mutate(
        employed     = ifelse(is.na(occupation), 0, 1),
        lfp          = ifelse(!is.na(occupation) | searching_job == "Sim", 1, 0)
    ) %>% 
    group_by(period, state) %>%
    summarise(
        lfp                 = mean(lfp, na.rm = T),
        y                   = mean(informal, na.rm = T),
        quarter             = mean(as.integer(quarter)),
        education           = mean(education_year, na.rm = T),
        white               = mean(white, na.rm = T),
        mixed               = mean(mixed, na.rm = T),
        hschool             = mean(hschool, na.rm = T),
        old                 = mean(old, na.rm = T),
        young               = mean(young, na.rm = T),
        literacy            = mean(literacy_bin, na.rm = T)
    ) %>%
    ungroup() %>% 
    mutate(
        y     = y/lfp,
        state = as.character(state))



################################################################################
# Aggregating Formal Employment
################################################################################

pnad_formal_employment <- pnad %>%
    filter(state %!in% c("São Paulo", "Amazonas", "Rio de Janeiro", "Distrito Federal")) %>% 
    filter(year < 2018) %>% 
    filter(age >= 14) %>%       # getting only working age people
    mutate(
        lfp          = ifelse(!is.na(occupation) | searching_job == "Sim", 1, 0)
    ) %>% 
    group_by(period, state) %>%
    summarise(
        lfp                 = mean(lfp, na.rm = T),
        y                   = mean(formal, na.rm = T),
        quarter             = mean(as.integer(quarter)),
        education           = mean(education_year, na.rm = T),
        white               = mean(white, na.rm = T),
        mixed               = mean(mixed, na.rm = T),
        hschool             = mean(hschool, na.rm = T),
        old                 = mean(old, na.rm = T),
        young               = mean(young, na.rm = T),
        literacy            = mean(literacy_bin, na.rm = T)
    ) %>%
    ungroup() %>% 
    mutate(
        y     = y/lfp,
        state = as.character(state))


################################################################################
# Aggregating Formality
################################################################################

pnad_formality <- pnad %>%
    filter(state %!in% c("São Paulo", "Amazonas", "Rio de Janeiro", "Distrito Federal")) %>% 
    filter(year < 2018) %>% 
    filter(age >= 14) %>%       # getting only working age people
    mutate(
        lfp          = ifelse(!is.na(occupation) | searching_job == "Sim", 1, 0)
    ) %>% 
    group_by(period, state) %>%
    summarise(
        informal            = mean(informal, na.rm = T),
        y                   = mean(formal, na.rm = T),
        quarter             = mean(as.integer(quarter)),
        education           = mean(education_year, na.rm = T),
        white               = mean(white, na.rm = T),
        mixed               = mean(mixed, na.rm = T),
        hschool             = mean(hschool, na.rm = T),
        old                 = mean(old, na.rm = T),
        young               = mean(young, na.rm = T),
        literacy            = mean(literacy_bin, na.rm = T)
    ) %>%
    ungroup() %>% 
    mutate(
        y     = y/(y + informal),
        state = as.character(state))



