################################################################################
# Hugo Sant'Anna Rodrigues
#
# @Organization - University of Georgia
# @Project      - Venezuelan Refugees in Roraima
# @Date         - May 2022
# @Description  - Wrangling PNAD
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
library(PNADcIBGE)    # Using the package to apply the deflator

# Small function to negate the boolean operator %in%
`%!in%` <- Negate(`%in%`)     

################################################################################
# RAW PNAD location
################################################################################

setwd("../Data/PNADC_RAW")

filenames <- list.files(pattern=".RData") # Getting the names 
                                          #   of the files in the folder

################################################################################
# Loading function based on a loop
################################################################################

load_pnad <- function(string) {
    
    pnad <- tibble()                # Creating the empty tibble
    print("Loading data:")          # Verbose statements
    print("Starting the loop.")
    
    # - the loop itself
    for (i in string) {             # Getting the variables I want.
        print(paste("Loading ", i)) # Check the dictionary of PNDAC
        load(i)                     # For more information.
        x <- as_tibble(x) %>% 
            select(Ano, Trimestre, UF, Capital, ID_DOMICILIO, Estrato, 
                   V2003, V2007, V2009, V4071, V4076, VD3005, 
                   VD4009, VD4011, VD4031, VD4035,
                   VD4032, VD4033, VD4034, V4012, VD4010, 
                   V4019, V4046, V403312, V403322,
                   V403412, V403422, V405012, V405022,
                   V405112, V405122, V405812,
                   V405912, V4039, V4039C, V4056, V4056C, V3001, V2010, V4029) %>% 
            pnadc_deflator("../deflator.xls")
        print("Loading complete. Binding rows.")
        pnad <- bind_rows(pnad, x) # Binding the dataset.
        print(paste("Removing residual data. ", 100 * which(string == i)/length(string), "% done."))
        rm(x)
        gc()
    }
    return(pnad)
}


################################################################################
# Proper wrangling of PNADC. Renaming variables and etc
################################################################################

pnad <- load_pnad(filenames) # Calling the function

pnad_named <- pnad %>% 
    transmute(
        year                 = Ano,
        quarter              = Trimestre,
        state                = UF,
        capital              = Capital,
        household            = ID_DOMICILIO,
        male                 = case_when(V2007 == "Homem" ~ 1,
                                         TRUE             ~ 0), 
        age                  = V2009,
        occupation           = V4012,
        searching_job        = V4071,
        unemployed_recently  = V4076,
        education            = VD3005,
        contract_nature      = VD4009,
        blue_collar          = VD4011,
        sector               = VD4010,
        inc_main_usual_cash  = as.numeric(V403312),
        inc_main_usual_goods = as.numeric(V403322),
        inc_main_ref_cash    = as.numeric(V403412),
        inc_main_ref_goods   = as.numeric(V403422),
        inc_part_usual_cash  = as.numeric(V405012),
        inc_part_usual_goods = as.numeric(V405022),
        inc_part_ref_cash    = as.numeric(V405112),
        inc_part_ref_goods   = as.numeric(V405122),
        hour_main_usual      = V4039,
        hour_main_ref        = V4039C,
        hour_part_usual      = V4056,
        hour_part_ref        = V4056C,
        main_firm_formal     = V4019,
        part_firm_formal     = V4046,
        literacy             = V3001,
        race                 = V2010,
        clt                  = V4029,
        deflator             = Efetivo
    )

pnad_named <- pnad_named %>% 
    mutate(
        quarter_dec = case_when(    # This is to make a simpler time period based on decimals
            quarter == 1 ~ 0,
            quarter == 2 ~ 25,
            quarter == 3 ~ 50,
            quarter == 4 ~ 75
        ),
        period = as.double(paste(year,".",  quarter_dec, sep = "")), 
        literacy_bin = case_when(literacy == "Sim" ~ 1,
                                 literacy == "N?o" ~ 0
        ),
        white = case_when(race == "Branca" ~ 1,
                          is.na(race) ~ NA_real_,
                          TRUE ~ 0),
        mixed = case_when(race == "Parda" ~ 1,
                          is.na(race) ~ NA_real_,
                          TRUE ~ 0),
        education_year = as.numeric(education) - 1,
        hschool = case_when(education_year >= 12 ~ 1,
                            is.na(education_year) ~ NA_real_,
                            TRUE ~ 0),
        old = case_when(age > 35 ~ 1, 
                        is.na(age) ~ NA_real_,
                        TRUE  ~ 0),
        young = case_when(age <= 35 ~ 1, 
                          is.na(age) ~ NA_real_,
                          TRUE  ~ 0),
    ) %>%   # Dummies for Formal and Informal job market
    mutate(
        formal = case_when(
            contract_nature %in% c("Trabalhador doméstico com carteira de trabalho assinada",
                                   "Empregado no setor privado com carteira de trabalho assinada",
                                   "Empregado no setor público com carteira de trabalho assinada")  ~ 1,
            TRUE ~ 0
        ),
        informal = case_when(
            contract_nature %in% c("Trabalhador doméstico sem carteira de trabalho assinada",
                                   "Empregado no setor privado sem carteira de trabalho assinada",
                                   "Empregado no setor público sem carteira de trabalho assinada")  ~ 1,
            TRUE ~ 0
        ),
    )


################################################################################
# Proper wrangling of PNADC. Renaming variables and etc
################################################################################

setwd("../Data")
write_csv(pnad_named, file = "pnad_clean.csv")
