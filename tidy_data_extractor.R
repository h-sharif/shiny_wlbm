# following libraries have to be loaded in shiny app
# library(data.table)
# library(tidyverse)
# library(readxl)

# a function to read GoldSim excel file results and prepare daily discharge,
# concentration and loads as well as a table for monthly and annual summaries,
# so it will be two data.frames for each type of variables

# model node names is based on the shiny app selectInput control:
# "El Plomo", "Dolores", "Tranque Plomito", "Planta Molienda Bronces",
# "Planta Molienda Confluencia", "Muro Cortafugas", "Confluence Area",
# "SF-VTO", "Upstream SF Tunnel", "Valenzuela Intake", 
# "Perez Caldera Tailings", "SF Tunnel", "Los Pitches Intake",
# "SF2400 Dren Sump", "Hotel Intake", "SF-PM", "Estero Yerba Loca",
# "Rio Molina", "Rio Mapocho"

# need to link these with available column & sheet names
# there is no input as it reads entire lightweight data set 
df_retriver <- function() {
  df_base <- fread(
    "timeseries_data/Daily_Loads_kgday_Discharge_m3s_Concentrations_mgL_v3.2_Scenario_Base.csv"
  )
  df_a <- fread(
    "timeseries_data/Daily_Loads_kgday_Discharge_m3s_Concentrations_mgL_v3.2_Scenario_A.csv"
  )
  df_b <- fread(
    "timeseries_data/Daily_Loads_kgday_Discharge_m3s_Concentrations_mgL_v3.2_Scenario_B.csv"
  )
  df_c <- fread(
    "timeseries_data/Daily_Loads_kgday_Discharge_m3s_Concentrations_mgL_v3.2_Scenario_C.csv"
  )
  df_all <- bind_rows(
    df_base %>%
      mutate(Scenario = "Base") %>%
      relocate(Scenario),
    df_a %>%
      mutate(Scenario = "A") %>%
      relocate(Scenario),
    df_b %>%
      mutate(Scenario = "B") %>%
      relocate(Scenario),
    df_c %>%
      mutate(Scenario = "C") %>%
      relocate(Scenario)
  ) %>%
    pivot_longer(
      cols = c(3:36), names_to = c("Station", "Constituent", "Variable"),
      values_to = "Simulated", names_sep = "_"
    ) %>%
    mutate(Constituent = ifelse(Constituent == "Q", "-", Constituent),
           Variable = ifelse(is.na(Variable), "Discharge", Variable))
  
  
} 



