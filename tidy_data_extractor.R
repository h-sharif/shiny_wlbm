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

# initial version: only using deaccumulated results for key outlets:
# "SF-VTO", "SF-PM", "Estero Yerba Loca", "Rio Molina", "Rio Mapocho"

# Fetch data function ----------------------------------------------------------
# need to link these with available column & sheet names
# there is no input as it reads entire lightweight data set 
df_retriver <- function() {
  if (
    file.exists(
      "timeseries_data/Daily_Loads_kgday_Discharge_m3s_Concentrations_mgL_v3.2_Scenario_Base.csv"
    ) &
    file.exists(
      "timeseries_data/Daily_Loads_kgday_Discharge_m3s_Concentrations_mgL_v3.2_Scenario_A.csv"
    ) &
    file.exists(
      "timeseries_data/Daily_Loads_kgday_Discharge_m3s_Concentrations_mgL_v3.2_Scenario_B.csv"
    ) &
    file.exists(
      "timeseries_data/Daily_Loads_kgday_Discharge_m3s_Concentrations_mgL_v3.2_Scenario_C.csv"
    )
  ) {
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
        cols = c(3:37), names_to = c("Station", "Constituent", "Variable"),
        values_to = "Simulated", names_sep = "_"
      ) %>%
      mutate(Constituent = ifelse(Constituent == "Q", "-", Constituent),
             Variable = ifelse(is.na(Variable), "Discharge", Variable),
             Station = case_when(
               Station == "Mapocho" ~ "Rio Mapocho",
               Station == "Molina" ~ "Rio Molina",
               Station == "YerbaLoca" ~ "Estero Yerba Loca",
               Station == "SFVTO" ~ "SF-VTO",
               Station == "SFPM" ~ "SF-PM",
             ),
             Date = as.Date(Date))
    return(df_all)
  } else {
    return(NULL)
  }
} 


# Discharge functions ----------------------------------------------------------
# monthly mean series in m3/s
# long-term monthly total in mm/month
# annual series in mm/year
# long-term annual mean in mm
# `unit_q` ----
# `Monthly_Mean` ----
# `Annual_Total` ----
# `Monthly_Mean_Total` ----
df_calc_discharge <- function(df) {
  scns <- unique(df$Scenario)
  df_q <- df %>%
    dplyr::filter(Variable == "Discharge",
                  Scenario == scns[1]) %>%
    dplyr::select(-c(Variable, Constituent, Scenario)) %>%
    mutate(Month = lubridate::month(Date, label = TRUE, abbr = TRUE),
           Year = year(Date),
           Month = ordered(Month, levels = month.abb)) %>%
    left_join(
      data.frame(
        Station = c("Rio Mapocho", "Rio Molina", "Estero Yerba Loca",
                    "SF-VTO", "SF-PM"),
        Area_effective_km2 =  c(608.97, 300.46, 108.5, 75, 35.77)
      ),
      by = "Station"
    ) %>%
    mutate(
      unit_q = Simulated / Area_effective_km2 * 1e-6 * 24 * 3600 * 1000
    )
  
  df_q_monthly_series <- df %>%
    dplyr::filter(Variable == "Discharge",
                  Scenario == scns[1]) %>%
    dplyr::select(-c(Variable, Constituent, Scenario)) %>%
    mutate(Month = lubridate::month(Date, label = TRUE, abbr = TRUE),
           Year = year(Date),
           Month = ordered(Month, levels = month.abb)) %>%
    group_by(Station, Year, Month) %>%
    summarise(
      Monthly_Mean = mean(Simulated, na.rm = TRUE),
      .groups = "drop"
    )
  
  df_q_annual <- df_q %>%
    group_by(Station, Year) %>%
    summarise(
      Annual_Total = sum(unit_q, na.rm = TRUE),
      .groups = "drop"
    )
  
  df_q_monthly_longterm <- df_q %>%
    group_by(Station, Year, Month) %>%
    summarise(
      Monthly_Total = sum(unit_q, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    group_by(Station, Month) %>%
    summarise(
      Monthly_Mean_Total = mean(Monthly_Total, na.rm = TRUE),
      .groups = "drop"
    )
    
    return(
      list(
        daily = df_q,
        monthly = df_q_monthly_series,
        longterm_monthly = df_q_monthly_longterm,
        annual = df_q_annual
      )
    )
}



# Concentration function -------------------------------------------------------
# `Monthly_Mean` ----
# `Annual_Mean` ----
# `Monthly_Mean` ----
df_calc_conc <- function(df) {
  df_conc <- df %>%
    dplyr::filter(Variable == "Conc") %>%
    dplyr::select(-c(Variable)) %>%
    mutate(Month = lubridate::month(Date, label = TRUE, abbr = TRUE),
           Year = year(Date),
           Month = ordered(Month, levels = month.abb))
  
  df_conc_monthly_series <- df_conc %>%
    group_by(Scenario, Station, Year, Month, Constituent) %>%
    summarise(
      Monthly_Mean = mean(Simulated, na.rm = TRUE),
      .groups = "drop"
    )
  
  df_conc_annual <- df_conc %>%
    group_by(Scenario, Station, Year, Constituent) %>%
    summarise(
      Annual_Mean = mean(Simulated, na.rm = TRUE),
      .groups = "drop"
    )
  
  df_conc_monthly_longterm <- df_conc_monthly_series %>%
    group_by(Scenario, Station, Month, Constituent) %>%
    summarise(
      Monthly_Mean = mean(Monthly_Mean, na.rm = TRUE),
      .groups = "drop"
    )
  
  return(
    list(
      daily = df_conc,
      monthly = df_conc_monthly_series,
      longterm_monthly = df_conc_monthly_longterm,
      annual = df_conc_annual
    )
  )
  
}




# Load function ----------------------------------------------------------------
# `Monthly_Total` ----
# `Annual_Total` ----
# `Monthly_Mean_Total` ----
df_calc_load <- function(df) {
  df_load <- df %>%
    dplyr::filter(Variable == "Load") %>%
    dplyr::select(-c(Variable)) %>%
    mutate(Month = lubridate::month(Date, label = TRUE, abbr = TRUE),
           Year = year(Date),
           Month = ordered(Month, levels = month.abb))
  
  df_load_monthly_series <- df_load %>%
    group_by(Scenario, Station, Year, Month, Constituent) %>%
    summarise(
      Monthly_Total = sum(Simulated, na.rm = TRUE),
      .groups = "drop"
    )
  
  df_load_annual <- df_load %>%
    group_by(Scenario, Station, Year, Constituent) %>%
    summarise(
      Annual_Total = sum(Simulated, na.rm = TRUE),
      .groups = "drop"
    )
  
  df_load_monthly_longterm <- df_load_monthly_series %>%
    group_by(Scenario, Station, Month, Constituent) %>%
    summarise(
      Monthly_Mean_Total = mean(Monthly_Total, na.rm = TRUE),
      .groups = "drop"
    )
  
  return(
    list(
      daily = df_load,
      monthly = df_load_monthly_series,
      longterm_monthly = df_load_monthly_longterm,
      annual = df_load_annual
    )
  )
}

# test -------------------------------------------------------------------------
# fetched_data <- df_retriver()
# discharge_list <- df_calc_discharge(fetched_data)
# conc_list <- df_calc_conc(fetched_data)
# load_list <- df_calc_load(fetched_data)
