pacman::p_load(usethis, gitcreds, dplyr, srvyr, haven, ggplot2, sf, ggthemes, ggrepel, plotly, viridis, Hmisc, readxl, sjmisc, leaflet, skimr, psych, sjlabelled, rio, reshape2, corrplot, tidyr)
enusc2023 = import("/Users/arielcardeiro/Desktop/Trabajos CEP antiguos/ENUSC 2023/DATA/base-usuario-20-enusc-2023.sav")


# Configurar enusc2023withcluster como diseño de encuesta
enusc2023withcluster <- enusc2023withcluster %>%
  filter(!is.na(Fact_Pers_Com)) %>%
  as_survey_design(ids = enc_idr, strata = enc_region, weights = Fact_Pers_Com)

# Calcular proporciones
propclust <- enusc2023withcluster %>%
  # Tratar valores 88 y 99 como NA en `rph_pertenencia_indigena`
  mutate(rph_pertenencia_indigena = ifelse(rph_pertenencia_indigena %in% c(88, 99), NA, rph_pertenencia_indigena)) %>%
  group_by(clustering) %>%
  summarise(
    prop_hombres = survey_mean(rph_sexo == 1, na.rm = TRUE), 
    prop_indigenas = survey_mean(rph_pertenencia_indigena == 1, na.rm = TRUE), # Proporción de personas que pertenecen a un pueblo indígena
    prop_0_18 = survey_mean(rph_edad %in% c(0, 1), na.rm = TRUE),              # Proporción 0 a 18 años
    prop_19_30 = survey_mean(rph_edad == 2, na.rm = TRUE),                     # Proporción 19 a 30 años
    prop_31_40 = survey_mean(rph_edad == 3, na.rm = TRUE),                     # Proporción 31 a 40 años
    prop_41_59 = survey_mean(rph_edad == 4 | rph_edad == 5, na.rm = TRUE),     # Proporción 41 a 59 años
    prop_60_mas = survey_mean(rph_edad %in% c(6, 7), na.rm = TRUE),            # Proporción 60 años o más
    prop_educsup = survey_mean(rph_nivel == 3, na.rm = TRUE),                  # Nivel educ superior
    prop_laboral = survey_mean(rph_situacion_laboral_a == 1, na.rm = TRUE),    # Laboral
    prop_nse_bajo = survey_mean(rph_nse == 1, na.rm = TRUE),                   # NSE bajo
    prop_nse_medio = survey_mean(rph_nse == 2, na.rm = TRUE),                  # NSE medio
    prop_nse_alto = survey_mean(rph_nse == 3, na.rm = TRUE)                    # NSE alto
  )
