library(tidyverse)
library(tidymodels)

# data available online at https://github.com/aervinp/machine_learning_pov_pry/tree/main/data

data <- read_rds("C:/Users/paerv/machine_learning_pov_pry/data/hh_merged_allyears_cleaned.rds")

write_rds(data, "data/hhfile_original.rds")

# Recipe to clean and standardize data --------------------------------------------

hh_rec <- recipe(lnipcm ~ ., data = data) %>%
  update_role(c("upm", "nvivi", "nhoga", "year", "fex", "facpob", "area", "ipcm", 
                "linea_pobreza_total", "linea_pobreza_extrema", "totpov", "extpov"), new_role = "ID") %>%
  step_impute_median(all_numeric_predictors()) %>% 
  step_impute_mode(all_nominal_predictors()) %>%
  step_mutate(jefe_tipo_empleo1 = as_factor(case_when(jefe_tipo_empleo %in% c("Domestico", "Cuenta_propia", "No_trabajo", "No_remunerado") ~ "Low",
                                                      TRUE ~ "High")),
              perc_tiene_trabajo_remunerado = hh_tiene_trabajo_remunerado/(hh_totpers),
              hh_tipo_hogar1 = as_factor(case_when(hh_tipo_hogar=="Unipersonal" ~ "Unipersonal",
                                                   TRUE ~ "MoreThanOne")),
              jefe_idioma1 = as_factor(case_when(jefe_idioma %in% c("No_habla", "Guarani") ~ "LowInc",
                                                 TRUE ~ "HigherInc")),
              jefe_aniosestudio1 = as_factor(case_when(jefe_aniosestudio %in% c(0:6) ~ "Low",
                                                       jefe_aniosestudio %in% c(6:12) ~ "Medium",
                                                       jefe_aniosestudio>12 ~ "High")),
              jefe_estado_civil1 = as_factor(case_when(jefe_estado_civil %in% c("Casado", "Unido") ~ "Together",
                                                       TRUE ~ "Alone")),
              dptorep1 = as_factor(case_when(dptorep %in% c("Central", "Alto_Parana") ~ "High",
                                             dptorep %in% c("Resto", "San_Pedro", "Itapua") ~ "Medium",
                                             TRUE ~ "Low")),
              vivi_pared1 = as_factor(case_when(vivi_pared %in% c("Madera", "Adobe", "No_tiene") ~ "Low",
                                                TRUE ~ "High")),
              vivi_agua_proveedor1 = as_factor(case_when(vivi_agua_proveedor %in% c("Artesiano", "Lluvia", "ESSAP", 
                                                                                    "Privada", "Pozo_bomba") ~ "High",
                                                         TRUE ~ "Low")),
              vivi_agua_proveedor_beber1 = as_factor(case_when(vivi_agua_proveedor_beber %in% c("Artesiano", "Embotellada", "Lluvia", "ESSAP", 
                                                                                                "Privada", "Manantial_protegido", "Pozo_protegido") ~ "High",
                                                               TRUE ~ "Low")),
              vivi_combustible1 = as_factor(case_when(vivi_combustible %in% c("Ninguno", "Gas", "Electricidad") ~ "High",
                                                      TRUE ~ "Low")),
              vivi_lote_propiedad1 = as_factor(case_when(vivi_lote_propiedad=="Propio" ~ "Propio",
                                                         TRUE ~ "Other")),
              vivi_piso1 = as_factor(case_when(vivi_piso %in% c("Porcelanato", "Parquet", "Otro", "Baldosa", "Madera") ~ "high",
                                               vivi_piso %in% c("Ladrillo", "Lecherada") ~ "mid",
                                               vivi_piso %in% c("Tierra") ~ "low")),
              vivi_agua_fuente1 = as_factor(case_when(vivi_agua_fuente %in% c("Caneria_vivienda") ~ "high",
                                                      vivi_agua_fuente %in% c("Otros", "Vecino", "Caneria_terreno", "Pozo_terreno") ~ "low")),
              vivi_agua_fuente_beber1 = as_factor(case_when(vivi_agua_fuente_beber %in% c("Embotellada", "Canaeria_vivienda") ~ "high",
                                                            vivi_agua_fuente_beber %in% c("Caneria_terreno", "Pozo_terreno", "Vecino", "Otros", "Canilla_publica") ~ "low")),
              vivi_basura1 = as_factor(case_when(vivi_basura %in% c("Vertedero_municipal", "Recoleccion_publica", "Recoleccion_privada") ~ "high",
                                                 vivi_basura %in% c("Otro", "Arroyo", "Hoyo", "Chacra", 
                                                                    "Patio", "Quema") ~ "low")),
              vivi_techo1 = as_factor(case_when(vivi_techo %in% c("Palma", "Hormigon", "Teja", "Otro") ~ "high",
                                                vivi_techo %in% c("Zinc") ~ "mid",
                                                vivi_techo %in% c("Paja", "Fibrocemento", "Madera", "Carton") ~ "low")),
              vivi_banho_desague1 = as_factor(case_when(vivi_banho_desague %in% c("Camara_septica") ~ "high",
                                                        vivi_banho_desague %in% c("Pozo_ciego", "Letrina_ventilada", "Letrina_comun", "Letrina_comun_sin_techo", "Red_sanitario", "Hoyo_abierto", "Otro", "No_banho") ~ "low")),
              piezas_por_miembro = case_when(vivi_piezas == 0 ~ 1/hh_totpers,
                                             vivi_piezas > 0 ~ vivi_piezas/hh_totpers),
              vivi_vivienda_propiedad1 = as_factor(case_when(vivi_vivienda_propiedad=="Propia" ~ "Own",
                                                             TRUE ~ "Other"))) %>% 
  step_rm(jefe_tipo_empleo, hh_tiene_trabajo_remunerado, hh_tiene_trabajo_noremunerado, hh_tipo_hogar,
          jefe_idioma, jefe_estado_civil, dptorep, vivi_pared, vivi_agua_proveedor, vivi_agua_proveedor_beber, vivi_agua_proveedor_beber1,
          vivi_combustible, vivi_lote_propiedad, vivi_piso, vivi_agua_fuente, vivi_agua_fuente_beber, vivi_agua_fuente_beber1,
          vivi_basura, vivi_techo, vivi_banho_desague, vivi_piezas, hh_dependents, hh_youth_dependents, hh_old_dependents, hh_males,
          hh_miembros_5ymenos, hh_miembros_6a14, hh_miembros_15a64, hh_miembros_65ymas, hh_females, vivi_celular, vivi_tableta, vivi_termocalefon, vivi_antena_parabolica,
          vivi_vivienda_propiedad, vivi_dormitorios, vivi_techo1, jefe_caja_jubilacion1, jefe_aniosestudio1) %>% 
  step_log(hh_totpers, piezas_por_miembro) %>% 
  step_dummy(all_nominal_predictors()) %>%
  step_zv(all_numeric_predictors()) %>%
  check_missing(all_predictors())


hh_rec_norm <- recipe(lnipcm ~ ., data = data) %>%
  update_role(c("upm", "nvivi", "nhoga", "year", "fex", "facpob", "area", "ipcm", 
                "linea_pobreza_total", "linea_pobreza_extrema", "totpov", "extpov"), new_role = "ID") %>%
  step_impute_median(all_numeric_predictors()) %>% 
  step_impute_mode(all_nominal_predictors()) %>%
  step_mutate(jefe_tipo_empleo1 = as_factor(case_when(jefe_tipo_empleo %in% c("Domestico", "Cuenta_propia", "No_trabajo", "No_remunerado") ~ "Low",
                                                      TRUE ~ "High")),
              perc_tiene_trabajo_remunerado = hh_tiene_trabajo_remunerado/(hh_totpers),
              hh_tipo_hogar1 = as_factor(case_when(hh_tipo_hogar=="Unipersonal" ~ "Unipersonal",
                                                   TRUE ~ "MoreThanOne")),
              jefe_idioma1 = as_factor(case_when(jefe_idioma %in% c("No_habla", "Guarani") ~ "LowInc",
                                                 TRUE ~ "HigherInc")),
              jefe_aniosestudio1 = as_factor(case_when(jefe_aniosestudio %in% c(0:6) ~ "Low",
                                                       jefe_aniosestudio %in% c(6:12) ~ "Medium",
                                                       jefe_aniosestudio>12 ~ "High")),
              jefe_estado_civil1 = as_factor(case_when(jefe_estado_civil %in% c("Casado", "Unido") ~ "Together",
                                                       TRUE ~ "Alone")),
              dptorep1 = as_factor(case_when(dptorep %in% c("Central", "Alto_Parana") ~ "High",
                                             dptorep %in% c("Resto", "San_Pedro", "Itapua") ~ "Medium",
                                             TRUE ~ "Low")),
              vivi_pared1 = as_factor(case_when(vivi_pared %in% c("Madera", "Adobe", "No_tiene") ~ "Low",
                                                TRUE ~ "High")),
              vivi_agua_proveedor1 = as_factor(case_when(vivi_agua_proveedor %in% c("Artesiano", "Lluvia", "ESSAP", 
                                                                                    "Privada", "Pozo_bomba") ~ "High",
                                                         TRUE ~ "Low")),
              vivi_agua_proveedor_beber1 = as_factor(case_when(vivi_agua_proveedor_beber %in% c("Artesiano", "Embotellada", "Lluvia", "ESSAP", 
                                                                                                "Privada", "Manantial_protegido", "Pozo_protegido") ~ "High",
                                                               TRUE ~ "Low")),
              vivi_combustible1 = as_factor(case_when(vivi_combustible %in% c("Ninguno", "Gas", "Electricidad") ~ "High",
                                                      TRUE ~ "Low")),
              vivi_lote_propiedad1 = as_factor(case_when(vivi_lote_propiedad=="Propio" ~ "Propio",
                                                         TRUE ~ "Other")),
              vivi_piso1 = as_factor(case_when(vivi_piso %in% c("Porcelanato", "Parquet", "Otro", "Baldosa", "Madera") ~ "high",
                                               vivi_piso %in% c("Ladrillo", "Lecherada") ~ "mid",
                                               vivi_piso %in% c("Tierra") ~ "low")),
              vivi_agua_fuente1 = as_factor(case_when(vivi_agua_fuente %in% c("Caneria_vivienda") ~ "high",
                                                      vivi_agua_fuente %in% c("Otros", "Vecino", "Caneria_terreno", "Pozo_terreno") ~ "low")),
              vivi_agua_fuente_beber1 = as_factor(case_when(vivi_agua_fuente_beber %in% c("Embotellada", "Canaeria_vivienda") ~ "high",
                                                            vivi_agua_fuente_beber %in% c("Caneria_terreno", "Pozo_terreno", "Vecino", "Otros", "Canilla_publica") ~ "low")),
              vivi_basura1 = as_factor(case_when(vivi_basura %in% c("Vertedero_municipal", "Recoleccion_publica", "Recoleccion_privada") ~ "high",
                                                 vivi_basura %in% c("Otro", "Arroyo", "Hoyo", "Chacra", 
                                                                    "Patio", "Quema") ~ "low")),
              vivi_techo1 = as_factor(case_when(vivi_techo %in% c("Palma", "Hormigon", "Teja", "Otro") ~ "high",
                                                vivi_techo %in% c("Zinc") ~ "mid",
                                                vivi_techo %in% c("Paja", "Fibrocemento", "Madera", "Carton") ~ "low")),
              vivi_banho_desague1 = as_factor(case_when(vivi_banho_desague %in% c("Camara_septica") ~ "high",
                                                        vivi_banho_desague %in% c("Pozo_ciego", "Letrina_ventilada", "Letrina_comun", "Letrina_comun_sin_techo", "Red_sanitario", "Hoyo_abierto", "Otro", "No_banho") ~ "low")),
              piezas_por_miembro = case_when(vivi_piezas == 0 ~ 1/hh_totpers,
                                             vivi_piezas > 0 ~ vivi_piezas/hh_totpers),
              vivi_vivienda_propiedad1 = as_factor(case_when(vivi_vivienda_propiedad=="Propia" ~ "Own",
                                                             TRUE ~ "Other"))) %>% 
  step_rm(jefe_tipo_empleo, hh_tiene_trabajo_remunerado, hh_tiene_trabajo_noremunerado, hh_tipo_hogar,
          jefe_idioma, jefe_estado_civil, dptorep, vivi_pared, vivi_agua_proveedor, vivi_agua_proveedor_beber, vivi_agua_proveedor_beber1,
          vivi_combustible, vivi_lote_propiedad, vivi_piso, vivi_agua_fuente, vivi_agua_fuente_beber, vivi_agua_fuente_beber1,
          vivi_basura, vivi_techo, vivi_banho_desague, vivi_piezas, hh_dependents, hh_youth_dependents, hh_old_dependents, hh_males,
          hh_miembros_5ymenos, hh_miembros_6a14, hh_miembros_15a64, hh_miembros_65ymas, hh_females, vivi_celular, vivi_tableta, vivi_termocalefon, vivi_antena_parabolica,
          vivi_vivienda_propiedad, vivi_dormitorios, vivi_techo1, jefe_caja_jubilacion1, jefe_aniosestudio1) %>% 
  step_log(hh_totpers, piezas_por_miembro) %>% 
  step_dummy(all_nominal_predictors()) %>%
  step_normalize(all_numeric_predictors()) %>%
  step_zv(all_numeric_predictors()) %>%
  check_missing(all_predictors())

# create data and export as csv -----------------------------------------------------------------

hh_prep <- hh_rec %>% prep() %>% bake(new_data = data)
hh_prep_normalized <- hh_rec_norm %>% prep() %>% bake(new_data = data)

write_csv(hh_prep, "data/hhfile_prepped.csv")
write_csv(hh_prep_normalized, "data/hhfile_prepped_normalized.csv")