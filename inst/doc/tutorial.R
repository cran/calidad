## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- message=FALSE, warning=FALSE--------------------------------------------
library(survey)
library(calidad)
library(dplyr)

ene <- ene %>% 
  mutate(fdt = if_else(cae_especifico >= 1 & cae_especifico <= 9, 1, 0), # labour force
         ocupado = if_else(cae_especifico >= 1 & cae_especifico <= 7, 1, 0), # employed
         desocupado = if_else(cae_especifico >= 8 & cae_especifico <= 9, 1, 0),
         hombre = if_else(sexo == 1, 1, 0),
         mujer = if_else(sexo == 2, 1, 0)) # unemployed

# One row per household
epf <- epf_personas %>% 
  group_by(folio) %>% 
  slice(1) %>% 
  ungroup()


## ---- results='hide'----------------------------------------------------------
# Store original options
old_options <-  options()

## -----------------------------------------------------------------------------
# Complex sample design for ENE
dc_ene <- svydesign(ids = ~conglomerado , strata = ~estrato_unico, data = ene, weights = ~fact_cal)

# Complex sample design for EPF
dc_epf <- svydesign(ids = ~varunit, strata = ~varstrat, data = epf, weights = ~fe)

options(survey.lonely.psu = "certainty")


## ---- eval=FALSE--------------------------------------------------------------
#  # Complex sample design for ELE
#  dc_ele <- svydesign(ids = ~rol_ficticio, weights = ~fe_transversal, strata = ~estrato, fpc = ~pob, data = ELE7)
#  
#  options(survey.lonely.psu = 'remove')
#  

## -----------------------------------------------------------------------------
insumos_prop <- create_prop(var = "desocupado", domains = "sexo", subpop = "fdt", design =  dc_ene) # proportion of unemployed people
insumos_total <-  create_size(var = "desocupado", domains = "sexo", subpop = "fdt", design =  dc_ene) # number of unemployed people

## -----------------------------------------------------------------------------
insumos_total

## -----------------------------------------------------------------------------
desagregar <- create_prop(var = "desocupado", domains = "sexo+region", subpop = "fdt", design =  dc_ene)

## ---- eval=F------------------------------------------------------------------
#  
#  eclac_inputs <-  create_prop(var = "desocupado", domains = "sexo+region", subpop = "fdt", design =  dc_ene, eclac_input = TRUE)
#  

## ---- eval=FALSE, warning=FALSE-----------------------------------------------
#  
#  create_prop(var = "mujer", denominator = "hombre", domains = "ocupado", design = dc_ene,
#              eclac_input = TRUE, scheme = 'eclac_2023')
#  

## ---- eval=T, warning=FALSE---------------------------------------------------
insumos_suma <-  create_total(var = "gastot_hd", domains = "zona", design =  dc_epf)

## -----------------------------------------------------------------------------
insumos_media <-  create_mean(var = "gastot_hd", domains = "zona", design =  dc_epf)

## -----------------------------------------------------------------------------
# ENE dataset
insumos_prop_nacional <- create_prop("desocupado", subpop = "fdt", design = dc_ene)
insumos_total_nacional <-  create_total("desocupado", subpop = "fdt", design = dc_ene)

# EPF dataset
insumos_suma_nacional <- create_total("gastot_hd", design = dc_epf)
insumos_media_nacional <-  create_mean("gastot_hd", design = dc_epf)

## -----------------------------------------------------------------------------
# ENE dataset
prop_nacional_ci <- create_prop("desocupado", subpop = "fdt", design = dc_ene, ci = TRUE)                        
prop_nacional_ci_logit <- create_prop("desocupado", subpop = "fdt", design = dc_ene, ci_logit = TRUE) 


## ---- eval=FALSE--------------------------------------------------------------
#  
#  prod_salarial <- create_prop('VA_2022f', denominator = 'REMP_TOTAL', domains = 'cod_actividad+cod_tamano', design = dc_ele)

## ---- eval=F, warning=FALSE---------------------------------------------------
#  
#  # INE Chile
#  evaluacion_prop <- assess(insumos_prop)
#  evaluacion_tot <- assess(insumos_total)
#  evaluacion_suma <- assess(insumos_suma)
#  evaluacion_media <- assess(insumos_media)
#  
#  # ECLAC
#  evaluacion_cepal_2020 <- assess(eclac_inputs, scheme = 'eclac_2020')
#  evaluacion_cepal_2023 <- assess(eclac_inputs, scheme = 'eclac_2023', domain_info = TRUE, low_df_justified = TRUE)
#  
#  

## ---- eval = FALSE------------------------------------------------------------
#  # INE Economics
#  
#  ## target sample size
#  table_n_obj <- ELE7_n_obj %>%
#    dplyr::mutate(cod_actividad = cod_actividad_letra,
#                  cod_tamano = as.character(cod_tamano)) %>%
#    dplyr::select(-cod_actividad_letra)
#  
#  
#  eval_ratio <- assess(prod_salarial, scheme = 'chile_economics',
#                       domain_info = TRUE, table_n_obj = table_n_obj, ratio_between_0_1 = FALSE)
#  

## ----eval=F-------------------------------------------------------------------
#  # Unemployment by region
#  desagregar <- create_size(var = "desocupado", domains = "region", subpop = "fdt", design =  dc_ene)
#  
#  # assess output
#  evaluacion_tot_desagreg <- assess(desagregar, publish = T)
#  evaluacion_tot_desagreg

## ----eval=F-------------------------------------------------------------------
#  # Reset original options
#  options(old_options)

