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
         desocupado = if_else(cae_especifico >= 8 & cae_especifico <= 9, 1, 0)) # unemployed

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

## -----------------------------------------------------------------------------
insumos_prop <- create_prop(var = "desocupado", domains = "sexo", subpop = "fdt", design =  dc_ene) # proportion of unemployed people
insumos_total <-  create_size(var = "desocupado", domains = "sexo", subpop = "fdt", design =  dc_ene) # number of unemployed people

## ---- include=F---------------------------------------------------------------
insumos_total

## -----------------------------------------------------------------------------
desagregar <- create_prop(var = "desocupado", domains = "sexo+region", subpop = "fdt", design =  dc_ene)

## ---- eval=F------------------------------------------------------------------
#  
#  eclac_inputs <-  create_prop(var = "desocupado", domains = "sexo+region", subpop = "fdt", design =  dc_ene, eclac_input = TRUE)

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

## ---- eval=T, warning=FALSE---------------------------------------------------
evaluacion_prop <- assess(insumos_prop)
evaluacion_tot <- assess(insumos_total)
evaluacion_suma <- assess(insumos_suma)
evaluacion_media <- assess(insumos_media)

## -----------------------------------------------------------------------------
# Desempleo desagregado por region
desagregar <- create_size(var = "desocupado", domains = "region", subpop = "fdt", design =  dc_ene)
# Evaluar tabulado
evaluacion_tot_desagreg <- assess(desagregar, publicar = T)

## -----------------------------------------------------------------------------
# Reset original options
options(old_options)

