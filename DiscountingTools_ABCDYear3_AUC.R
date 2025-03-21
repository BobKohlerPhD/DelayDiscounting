library(discountingtools)
library(dplyr)
library(tidyverse)
library(tidyr)
library(plyr)
#############################################################################
year3_dd <-  read.csv( "/Users/bobkohler/Desktop/nc_y_ddis-JBcount-forPython-3yr.csv") 
year3_dd_select <- dplyr::select(year3_dd,  ###ADD VALIDITY CHECK VARIABLE HERE AND FILTER BELOW
                                     src_subject_id,
                                     ddis_scr_val_indif_pnt_5yr,
                                     ddis_scr_val_indif_pnt_1yr,
                                     ddis_scr_val_indif_pnt_3mth,
                                     ddis_scr_val_indif_pnt_1mth,
                                     ddis_scr_val_indif_pnt_1da,
                                     ddis_scr_val_indif_point_6h,
                                     ddis_scr_val_indif_pnt_1week) %>% drop_na()


##these were not character data types for some reason 
year3_dd_select$ddis_scr_val_indif_pnt_5yr <- as.numeric(year3_dd_select$ddis_scr_val_indif_pnt_5yr)
year3_dd_select$ddis_scr_val_indif_pnt_1yr <- as.numeric(year3_dd_select$ddis_scr_val_indif_pnt_1yr)
year3_dd_select$ddis_scr_val_indif_pnt_1mth <- as.numeric(year3_dd_select$ddis_scr_val_indif_pnt_1mth)

############################################################
#####Create long-format dataframe and create numeric delays 
############################################################
year3_dd_select_long <- gather(year3_dd_select, 
                               delay, 
                               indifference,
                               ddis_scr_val_indif_point_6h,
                               ddis_scr_val_indif_pnt_1da, 
                               ddis_scr_val_indif_pnt_1week, 
                               ddis_scr_val_indif_pnt_1mth, 
                               ddis_scr_val_indif_pnt_3mth,
                               ddis_scr_val_indif_pnt_1yr, 
                               ddis_scr_val_indif_pnt_5yr) 

year3_dd_select_long_refactor <- year3_dd_select_long %>% mutate(delay_revalue = revalue(delay, c("ddis_scr_val_indif_point_6h" = 0.008, 
                                                                                                   "ddis_scr_val_indif_pnt_1da" = 0.033, 
                                                                                                   "ddis_scr_val_indif_pnt_1week" = 0.233,
                                                                                                   "ddis_scr_val_indif_pnt_1mth" = 1, 
                                                                                                   "ddis_scr_val_indif_pnt_3mth" = 3,
                                                                                                   "ddis_scr_val_indif_pnt_1yr" = 12,
                                                                                                   "ddis_scr_val_indif_pnt_5yr" = 60))) %>% drop_na()

year3_dd_select_long_refactor$delay_revalue <- as.numeric(year3_dd_select_long_refactor$delay_revalue)

year3_dd_select_long_refactor_filtered <- filter(year3_dd_select_long_refactor, indifference <= 100) ##there were so VERY high indifference values and they shouldnt be above 100 so removed 
#################################
##Run discounting tools function
#################################
results_auc = fit_dd_curves(data = year3_dd_select_long_refactor_filtered,
                      settings = list(Delays     = delay_revalue,
                                      Values     = indifference,
                                      Individual = src_subject_id),
                      maxValue = 100,
                      plan = c('mazur', 'exponential', 'rachlin'),
                      verbose  = TRUE) |>
  dd_analyze(modelSelection = TRUE)

list_auc <- list(results_auc$mbauclog10)


test1 <- sapply(unlist(list_auc, recursive = FALSE), `[[`, 1)
test2 <- sapply(unlist(list_auc, recursive = FALSE), `[[`, 1)
names(test1) <- NULL
auc_numerics <- test1
auc_ids <- names(test2)
auc_nofilter <- data.frame(auc_numerics, auc_ids)
hist(auc_nofilter$auc_numerics, breaks = 20) ##checking assumptions normality

