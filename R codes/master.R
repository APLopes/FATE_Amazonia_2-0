# FATE-SEEG Master Analysis Script
# Sources all analysis scripts in the correct order.
#
# Execution order:
#   1. Fig 1 - Deforestation analyses
#   2. Fig 2 - AGB change
#   3. Fig 3 - GEE model output (emissions)
#
# R version: 4.1.2

cat("\n========== Script 1: Deforestation analyses (Fig 1) ==========\n")
source("R codes/1.Script_Deforestation_analyses_Fig1.R")

cat("\n========== Script 2: AGB change analyses (Fig 2) ==========\n")
source("R codes/2.Script_AGB_change_analyses_Fig2.R")

cat("\n========== Script 3: GEE model output (Fig 3) ==========\n")
source("R codes/3.Script_GEE_model_output_Fig3.R")

cat("\n========== All scripts completed ==========\n")
