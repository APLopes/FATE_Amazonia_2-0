# Data

Input datasets and reference outputs for the FATE-SEEG manuscript. During a
Code Ocean Reproducible Run, this folder is mounted at `/data`.

## Reference outputs

Manually created or externally generated figures not produced by the R scripts.
All other figures and source data tables are generated automatically by the
Reproducible Run and saved to `/results/`.

| File | Description | Source |
|------|-------------|--------|
| `figures/Figure1_map.jpg` | Figure 1 Box a --- Burned area map (2020) | GEE `Paper_fig1_map.js` |
| `figures/Figure1_full.png` | Figure 1 complete (boxes a-d) | Assembled in Inkscape |
| `figures/Figure4.png` | Supplementary figure | Pre-rendered |

## Input datasets

### AGB change

Field inventory measurements from burned forest plots in the Brazilian Amazon.

| File | Description | Used by |
|------|-------------|---------|
| `agb_change_allplots_corrected.csv` | Plot-level AGB change corrected data | Script 2 |
| `agb_change_allplots_corrected.xlsx` | Same data in XLSX | Script 2 |

### Desf_Fire_freq

Annual deforestation and fire frequency statistics (MapBiomas Fire + SEEG),
1990--2021. Each subdirectory contains one CSV file per year.

| Directory | Description | Used by |
|-----------|-------------|---------|
| `burn_desf_area-YYYY/` | Burned and deforested area per year | Script 1 |
| `burn_std_for_area-YYYY/` | Burned standing forest area per year | Script 1 |
| `freq_desf-YYYY/` | Fire frequency --- deforested forests | Script 1 |
| `freq_std-YYYY/` | Fire frequency --- standing forests | Script 1 |
| `ysf_desf-YYYY/` | Years-since-fire --- deforested | Script 1 |
| `ysf_std_for_area-YYYY/` | Years-since-fire --- standing | Script 1 |
| `burned_deforested_areas.csv` | Aggregated burned + deforested | Script 1 |
| `burned_standing_areas.csv` | Aggregated burned standing forest | Script 1 |

### GEE_FireDyn_IntegratedModel_AM_output

FATE model outputs from Google Earth Engine. Greenhouse gas emissions
(combustion + mortality + decomposition) for the Brazilian Amazon.

| File | Description | Used by |
|------|-------------|---------|
| `amazonia-CO2_Tg_balance-v2-1-legacy.csv` | CO2 emissions balance | Script 3 |
| `amazonia-CH4_Tg_comb-v2-1-legacy.csv` | CH4 combustion emissions | Script 3 |
| `amazonia-CO_Tg_comb-v2-1-legacy.csv` | CO combustion emissions | Script 3 |
| `amazonia-N2O_Tg_comb-v2-1-legacy.csv` | N2O combustion emissions | Script 3 |
| `amazonia-NOX_Tg_comb-v2-1-legacy.csv` | NOX combustion emissions | Script 3 |
| `amazonia-CO2_Tg_comb_read_warning-v2-1-legacy.csv` | CO2 combustion (warnings) | --- |
| `amazonia-Fate_model_output_metadata-v2-1-legacy.csv` | Model metadata | --- |

## Data sources

All datasets derived from:

- **MapBiomas Fire Collection 2** --- burned area, fire frequency, time-since-fire
- **SEEG Collections 9--10** --- deforestation and regeneration
- **QCN** --- carbon stock maps (MapBiomas/INPE)
- **MODIS MCD64A1** --- NASA burned area (supplementary)
- **Field inventory** --- plot-level AGB measurements

## License

`LICENSE` --- CC0 1.0 Universal. Applies to all data files in this directory.
