# GEE codes

Google Earth Engine JavaScript scripts implementing the FATE spatial model for greenhouse gas emissions from Amazonian forest fires. The spatial analysis is performed entirely in GEE; the R workflows consume the exported CSV outputs.

## Scripts

| Script | Description | Figure / Output |
|--------|-------------|-----------------|
| `fate_model.js` | Core FATE model — carbon balance (combustion + mortality + decomposition), CO2/CH4/CO/N2O/NOX emissions. Uses MapBiomas Fire Collection 2 time-since-fire (1986–2022) + QCN carbon stocks. | Emissions rasters + Drive CSVs |
| `fate_model_legacy.js` | Legacy version including post-2022 mortality decomposition ("legacy" emissions). | Emissions rasters + Drive CSVs |
| `fate_model_mapbiomas-fire-collection2-2002-2023.js` | Core model variant sliced to 2002–2023 window. | Emissions rasters + Drive CSVs |
| `fate_model_legacy_mapbiomas-fire-collection2-2002-2023.js` | Legacy variant sliced to 2002–2023 window. | Emissions rasters + Drive CSVs |
| `fate_model_nasa-mcd64a1-v6-2002-2023.js` | Model variant using NASA MCD64A1 burned area product (2002–2023) as fire input instead of MapBiomas. | Emissions rasters + Drive CSVs |
| `fate_model_legacy_nasa-mcd64a1-v6-2002-2023.js` | Legacy variant using NASA MCD64A1 burned area product. | Emissions rasters + Drive CSVs |
| `Paper_fig1_data.js` | Generates burned/deforested area statistics per year for Figure 1. Uses Collection 1 fire frequency and annual burned coverage, SEEG deforestation/regeneration, and 10km grid aggregation. | `grid-ysf_forest_stable-and-ysf_deforestation` (FeatureCollection) |
| `Paper_fig1_map.js` | Generates the Figure 1 map panel — spatial visualization of years-since-fire, burned area, and deforestation in the Amazon biome. | Map rendering (no export) |

## GEE Data Assets

All scripts require access to assets in the following GEE projects. Paths were updated in July 2026 to reflect the current MapBiomas workspace reorganization.

| Asset | Path | Used by |
|-------|------|---------|
| Stable forest mask (SEEG C10) | `projects/mapbiomas-workspace/SEEG/2023/c10/2_0_Mask_stable` | `Paper_fig1_map.js` |
| Stable forest mask — band 2020 | `projects/mapbiomas-workspace/SEEG/2023/c10/2_0_Mask_stable/SEEG_c10_v_0_29_2020` | All `fate_model*.js` |
| QCN carbon stocks (30 m) | `projects/mapbiomas-workspace/SEEG/2022/QCN/QCN_30m_BR_v2_0_1` | All scripts |
| Time after fire (MapBiomas Fire C2) | `projects/mapbiomas-workspace/FOGO/COLLECTIONS/COL02/PRODUTOS_REGIME_DO_FOGO/mapbiomas-fire-collection2-time-after-fire-v1` | `fate_model.js`, `fate_model_legacy.js`, `Paper_fig1_map.js` |
| Annual burned coverage (C2) | `projects/mapbiomas-workspace/FOGO/COLLECTIONS/COL02/SUBPRODUTOS/mapbiomas-fire-collection2-annual-burned-coverage-v1` | `fate_model.js`, `fate_model_legacy.js`, `fate_model_mapbiomas-fire-collection2-2002-2023.js`, `fate_model_legacy_mapbiomas-fire-collection2-2002-2023.js`, `Paper_fig1_map.js` |
| Fire frequency coverage (C2) | `projects/mapbiomas-workspace/FOGO/COLLECTIONS/COL02/SUBPRODUTOS/mapbiomas-fire-collection2-fire-frequency-coverage-v1` | `fate_model.js`, `fate_model_legacy.js`, `Paper_fig1_map.js` |
| Brazilian biomes (IBGE 2019) | `projects/ee-ipam/assets/ORIGINAIS/IBGE/limite_biomas_IBGE_2019` | All `fate_model*.js`, `Paper_fig1_data.js` |
| Brazilian states (IBGE 2025) | `projects/ee-ipam/assets/ORIGINAIS/IBGE/limite_ufs_IBGE_2025` | `Paper_fig1_map.js` |
| South America countries | `projects/mapbiomas-workspace/AUXILIAR/America_do_Sul` | `Paper_fig1_map.js` |
| Protected areas (2022) | `projects/mapbiomas-workspace/AUXILIAR/areas-protegidas-por-ano-2022/ap2022` | `fate_model_legacy-in_protect_areas.js` |
| Fire frequency (Collection 1) | `projects/mapbiomas-public/assets/brazil/lulc/collection6/mapbiomas-fire-collection1-fire-frequency-1` | `Paper_fig1_data.js` |
| Annual burned (Collection 1) | `projects/mapbiomas-public/assets/brazil/lulc/collection6/mapbiomas-fire-collection1-annual-burned-coverage-1` | `Paper_fig1_data.js` |
| Time after fire — MapBiomas C2 (2002–2023) | `projects/ee-seegfiredyn/assets/mapbiomas-fire-collection2-time-after-fire-v1-2002to2023` | `fate_model_mapbiomas-fire-collection2-2002-2023.js`, `fate_model_legacy_mapbiomas-fire-collection2-2002-2023.js` |
| Time after fire — NASA MCD64A1 (2002–2023) | `projects/ee-seegfiredyn/assets/internal-version-2023-nasa-mcd64a1-time-after-fire-v1-2002to2023` | `fate_model_nasa-mcd64a1-v6-2002-2023.js`, `fate_model_legacy_nasa-mcd64a1-v6-2002-2023.js` |
| Year since fire (Collection 1) | `projects/ee-seegfiredyn/assets/mapbiomas-fire-collection1-year-since-fire-v1` | `Paper_fig1_data.js` |
| 10 km grid | `projects/ee-ipam/assets/SEEG/SEEG13/10grid` | `Paper_fig1_data.js` |
| Deforestation (SEEG Col 9) | `projects/ee-seeg-brazil/assets/collection_9/v1/1_1_Temporal_filter_deforestation` | `Paper_fig1_data.js` |
| Regeneration (SEEG Col 9) | `projects/ee-seeg-brazil/assets/collection_9/v1/1_1_Temporal_filter_regeneration` | `Paper_fig1_data.js` |
| Amazon biome boundary (MB) | `users/camilaflorestal/MAPBIOMAS/mb_biomescopy` | `Paper_fig1_map.js`, `Paper_fig1_data.js` |
| MODIS MCD64A1 burned area | `MODIS/061/MCD64A1` | `fate_model_nasa-mcd64a1-v6-2002-2023.js`, `fate_model_legacy_nasa-mcd64a1-v6-2002-2023.js` |

## Access requirements

- **MapBiomas workspace** (`projects/mapbiomas-workspace/`): requires membership in the MapBiomas GEE project.
- **MapBiomas public** (`projects/mapbiomas-public/`): publicly accessible.
- **IPAM assets** (`projects/ee-ipam/`): requires membership in the IPAM GEE project.
- **INPE SEEG-FireDyn** (`projects/ee-seegfiredyn/`): requires membership in the INPE SEEG-FireDyn project.
- **INPE SEEG-Brazil** (`projects/ee-seeg-brazil/`): requires membership in the INPE SEEG-Brazil project.
- **User assets** (`users/camilaflorestal/`): depends on sharing settings of the owner.
- **MODIS**: publicly available.

## Contacts

- Camila Silva <camila.silva@ipam.org.br>
- Aline Pontes <alineplopes@gmail.com>
- Wallace Silva <wallace.silva@ipam.org.br>
- Celso H. L. Silva-Junior <celso.junior@ipam.org.br>
