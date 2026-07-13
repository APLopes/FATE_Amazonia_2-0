
## ============================================================================
## FATE-SEEG Fire manuscript
## Figure 3 – Greenhouse Gas Emissions from Forest Fires
##
## Description:
## Processes Google Earth Engine outputs and FATE model simulations to
## quantify greenhouse gas emissions from forest fires not associated
## with deforestation in the Brazilian Amazon. Generates the analyses,
## source data tables, and visualization used in Figure 3.
##
## Inputs:
## - FATE model outputs derived from Google Earth Engine datasets
## - Greenhouse gas emissions from combustion, mortality, and decomposition
##
## Outputs:
## - Figure 3 (main text)
## - Source data tables used in the figure
##
## Author:
## Aline Pontes Lopes
##
## Public repository version.
## ============================================================================


library(data.table)
library(openxlsx)
library(dplyr)
library(reshape2)
library(ggplot2)
library(egg)


## load integrated model data --------------------------------------------------

path = '/data/GEE_FireDyn_IntegratedModel_AM_output'
 
file.list <- list.files(path = path, full.names = TRUE, recursive = TRUE, pattern = "_Tg_")
file.list=file.list[!grepl("warning",file.list)]

df.list = list();

for (i in 1:length(file.list)) {

  #reading
  df = fread(file = file.list[i])

  #cleaning
  df$`system:index` <- NULL
  df$.geo <- NULL
 
  df.list[[i]] = df # as from GEE

}
rm(df)

df <- bind_rows(df.list)


## main figure, stacked bar plot with CO2, CH4 and NO2 -------------------------

# changing factor order
df.ss = df
df.ss$GHG = factor(df.ss$GHG, levels = c('CO2','CH4','N2O','CO','NOX'))
df.ss$GHG = factor(df.ss$GHG,
                    labels = c(expression("(a) CO"[2]),
                               expression("(b) CH"[4]),
                               expression("(c) N"[2]*"O"),
                               expression("(d) CO"[ ]),
                               expression("(e) NO"[x])))

# color palette
gases.pal = c("#caa8f5","#8332ac","#340949","#07beb8","#73eedc")


# subset CO2, CH4 and NO2
df.main.fig = subset(df, GHG != 'CO' & GHG != 'NOX')
df.main.fig = droplevels(df.main.fig)

options(scipen=999)

# convert CH4 and NO2 to CO2eq
# IPCC AR6 100-year Global Warming Potentials (GWP100)
df.main.fig$Emissions_Tg_Co2eq = ifelse(df.main.fig$GHG == 'CH4', 
                                        df.main.fig$Emissions_Tg*27.2, df.main.fig$Emissions_Tg)
df.main.fig$Emissions_Tg_Co2eq = ifelse(df.main.fig$GHG == 'N2O', 
                                        df.main.fig$Emissions_Tg*273, df.main.fig$Emissions_Tg_Co2eq)

df.main.fig$GHG = factor(df.main.fig$GHG,
                     levels = rev(c('CO2','CH4','N2O')),
                     labels = rev(c(expression("CO"[2]),
                                expression("CH"[4]),
                                expression("N"[2]*"O"))))
# legacy
df.main.fig$Year.num = as.numeric(df.main.fig$Year)
df.main.fig$Year = as.factor(df.main.fig$Year)
df.main.fig$legacy = ifelse(df.main.fig$Year.num > 2022, 1, 0)
df.main.fig$legacy = as.factor(df.main.fig$legacy)
df.main.fig = droplevels(df.main.fig)


# plot

pm = ggplot(df.main.fig, aes(Year, Emissions_Tg_Co2eq, fill = GHG, alpha=legacy)) +
  geom_bar(stat = "identity") +
  geom_hline(yintercept = 0) +
  scale_fill_manual(values = rev(gases.pal[1:3]), 
                    labels = rev(c(expression("CO"[2]),expression("CH"[4]),expression("N"[2]*"O")))) +
  scale_alpha_manual(values = c(0.8,0.4)) +
  scale_y_continuous(expand = c(0,0)) +
  theme_minimal() +
  theme(axis.title = element_text(size = 11, color = 'gray19'),
        axis.line = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text.y = element_text(size = 11, color = 'gray19'),
        axis.text.x = element_text(size = 8, color = 'gray19', angle=90,hjust=0.95,vjust=0.5),
        plot.title = element_text(size=13, face="bold", hjust = 0, color = 'gray19'),
        strip.text = element_text(size=12, face="bold", hjust = 0, color = 'gray19'),
        legend.position = c(0.9,0.8), legend.direction = "vertical") +
  guides(alpha = 'none') +
  labs(x = 'Year', 
       fill = 'Gases', 
       y = expression("Greenhouse gas emissions (Tg CO"[2]*"eq year"^{-1}*')')) + 
  annotate(geom = 'text', x='2024', y=70, label='Legacy from tree mortality',
           size = 4, col = gases.pal[1], alpha=0.8, hjust= 0.1, fontface = 'italic')
x11(); pm

png("/results/Figures/Figure3.png", width = 20, height = 10, units = 'cm', res = 400)
pm
dev.off()

# exporting table
write.xlsx(df.main.fig,
           '/results/Tables/Figure3_data.xlsx')


# Queries for manuscript text --------------------------------------------------

# total Co2eq emissions 1990-2022
sum(subset(df.main.fig, Year.num >=1990 & Year.num<=2022)$Emissions_Tg_Co2eq) 
# 1840.239 Tg

# CO2eq emissions in 1998
sum(subset(df.main.fig, Year.num == 1998)$Emissions_Tg_Co2eq) 
# 56.6 Tg year-1

# Co2eq emissions since 2010
df.query = droplevels(subset(df.main.fig, Year.num >= 2010 & Year.num <= 2022))
df.query = df.query %>%
  dplyr::group_by(Year) %>%
  dplyr::summarise(Emissions_Tg_Co2eq = sum(Emissions_Tg_Co2eq))
# 2010: 93.7 Tg CO2eq
# 2015: 73.9 Tg C02eq
# 2016: 90.6 Tg CO2eq
# 2020: 92.3 Tg CO2eq
# 2022: 124.2 Tg CO2eq

# legacy emissions
sum(subset(df.main.fig, Year.num > 2022 )$Emissions_Tg_Co2eq) # 569.3 Tg year-1

