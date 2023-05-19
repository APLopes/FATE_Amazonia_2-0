rm(list = ls());
gc();
 
##_____________________________________________________________________________
## SEEG Fogo 
## GEE emissions from forest fires not related to deforestation
## FATE model results - from combustion, mortality and decomposition
## for forests only
## 
## Mapbiomas x MCD64 burned area 
## SM figure
##
## Load spreadsheets from Google Earth, join, export, and plot
##
## Made by: Aline Pontes Lopes, 10/06/2022, edits 05/07/2022, 15/05/2023
##
## Script called 'SEEGFire_AM_Manuscript_Emissions_MCD64_SupFigure_20230515.R' in my PC
##_____________________________________________________________________________


library(data.table)
library(openxlsx)
library(dplyr)
library(reshape2)
library(tidyr)
#library(tidyverse)
library(ggplot2)
library(egg)


# set WD 
# setwd('C:/Users/liepl/OneDrive - inpe.br/IPAM/Codes')
setwd('C:/Users/Usuario/OneDrive - inpe.br/IPAM/Codes')


## load 1986-2020 Mapbiomas-based data -----------------------------------------

load('./Model_outputs_1986_2022_with_legacy_20230426.Rdata')

df.MB.allyears = df;
rm(list = c('df','df.main.fig','df.ss'));

df.MB.allyears$Biome = NULL
df.MB.allyears$Base = 'MapBiomas Fire 1986-2022'
df.MB.allyears = df.MB.allyears[,c("Year","Base","Emissions_Tg","GHG")]


## load 2002-2022 MB data with legacy ------------------------------------------

path = './MCD64_comparison/Mapbiomas_Fire_col2_2001to2022_legacy'

# read all sheets at once, organize and export 

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
df.MB.since2002 = df;
rm(df, df.list)

df.MB.since2002$Biome = NULL
df.MB.since2002$Base = 'MapBiomas Fire 2002-2022'
df.MB.since2002 = df.MB.since2002[,c("Year","Base","Emissions_Tg","GHG")]


## load 2002-2022 MCD64 data with legacy ---------------------------------------

path = './MCD64_comparison/MCD64_2001to2022_legacy'

# read all sheets at once, organize and export 

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
df.MCD.since2002 = df;
rm(df, df.list,file.list,path,i)

df.MCD.since2002$Biome = NULL
df.MCD.since2002$Base = 'MODIS MCD64 2002-2022'
df.MCD.since2002 = df.MCD.since2002[,c("Year","Base","Emissions_Tg","GHG")]


## organizing for ggplot -------------------------------------------------------

df2 <- bind_rows(df.MB.allyears,df.MB.since2002,df.MCD.since2002)
df2$Year = as.factor(df2$Year)
df2$Base = as.factor(df2$Base)
df2$GHG = as.factor(df2$GHG)

df2$GHG = factor(df2$GHG, levels = c('CO2','CH4','N2O','CO','NOX'))
# df2$GHG2 = df2$GHG
# df2$GHG2 = factor(df2$GHG2,
#                     labels = c(expression("(a) CO"[2]),
#                                expression("(b) CH"[4]),
#                                expression("(c) N"[2]*"O"),
#                                expression("(d) CO"[ ]),
#                                expression("(e) NO"[x])))

df2$Year.num = as.numeric(as.character(df2$Year))
df2$legacy = ifelse(df2$Year.num > 2022, 1, 0)
df2$legacy = as.factor(df2$legacy)


# # supplementary figure with all gases --------------------------------------------
# 
# df2$Year = factor(df2$Year, levels = as.character(seq(1986,2036,1)))
# 
# df2$Year.num = as.numeric(as.character(df2$Year))
# df2$legacy = ifelse(df2$Year.num > 2020, 1, 0)
# df2$legacy = as.factor(df2$legacy)
# df2 = droplevels(df2)
# 
# df.text = data.frame(GHG = c('CO2','CH4','N2O','CO','NOX'),
#                      label = c('Legacy from tree mortality', ' ', ' ', ' ', ' '),
#                      Year = rep('2021',5))
# df.text$Year = as.factor(df.text$Year)
# 
# df.text$GHG2 = df.text$GHG
# df.text$GHG2 = factor(df.text$GHG2,
#                       levels = c('CO2','CH4','N2O','CO','NOX'),
#                       labels = c(expression("(a) CO"[2]),
#                                 expression("(b) CH"[4]),
#                                 expression("(c) N"[2]*"O"),
#                                 expression("(d) CO"[ ]),
#                                 expression("(e) NO"[x])))

gases.pal = c("#caa8f5","#8332ac","#340949","#07beb8","#73eedc")


# ps = ggplot(df2) +
#   geom_bar(stat = "identity", 
#            aes(x=Year, y=Emissions_Tg, group=Base2, fill=Base2, alpha=legacy), 
#            position = position_dodge(preserve = "single")) + # fill = "#f94144"
#   geom_hline(yintercept = 0) +
#   #geom_vline(data = df.line, aes(xintercept = x.line)) +
#   #facet_grid(~GHG, labeller = label_parsed, scales = "free") +
#   geom_text(data=df.text, y=55, aes(x=Year, label=label),
#             hjust=0, vjust=1, size = 3, col = 'grey30', fontface = 'italic') +
#   facet_wrap(~GHG2, ncol = 1, labeller = label_parsed, scales = "free") +
#   scale_fill_manual(values=gases.pal[c(1,2,4)]) +
#   scale_alpha_manual(values = c(0.9,0.5)) +
#   scale_x_discrete(drop=F) +
#   theme_minimal() +
#   theme(axis.title = element_text(size = 11, color = 'gray19'),
#         axis.line = element_blank(),
#         panel.grid.minor.y = element_blank(),
#         panel.grid.major.x = element_blank(),
#         panel.grid.minor.x = element_blank(),
#         axis.text.y = element_text(size = 11, color = 'gray19'),
#         axis.text.x = element_text(size = 8, color = 'gray19', angle=90,hjust=0.95,vjust=0.5),
#         plot.title = element_text(size=13, face="bold", hjust = 0, color = 'gray19'),
#         strip.text = element_text(size=12, face="bold", hjust = 0, color = 'gray19'),
#         legend.position = 'top', legend.direction = "horizontal") +
#   labs(x = 'Year', y = expression("Annual GHG emissions (Tg)"),
#        fill = 'Burned area maps:') + # not CO2 eq. yet 
#   guides(alpha = "none")
# 
# x11(); ps
# 
# 
# # pallete SEEG1 = ["#ffc000","#f68b32","#4f6128","#92d050","#d6e3bc"]
# # colorblind palette 
# # cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
# 
# 
# png("./figures/Manuscript_MCD64_Sup_Figure_Each_gas_Tg_20220705.png", width = 20, height = 25, units = 'cm', res = 400)
# ps
# dev.off()


# supplementary figure, stacked bar plot with CO2, CH4 and NO2 ----------------------------------------

# subset CO2, CH4 and NO2                                   
df2.CO2eq = subset(df2, GHG != 'CO' & GHG != 'NOX')
df2.CO2eq = droplevels(df2.CO2eq)

options(scipen=999)

# convert CH4 and NO2 to CO2eq
# according to IPCC AR6: https://www.ercevolution.energy/ipcc-sixth-assessment-report/
# used AR6 100-y GWP 
df2.CO2eq$Emissions_Tg_Co2eq = ifelse(df2.CO2eq$GHG == 'CH4', df2.CO2eq$Emissions_Tg*27.2, df2.CO2eq$Emissions_Tg)
df2.CO2eq$Emissions_Tg_Co2eq = ifelse(df2.CO2eq$GHG == 'N2O', df2.CO2eq$Emissions_Tg*273, df2.CO2eq$Emissions_Tg_Co2eq)

# sum CO2eq
df2.CO2eq.sum = df2.CO2eq %>%
  group_by(Year,Base,legacy) %>%
  summarise(Emissions_Tg_Co2eq = sum(Emissions_Tg_Co2eq))

df2.CO2eq.sum = as.data.frame(df2.CO2eq.sum)

# plot
ps2 = ggplot(df2.CO2eq.sum, aes(Year, Emissions_Tg_Co2eq, group=Base, fill=Base, alpha=legacy)) +
  geom_bar(stat = "identity", position = position_dodge(preserve = "single")) +
  geom_hline(yintercept = 0) +
  scale_fill_manual(values=gases.pal[c(1,2,4)]) +
  scale_alpha_manual(values = c(0.9,0.5)) +
  scale_x_discrete(drop=F) +
  scale_y_continuous(expand=c(0,0),limits = c(0,130)) +
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
        legend.position = 'top', legend.direction = "horizontal") +
  guides(alpha = 'none') +
  labs(x = 'Year', 
       fill = 'Burned area maps:', 
       y = expression("Annual GHG emissions (Tg CO"[2]*"eq)")) + # use when as CO2 eq.
  annotate(geom = 'text', x=38.8, y=128, label='Legacy from tree mortality',
           size = 2.5, col = 'grey30', alpha=0.8, hjust= 0.1, fontface = 'italic') +
  annotate("segment", x = 37.5, xend = 37.5, y = 0, yend = 130, colour = 'grey30', size=0.5, alpha=0.8)
x11(); ps2


# cumulated plot

df2.CO2eq.sum.cum = df2.CO2eq.sum %>% 
                      arrange(Year) %>%
                      group_by(Base) %>%
                      mutate(Emissions_Tg_Co2eq_cum = cumsum(Emissions_Tg_Co2eq)) %>%
                      arrange(Base)

df2.CO2eq.sum.cum = as.data.frame(df2.CO2eq.sum.cum)

# # removing legacy
# df2.CO2eq.sum.cum = subset(df2.CO2eq.sum.cum, legacy=="0")
# df2.CO2eq.sum.cum = droplevels(df2.CO2eq.sum.cum)

ps3 = ggplot(df2.CO2eq.sum.cum, aes(Year, Emissions_Tg_Co2eq_cum, group=Base, fill=Base, alpha=legacy)) +
  geom_bar(stat = "identity", position = position_dodge(preserve = "single")) +
  geom_hline(yintercept = 0) +
  scale_fill_manual(values=gases.pal[c(1,2,4)]) +
  scale_alpha_manual(values = c(0.9,0.5)) +
  scale_x_discrete(drop=F) +
  scale_y_continuous(expand=c(0,0),limits = c(0,2500)) +
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
        legend.position = 'none', legend.direction = "horizontal") +
  guides(alpha = 'none') +
  labs(x = 'Year', 
       fill = 'Burned area maps:', 
       y = expression("Cumulated GHG emissions (Tg CO"[2]*"eq)")) + # use when as CO2 eq.
  annotate(geom = 'text', x=38.8, y=2450, label='Legacy from tree mortality',
           size = 2.5, col = 'grey30', alpha=0.8, hjust= 0.1, fontface = 'italic') +
  annotate("segment", x = 37.5, xend = 37.5, y = 0, yend = 2500, colour = 'grey30', size=0.5, alpha=0.8)

x11(); ps3


png("./figures/Manuscript_MCD64_Sup_ALLgases_cum_Tg_20230519.png", width = 20, height = 15, units = 'cm', res = 400)
ggarrange(ps2,ps3,ncol=1)
dev.off()
