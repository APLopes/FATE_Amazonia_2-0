rm(list = ls());
gc();
 
##_____________________________________________________________________________
## SEEG Fogo 
## Desforestation of burned forests 
##
## Load spreadsheets from Google Earth, join, and plot
##
## Made by: Aline Pontes Lopes, 25/05/2022, edits 23/12/2022, 10/01/2023
## cleaned script, new GEE data from 06/01/2023
## last update: 18/03/2023
##
## APL: version called "SEEGFire_AM_Burnedforests_deforestation_analyses_20230418.R" in my PC
##
##_______________________________________________________________________________


library(openxlsx)
library(dplyr)
library(reshape2)
library(tidyr)
library(ggplot2)
library(scales)

library(raster)
library(ggsn)
library(ggthemes)
library(ggpmisc)

setwd('C:/Users/liepl/OneDrive - inpe.br/IPAM/Codes')
#setwd('C:/Users/Usuario/OneDrive - inpe.br/IPAM/Codes')


## Box a - Map of the accumulated burned area in 2020 --------------------------
# standing vs deforested

# path.maps = 'C:/Users/liepl/OneDrive - inpe.br/IPAM/Codes/Desm_freq_queima/Burnedforests_deforestation_20230109/mapa'
# 
# map.paths = list.files(path = path.maps, full.names = TRUE, recursive = TRUE, pattern = "*.tif")
# maps.list = lapply(map.paths, raster)
# #plot(maps.list[[1]])
# #maps.list[[1]]
# 
# names(maps.list) <- NULL
# maps.list$fun <- sum
# maps.list$na.rm <- TRUE
# map <- do.call(mosaic, maps.list) 
# plot(map)

# Add manually using inkscape!



# Box b - Burned and deforested x all deforested forests ------------------------------------

# APL, 18/04/2023: Esta parte do código ainda precisa ser atualizada assim que o problema no csv vazio for corrigido.


# burned and deforested, each-year deforestation area (Km2)
#file = 'C:/Users/liepl/OneDrive - inpe.br/IPAM/Codes/Desm_freq_queima/Burnedforests_deforestation/Burned_and_allForest_deforested.xlsx'
file = 'C:/Users/liepl/OneDrive - inpe.br/IPAM/Codes/Desm_freq_queima/Burnedforests_deforestation_20230418/burn_desf_area.csv'  # ARQUIVO VAZIO

df = read.csv(file, header = TRUE)
head(df)
df_bd = data.frame(year = substr(names(df),15,18)[2:32],
                  annual_bur_defor = t(df[1,2:32]))
names(df_bd) =c('year','annual_burn_defor')
row.names(df_bd) = NULL
df_bd$year = as.numeric(df_bd$year)

# cumulative data
df_bd = df_bd %>% mutate(cum_burn_defor = cumsum(annual_burn_defor))

# simple plot to check
# ggplot(df_bd, aes(x=year,y=annual_burn_defor))+ geom_path()
# ggplot(df_bd, aes(x=year,y=cum_burn_defor))+ geom_path()


# burned and standing, cumulative burned area (Km2)
file2 = 'C:/Users/liepl/OneDrive - inpe.br/IPAM/Codes/Desm_freq_queima/Burnedforests_deforestation_20230418/burn_std_for_area.csv'

df2 = read.csv(file2, header = TRUE)
head(df2)
df_bs = data.frame(year = as.numeric(substr(names(df2),16,19)[2:32]) + 1,        # corrigi o nome da banda aqui!
                   cum_bur_standing = t(df2[1,2:32]))
names(df_bs) =c('year','cum_burn_standing')
row.names(df_bs) = NULL

# simple plot to check
# ggplot(df_bs, aes(x=year,y=cum_burn_standing))+ geom_path()

rm(df, df2, file, file2)


## Box b - 2nd axis - Cumulated burned area x cumulated burned deforested area
# old box a

# stacked barplot

df = df_bd
df$cum_burn_standing = df_bs$cum_burn_standing

df_boxb = melt(df[,c(1,3,4)], id='year')
head(df_boxb)
names(df_boxb) = c('Year','Status','Area')
df_boxb$Year = as.factor(df_boxb$Year)

df_boxb$Status = factor(df_boxb$Status, levels = c("cum_burn_standing","cum_burn_defor"),
                         labels = c('burned and standing', 'burned and deforested'))


## Box b - 1st axis - Burned forests: % standing x % deforested
# old box c

df_boxb = df_boxb %>%
  group_by(Year) %>%
  mutate(area_perc = Area/sum(Area) * 100)

#df_boxb$Status = factor(df_boxb$Status, levels = rev(levels(df_boxb$Status)))

df_boxb = as.data.frame(df_boxb)

df_boxb$area_perc_scaled = df_boxb$area_perc*2500                            
df_boxb_line = subset(df_boxb, Status == "burned and standing")
df_boxb_line = droplevels(df_boxb_line)

cbPal <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

## Plotting Boxb

pb = ggplot() +
  geom_col(data = df_boxb, aes(x=Year, y=Area, fill=Status, alpha=Status)) +
  geom_line(data=df_boxb_line, aes(x=Year, y=area_perc_scaled, group = Status),
            col='#145CE1', linewidth=1.0, alpha=0.7) +
  geom_hline(yintercept = 0) +
  scale_y_continuous(expand = c(0,0), 
                     limits = c(0,250000),
                     breaks = c(0,50000,100000,150000,200000,250000),
                     labels = c(0,50000,100000,150000,200000,250000)/2500,
                     sec.axis = sec_axis(~ ./1000 , name = expression("Cumulated burned forest area (x1000 km"^{2}*")"))) +
  scale_fill_manual(values = c('#56B4E9','#CC79A7'), drop=T) + # rev(hue_pal()(2))) =
  scale_alpha_manual(values=c(0.6,0.8)) +
  theme_minimal() +
  theme(axis.title = element_text(size = 11, color = 'gray19'),
        axis.title.x = element_blank(),
        axis.title.y.left = element_text(size=11, color = '#145CE1'),
        axis.line = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text.y = element_text(size = 11, color = 'gray19'),
        axis.text.y.left = element_text(size = 11, color = '#145CE1'),
        axis.text.x = element_text(size = 8, color = 'gray19', angle=90,hjust=0.95,vjust=0.5),
        plot.title = element_text(size=13, face="bold", hjust = 0, color = 'gray19'),
        legend.position = 'top', legend.direction = "horizontal") +
  labs(x = 'Year', y = '% of cumulated burned standing forest', fill="", alpha="") + 
  annotate('text', x='1991', y=240000, label = "b", size=7, fontface='bold')

x11(); pb


## Box c - Fire frequency, standing vs. deforested ------------------------------------
# old box f

# APL, 18/04/2023: Esta parte já foi atualizada, mas as séries estão começando e terminando em anos distintos. Resolver.


file3 = 'C:/Users/liepl/OneDrive - inpe.br/IPAM/Codes/Desm_freq_queima/Burnedforests_deforestation_20230418/freq_desf.csv'

df3_desf_freq = read.csv(file3, header = TRUE)
head(df3_desf_freq)
df3_desf_freq = data.frame(status = 'burned and deforested',
                           year = df3_desf_freq$year,
                           freq_desf = df3_desf_freq$freq_desf,
                           area_km2 = df3_desf_freq$area_km2)

# # read all sheets at once and organize _____________________  
# 
# file.list1.freq = list();
# df.list2.freq = list(); # as from the GEE
# df.list3.freq = list(); # organized for ggplot
# 
# file.list1.freq <- list.files(path = path, full.names = TRUE, recursive = TRUE, pattern = "*.csv")
# df.list2.freq <- sapply(file.list1.freq, function(x) read.csv(file = x, header = TRUE), simplify = FALSE)
# df.list2.freq[[10]]
# 
# for (i in 1:length(df.list2.freq)) {
#   
#   temp = df.list2.freq[[i]]
#   
#   # organizing
#   temp$year = as.numeric(temp$year) 
#   temp = temp[,2:4]
#   temp$status = 'burned and deforested'
#   temp = temp[,4:1]
#   
#   df.list3.freq[[i]] = temp
#   
# }
# 
# #df.list2.freq[[10]]
# #df.list3.freq[[10]]
# 
# df.list3.freq = bind_rows(df.list3.freq)
# 
# df.list3.freq = subset(df.list3.freq, freq_desf>0) #

df3_desf_freq$freq_desf = as.factor(df3_desf_freq$freq_desf)
df3_desf_freq$year = as.factor(df3_desf_freq$year)

#rm(i, temp, path, file.list1.freq, df.list2.freq)
rm(file3)


# average +- sd __________________________

# without unburned areas (frequency > 0)

df3_desf_freq$freq_num = as.numeric(levels(df3_desf_freq$freq_desf))[df3_desf_freq$freq_desf]

#function for weighted var and sd
weighted.var <- function(x, w = NULL, na.rm = TRUE) {
  if (na.rm) {
    na <- is.na(x) | is.na(w)
    x <- x[!na]
    w <- w[!na]
  }
    sum(w * (x - weighted.mean(x, w)) ^ 2) / (sum(w) - 1)
}
weighted.sd <- function(x, w, na.rm = TRUE) sqrt(weighted.var(x, w, na.rm = TRUE))

#function for weighted mean (default not removing NAs)
weighted.mn <- function(x, w = NULL, na.rm=T) {
  if (na.rm) {
    na <- is.na(x) | is.na(w)
    x <- x[!na]
    w <- w[!na]
  }
  weighted.mean(x, w)
}

df3_desf_freq_summary = df3_desf_freq %>%
  group_by(status, year) %>%
  summarize(total_area = sum(area_km2,na.rm = T),
            wg_avg_freq = weighted.mn(x=freq_num, w=area_km2,na.rm=T),
            wg_sd_freq = weighted.sd(x=freq_num, w=area_km2))

df3_desf_freq_summary = as.data.frame(df3_desf_freq_summary)
df3_desf_freq_summary$year = as.numeric(levels(df3_desf_freq_summary$year))[df3_desf_freq_summary$year]
df3_desf_freq_summary$sd_up = df3_desf_freq_summary$wg_avg_freq + df3_desf_freq_summary$wg_sd_freq
df3_desf_freq_summary$sd_lo = df3_desf_freq_summary$wg_avg_freq - df3_desf_freq_summary$wg_sd_freq

# # plot
# pc_v1 = ggplot(df.list3.freq_summary) +
#   geom_ribbon(aes(x=year, ymin=sd_lo, ymax=sd_up), fill='#56B4E9', alpha=0.6) +
#   geom_line(aes(x=year,y=wg_avg_freq, group = 1)) +
#   geom_hline(yintercept = 0) +
#   scale_x_continuous(limits = c(1990,2020), breaks = seq(1990,2020,1),expand = c(0.01,0.01)) +
#   scale_y_continuous(expand = c(0,0), limits = c(0,4)) +
#   theme_minimal() +
#   theme(axis.title = element_text(size = 11, color = 'gray19'),
#         axis.line = element_blank(),
#         panel.grid.minor.y = element_blank(),
#         panel.grid.major.x = element_blank(),
#         panel.grid.minor.x = element_blank(),
#         axis.text.y = element_text(size = 11, color = 'gray19'),
#         axis.text.x = element_text(size = 8, color = 'gray19', angle=90,hjust=0.95,vjust=0.5),
#         plot.title = element_text(size=13, face="bold", hjust = 0, color = 'gray19'),
#         legend.position = 'none', legend.direction = "horizontal") +
#   labs(x = 'Year', y = "Fire frequency when deforested", fill="") +
#   annotate('text', x=1991, y=3.9, label = "c", size=7, fontface='bold')
# 
# x11(); pc_v1 # only burned and deforested   

# now adding fire frequency for standing forests __________________________

file4 = 'C:/Users/liepl/OneDrive - inpe.br/IPAM/Codes/Desm_freq_queima/Burnedforests_deforestation_20230418/freq_std.csv'

df4_std_freq = read.csv(file4, header = TRUE)
head(df4_std_freq)
df4_std_freq = data.frame(status = 'burned and standing',
                           year = df4_std_freq$year,
                          freq_std = df4_std_freq$freq_std,
                           area_km2 = df4_std_freq$area_km2)


# # read all sheets at once and organize _____________________  
# 
# file.list4.freq = list();
# df.list5.freq = list(); # as from the GEE
# df.list6.freq = list(); # organized for ggplot
# 
# file.list4.freq <- list.files(path = path2, full.names = TRUE, recursive = TRUE, pattern = "*.csv")
# df.list5.freq <- sapply(file.list4.freq, function(x) read.csv(file = x, header = TRUE), simplify = FALSE)
# df.list5.freq[[10]]
# 
# for (i in 1:length(df.list5.freq)) {
#   
#   temp = df.list5.freq[[i]]
#   
#   # organizing
#   temp$year = as.numeric(temp$year) 
#   temp = temp[,2:4]
#   temp$status = 'burned and standing'
#   temp = temp[,4:1]
#   
#   df.list6.freq[[i]] = temp
#   
# }
# 
# #df.list5.freq[[10]]
# #df.list6.freq[[10]]
# 
# df.list6.freq = bind_rows(df.list6.freq)
# df.list6.freq = subset(df.list6.freq, freq_std>0)

df4_std_freq$freq_std = as.factor(df4_std_freq$freq_std)
df4_std_freq$year = as.factor(df4_std_freq$year)

#rm(i, temp, path2, file.list4.freq, df.list5.freq)
rm(file4)


df4_std_freq$freq_num = as.numeric(levels(df4_std_freq$freq_std))[df4_std_freq$freq_std]

df4_std_freq_summary = df4_std_freq %>%
  group_by(status, year) %>%
  summarize(total_area = sum(area_km2,na.rm = T),
            wg_avg_freq = weighted.mn(x=freq_num, w=area_km2,na.rm=T),
            wg_sd_freq = weighted.sd(x=freq_num, w=area_km2))

df4_std_freq_summary = as.data.frame(df4_std_freq_summary)
df4_std_freq_summary$year = as.numeric(levels(df4_std_freq_summary$year))[df4_std_freq_summary$year]
df4_std_freq_summary$sd_up = df4_std_freq_summary$wg_avg_freq + df4_std_freq_summary$wg_sd_freq
df4_std_freq_summary$sd_lo = df4_std_freq_summary$wg_avg_freq - df4_std_freq_summary$wg_sd_freq


# joining both dataframes

df.freq_bd_bs_summary = rbind(df3_desf_freq_summary, df4_std_freq_summary)
df.freq_bd_bs_summary$status = as.factor(df.freq_bd_bs_summary$status)

df.freq_bd_bs_summary$status = factor(df.freq_bd_bs_summary$status, 
                                      levels = c('burned and standing', 'burned and deforested'))

# plot
pc_v2 = ggplot(df.freq_bd_bs_summary, aes(x=year,y=wg_avg_freq, group=status, fill=status)) +
  geom_ribbon(aes(ymin=sd_lo, ymax=sd_up), alpha=0.5) +
  geom_line(aes(col=status), linewidth=0.7) +
  geom_hline(yintercept = 0) +
  scale_x_continuous(limits = c(1990,2023), breaks = seq(1990,2023,1),expand = c(0.01,0.01)) +
  scale_y_continuous(expand = c(0,0), limits = c(0,4)) +
  scale_fill_manual(values = c('#56B4E9','#CC79A7')) +
  scale_color_manual(values = c('#145CE1','#D56082')) +
  theme_minimal() +
  theme(axis.title = element_text(size = 11, color = 'gray19'),
        axis.line = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text.y = element_text(size = 11, color = 'gray19'),
        axis.text.x = element_text(size = 8, color = 'gray19', angle=90,hjust=0.95,vjust=0.5),
        plot.title = element_text(size=13, face="bold", hjust = 0, color = 'gray19'),
        legend.position = 'top', legend.direction = "horizontal") +
  labs(x = 'Year', y = "Fire frequency", fill="", color="") +
  annotate('text', x=1991, y=3.9, label = "c", size=7, fontface='bold')

x11(); pc_v2 # now both 'burned and deforested' & 'burned and standing'

## APL, 18/04/2023: As série estão começando e terminando em anos distintos. Precisamos verificar isso. 



# ## Box c - Percentiles ------------------------------------------------------------------
# 
# path = 'C:/Users/liepl/OneDrive - inpe.br/IPAM/Codes/Desm_freq_queima/Burnedforests_deforestation_20230109/freq_desf_yyyy'
# 
# 
# # read all sheets at once and organize _____________________
# 
# file.list1.freq = list();
# df.list2.freq = list(); # as from the GEE
# df.list3.freq = list(); # organized for ggplot
# 
# file.list1.freq <- list.files(path = path, full.names = TRUE, recursive = TRUE, pattern = "*.csv")
# df.list2.freq <- sapply(file.list1.freq, function(x) read.csv(file = x, header = TRUE), simplify = FALSE)
# df.list2.freq[[10]]
# 
# for (i in 1:length(df.list2.freq)) {
# 
#   temp = df.list2.freq[[i]]
# 
#   # organizing
#   temp$year = as.numeric(temp$year)
#   temp = temp[,2:4]
#   temp$status = 'burned and deforested'
#   temp = temp[,4:1]
# 
#   df.list2.freq[[i]] = temp
# 
#   # percentiles
#   temp2 = subset(temp, freq_desf > 0)
#   temp2$area_km2_scl = as.integer(round(temp2$area_km2 *10^5,0))
#   clean_data <- temp2 %>%
#     uncount(weights = area_km2_scl)
# 
#   temp3 <- quantile(clean_data$freq_desf, probs = c(0.25, 0.5, 0.75))
# 
#   temp3 = data.frame(status = unique(temp2$status),
#                      year = unique(temp2$year),
#                      percentile = names(temp3),
#                      values = temp3)
#   rownames(temp3) = NULL
# 
#   df.list3.freq[[i]] = temp3
# 
#   print (paste0(i," done" ))
# 
#   rm(temp, temp2, temp3, clean_data)
#   gc()
# 
# }
# 
# #df.list2.freq[[10]]
# #df.list3.freq[[10]]
# 
# df.list3.freq = bind_rows(df.list3.freq)
# df.list3.freq$percentile = as.factor(df.list3.freq$percentile)
# df.list3.freq$year = as.factor(df.list3.freq$year)
# 
# rm(i, path, file.list1.freq, df.list2.freq)
# 
# 
# # now adding fire frequency for standing forests __________________________
# 
# path2 = 'C:/Users/liepl/OneDrive - inpe.br/IPAM/Codes/Desm_freq_queima/Burnedforests_deforestation_20230109/freq_std_for_area_yyyy'
# 
# 
# # read all sheets at once and organize _____________________
# 
# file.list4.freq = list();
# df.list5.freq = list(); # as from the GEE
# df.list6.freq = list(); # organized for ggplot
# 
# file.list4.freq <- list.files(path = path2, full.names = TRUE, recursive = TRUE, pattern = "*.csv")
# df.list5.freq <- sapply(file.list4.freq, function(x) read.csv(file = x, header = TRUE), simplify = FALSE)
# df.list5.freq[[10]]
# 
# for (i in 1:length(df.list5.freq)) {
# 
#   temp = df.list5.freq[[i]]
# 
#   # organizing
#   temp$year = as.numeric(temp$year)
#   temp = temp[,2:4]
#   temp$status = 'burned and standing'
#   temp = temp[,4:1]
# 
#   df.list5.freq[[i]] = temp
# 
#   # percentiles
#   temp2 = subset(temp, freq_std > 0)
#   temp2$area_km2_scl = as.integer(round(temp2$area_km2*10^2,0))
#   clean_data <- temp2 %>%
#     uncount(weights = area_km2_scl)
# 
#   temp3 <- quantile(clean_data$freq_std, probs = c(0.25, 0.5, 0.75))
# 
#   temp3 = data.frame(status = unique(temp2$status),
#                      year = unique(temp2$year),
#                      percentile = names(temp3),
#                      values = temp3)
#   rownames(temp3) = NULL
# 
#   df.list6.freq[[i]] = temp3
# 
#   print (paste0(i," done" ))
# 
#   rm(temp, temp2, temp3, clean_data)
#   gc()
# 
# }
# 
# #df.list5.freq[[10]]
# #df.list6.freq[[10]]
# 
# df.list6.freq = bind_rows(df.list6.freq)
# 
# df.list6.freq$percentile = as.factor(df.list6.freq$percentile)
# df.list6.freq$year = as.factor(df.list6.freq$year)
# 
# rm(i, path2, file.list4.freq, df.list5.freq)
# 
# 
# # joining both dataframes
# df.freq_bd_bs_perc = rbind(df.list3.freq, df.list6.freq)
# df.freq_bd_bs_perc$status = as.factor(df.freq_bd_bs_perc$status)
# 
# # percentiles as columns
# df.freq_bd_bs_perc2 = df.freq_bd_bs_perc %>%
#                         pivot_wider(names_from = percentile,
#                                     values_from = values)
# names(df.freq_bd_bs_perc2)[3:5] = c("perc25","perc50","perc75")
# df.freq_bd_bs_perc2$year = as.numeric(df.freq_bd_bs_perc2$year) + 1989
# 
# df.freq_bd_bs_perc2$status = factor(df.freq_bd_bs_perc2$status,
#                                       levels = c('burned and standing', 'burned and deforested'))
# 
# # subset(df.freq_bd_bs_perc2, perc50)
# # !n?o ta rolando porque o perc50 ? =1 para todas as classes e anos!
# 
# # plot
# pc_v3 = ggplot(df.freq_bd_bs_perc2, aes(x=year,y=perc50, group=status, fill=status)) +
#   geom_ribbon(aes(ymin=perc25, ymax=perc75), alpha=0.5) +
#   geom_line(aes(col=status), linewidth=0.7) +
#   geom_hline(yintercept = 0) +
#   scale_x_continuous(limits = c(1990,2020), breaks = seq(1990,2020,1),expand = c(0.01,0.01)) +
#   scale_y_continuous(expand = c(0,0), limits = c(0,3)) +
#   scale_fill_manual(values = c('#56B4E9','#CC79A7')) +
#   scale_color_manual(values = c('#145CE1','#D56082')) +
#   theme_minimal() + 
#   facet_wrap(.~status, ncol = 1) +
#   theme(axis.title = element_text(size = 11, color = 'gray19'),
#         axis.line = element_blank(),
#         panel.grid.minor.y = element_blank(),
#         panel.grid.major.x = element_blank(),
#         panel.grid.minor.x = element_blank(),
#         axis.text.y = element_text(size = 11, color = 'gray19'),
#         axis.text.x = element_text(size = 8, color = 'gray19', angle=90,hjust=0.95,vjust=0.5),
#         plot.title = element_text(size=13, face="bold", hjust = 0, color = 'gray19'),
#         strip.text.x = element_blank(),
#         legend.position = c(0.5,0.95), legend.direction = "horizontal") +
#   labs(x = 'Year', y = "Fire frequency", fill="", color="") #+
#   #annotate('text', x=1991, y=2.9, label = "c", size=7, fontface='bold')
# 
# x11(); pc_v3 # now both 'burned and deforested' & 'burned and standing'
# 



## Box d - Year since the last fire when deforested ---------------------------------------
# old box d

setwd('C:/Users/liepl/OneDrive - inpe.br/IPAM/Codes')

#path = 'C:/Users/liepl/OneDrive - inpe.br/IPAM/Codes/Desm_freq_queima/Burnedforests_deforestation/ysf_histogram_v2'
file5 = 'C:/Users/liepl/OneDrive - inpe.br/IPAM/Codes/Desm_freq_queima/Burnedforests_deforestation_20230418/ysf_desf.csv'

df5_desf_ysf = read.csv(file5, header = TRUE)
head(df5_desf_ysf)
df5_desf_ysf = data.frame(status = 'burned and deforested',
                          year = df5_desf_ysf$year,
                          ysf_desf = df5_desf_ysf$ysf_desf,
                          area_km2 = df5_desf_ysf$area_km2)

# # read all sheets at once and organize __________________________________  
# 
# file.list1.ysf = list();
# df.list2.ysf = list(); # as from the GEE
# df.list3.ysf = list(); # organized for ggplot
# 
# file.list1.ysf <- list.files(path = path3, full.names = TRUE, recursive = TRUE, pattern = "*.csv")
# df.list2.ysf <- sapply(file.list1.ysf, function(x) read.csv(file = x, header = TRUE), simplify = FALSE)
# df.list2.ysf[[10]]
# 
# for (i in 1:length(df.list2.ysf)) {
#   
#   temp = df.list2.ysf[[i]]
#   
#   # organizing
#   temp$year = as.numeric(temp$year) 
#   temp = temp[,2:4]
#   temp$status = 'burned and deforested'
#   temp = temp[,4:1]
#   
#   df.list3.ysf[[i]] = temp
#   
# }
# 
# #df.list2.ysf[[10]]
# #df.list3.ysf[[10]]
# 
# df.list3.ysf = bind_rows(df.list3.ysf)
# 
# df.list3.ysf = subset(df.list3.ysf, ysf_desf>0) # no zero ysf

df5_desf_ysf$ysf_desf = as.factor(df5_desf_ysf$ysf_desf)
df5_desf_ysf$year = as.factor(df5_desf_ysf$year)

# rm(i, temp, path3, file.list1.ysf, df.list2.ysf)
rm(file5)


# average +- sd __________________________

df5_desf_ysf$ysf_num = as.numeric(levels(df5_desf_ysf$ysf_desf))[df5_desf_ysf$ysf_desf]

df5_desf_ysf_summary = df5_desf_ysf %>%
  group_by(status, year) %>%
  summarize(total_area = sum(area_km2,na.rm = T),
            wg_avg_ysf = weighted.mn(x=ysf_num, w=area_km2,na.rm=T),
            wg_sd_ysf = weighted.sd(x=ysf_num, w=area_km2))

df5_desf_ysf_summary = as.data.frame(df5_desf_ysf_summary)
df5_desf_ysf_summary$year = as.numeric(levels(df5_desf_ysf_summary$year))[df5_desf_ysf_summary$year]
df5_desf_ysf_summary$sd_up = df5_desf_ysf_summary$wg_avg_ysf + df5_desf_ysf_summary$wg_sd_ysf
df5_desf_ysf_summary$sd_lo = df5_desf_ysf_summary$wg_avg_ysf - df5_desf_ysf_summary$wg_sd_ysf
df5_desf_ysf_summary$sd_lo = ifelse(df5_desf_ysf_summary$sd_lo < 0, 0, df5_desf_ysf_summary$sd_lo)

# # plot
# pd_v1 = ggplot(df.list3.ysf_summary) +
#   geom_ribbon(aes(x=year, ymin=sd_lo, ymax=sd_up), fill='#56B4E9', alpha=0.6) +
#   geom_line(aes(x=year,y=wg_avg_ysf, group = 1)) +
#   geom_hline(yintercept = 0) +
#   scale_x_continuous(limits = c(1990,2020), breaks = seq(1990,2020,1),expand = c(0.01,0.01)) +
#   scale_y_continuous(expand = c(0,0), limits = c(0,15)) +
#   theme_minimal() +
#   theme(axis.title = element_text(size = 11, color = 'gray19'),
#         axis.line = element_blank(),
#         panel.grid.minor.y = element_blank(),
#         panel.grid.major.x = element_blank(),
#         panel.grid.minor.x = element_blank(),
#         axis.text.y = element_text(size = 11, color = 'gray19'),
#         axis.text.x = element_text(size = 8, color = 'gray19', angle=90,hjust=0.95,vjust=0.5),
#         plot.title = element_text(size=13, face="bold", hjust = 0, color = 'gray19'),
#         legend.position = 'none', legend.direction = "horizontal") +
#   labs(x = 'Year', y = "Time from fire to deforestation (years)", fill="") +
#   annotate('text', x=1991, y=14.5, label = "d", size=7, fontface='bold')
# 
# x11(); pd_v1 # only burned and deforested  


# now adding ysf for standing forests __________________________

#path4 = 'C:/Users/liepl/OneDrive - inpe.br/IPAM/Codes/Desm_freq_queima/Burnedforests_deforestation_20230109/ysf_std_for_area_yyyy'
file6 = 'C:/Users/liepl/OneDrive - inpe.br/IPAM/Codes/Desm_freq_queima/Burnedforests_deforestation_20230418/ysf_std_for_area.csv'

df6_std_ysf = read.csv(file6, header = TRUE)
head(df6_std_ysf)
df6_std_ysf = data.frame(status = 'burned and standing',
                          year = df6_std_ysf$year,
                          ysf_std = df6_std_ysf$ysf_std_for_area,
                          area_km2 = df6_std_ysf$area_km2)

# # read all sheets at once and organize _____________________  
# 
# file.list4.ysf = list();
# df.list5.ysf = list(); # as from the GEE
# df.list6.ysf = list(); # organized for ggplot
# 
# file.list4.ysf <- list.files(path = path4, full.names = TRUE, recursive = TRUE, pattern = "*.csv")
# df.list5.ysf <- sapply(file.list4.ysf, function(x) read.csv(file = x, header = TRUE), simplify = FALSE)
# df.list5.ysf[[10]]
# 
# for (i in 1:length(df.list5.ysf)) {
#   
#   temp = df.list5.ysf[[i]]
#   
#   # organizing
#   temp$year = as.numeric(temp$year) 
#   temp = temp[,2:4]
#   temp$status = 'burned and standing'
#   temp = temp[,4:1]
#   
#   df.list6.ysf[[i]] = temp
#   
# }
# 
# #df.list5.ysf[[10]]
# #df.list6.ysf[[10]]
# 
# df.list6.ysf = bind_rows(df.list6.ysf)

df6_std_ysf = subset(df6_std_ysf, ysf_std>0)
df6_std_ysf$ysf_std = as.factor(df6_std_ysf$ysf_std)
df6_std_ysf$year = as.factor(df6_std_ysf$year)

#rm(i, temp, path4, file.list4.ysf, df.list5.ysf)
rm(file6)

#summary 

df6_std_ysf$ysf_num = as.numeric(levels(df6_std_ysf$ysf_std))[df6_std_ysf$ysf_std]

df6_std_ysf_summary = df6_std_ysf %>%
  group_by(status, year) %>%
  summarize(total_area = sum(area_km2,na.rm = T),
            wg_avg_ysf = weighted.mn(x=ysf_num, w=area_km2,na.rm=T),
            wg_sd_ysf = weighted.sd(x=ysf_num, w=area_km2))

df6_std_ysf_summary = as.data.frame(df6_std_ysf_summary)
df6_std_ysf_summary$year = as.numeric(levels(df6_std_ysf_summary$year))[df6_std_ysf_summary$year]
df6_std_ysf_summary$sd_up = df6_std_ysf_summary$wg_avg_ysf + df6_std_ysf_summary$wg_sd_ysf
df6_std_ysf_summary$sd_lo = df6_std_ysf_summary$wg_avg_ysf - df6_std_ysf_summary$wg_sd_ysf
df6_std_ysf_summary$sd_lo = ifelse(df6_std_ysf_summary$sd_lo < 0, 0, df6_std_ysf_summary$sd_lo)

# joining both dataframes

df.ysf_bd_bs_summary = rbind(df5_desf_ysf_summary, df6_std_ysf_summary)
df.ysf_bd_bs_summary$status = as.factor(df.ysf_bd_bs_summary$status)

df.ysf_bd_bs_summary$status = factor(df.ysf_bd_bs_summary$status, 
                                      levels = c('burned and standing', 'burned and deforested'))

# plot
pd_v2 = ggplot(df.ysf_bd_bs_summary, aes(x=year,y=wg_avg_ysf, group=status, fill=status)) +
  geom_ribbon(aes(ymin=sd_lo, ymax=sd_up), alpha=0.5) +
  geom_line(aes(col=status), linewidth=0.7) +
  geom_hline(yintercept = 0) +
  scale_x_continuous(limits = c(1990,2023), breaks = seq(1990,2023,1),expand = c(0.01,0.01)) +
  scale_y_continuous(expand = c(0,0), limits = c(0,22)) +
  scale_fill_manual(values = c('#56B4E9','#CC79A7')) +
  scale_color_manual(values = c('#145CE1','#D56082')) +
  theme_minimal() +
  theme(axis.title = element_text(size = 11, color = 'gray19'),
        axis.line = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text.y = element_text(size = 11, color = 'gray19'),
        axis.text.x = element_text(size = 8, color = 'gray19', angle=90,hjust=0.95,vjust=0.5),
        plot.title = element_text(size=13, face="bold", hjust = 0, color = 'gray19'),
        legend.position = 'top', legend.direction = "horizontal") +
  labs(x = 'Year', y = "Time since last fire (years)", fill="", color="") +
  annotate('text', x=1991, y=21, label = "d", size=7, fontface='bold')

x11(); pd_v2 # now both 'burned and deforested' & 'burned and standing'


## FINAL PLOT ---------------------------------------------------------


# APL, 18/04/2023: Ainda não plotei porque precisamos arrumar inconsistências nos dados.

library(egg)
library(svglite)

null.df = data.frame(year = rep(seq(1990,2020),2),
                     data = rep(rep(-1,31),2),
                     status = c(rep('burned and standing',31),rep('burned and deforested',31)))

null.df$status = as.factor(null.df$status)
null.df$status = factor(null.df$status,
                        levels = c('burned and standing','burned and deforested'))
null.df$year = as.factor(null.df$year)

#colors <- c('burned and standing' = '#145CE1', 'burned and deforested' = '#D56082')
colors <- c('burned and standing' = '#0000ff', 'burned and deforested' = '#ff0000')

pa = ggplot(null.df) +
  geom_col(aes(x=year,y=data,fill=status)) +
  scale_y_continuous(lim=c(-2,250),exp=c(0,0)) +
  scale_fill_manual(values=colors) +
  theme_void() +
  theme(legend.position = 'top',legend.direction = 'horizontal') +
  labs(fill='')+
  annotate('text', x='1991', y=240, label = "a", size=7, fontface='bold') +
  coord_cartesian(ylim=c(1,250))

x11(); pa

# # without standing forests
# png("./figures/Manuscript_Figure_Burned_Deforestation_20230113.png",
#     width = 25, height = 20, units = 'cm', res = 400)
# ggarrange(pa, pb, pc_v1, pd_v1, ncol = 2, nrow = 2, heights = c(2,2))
# dev.off()

# with standing forests
# as .png
png("./figures/Manuscript_Figure_Burned_Deforestation_20230418_full.png",
    width = 25, height = 19.5, units = 'cm', res = 400)
ggarrange(pa, pb, pc_v2, pd_v2, ncol = 2, nrow = 2, heights = c(2,2))
dev.off()

# as .svg
img = ggarrange(pa, pb, pc_v2, pd_v2, ncol = 2, nrow = 2, heights = c(2,2))
ggsave(file="./figures/Manuscript_Figure_Burned_Deforestation_20230418_full.svg", 
       plot=img, width = 25, height = 19.5, units = 'cm', dpi = 400)

