
## ============================================================================
## FATE-SEEG Fire manuscript
## Figure 1 – Deforestation of Burned Forests
##
## Description:
## Processes Google Earth Engine outputs to quantify and analyze the
## deforestation of previously burned forests in the Brazilian Amazon.
## Generates datasets and visualizations used in Figure 1.
##
## Inputs:
## - Annual burned and deforested forest area
## - Burn frequency datasets
## - Time-since-last-fire datasets
##
## Outputs:
## - Figure 1 (main text)
## - Source data tables used in the figure
##
## Author:
## Aline Pontes Lopes
##
## Public repository version.
## ============================================================================


library(openxlsx)
library(dplyr)
library(reshape2)
library(tidyr)
library(ggplot2)
library(scales)
library(egg)
library(svglite)


## Box a - Map of the accumulated burned area in 2020 --------------------------
# standing vs deforested

# Add manually using inkscape!



## Box b - Burned and deforested x all deforested forests ----------------------

# burned and deforested, each-year deforestation area (Km2) ----
path = '/data/Desf_Fire_freq/burn_desf_area-YYYY'

# read all sheets at once and organize 

file.list = list();
df.list = list();

file.list <- list.files(path = path, full.names = TRUE, recursive = TRUE, pattern = "*.csv")
df.list <- sapply(file.list, function(x) read.csv(file = x, header = TRUE), simplify = FALSE)
df_bd = bind_rows(df.list)
rm(path, df.list, file.list)

df_bd = df_bd[,c(4,2)]
names(df_bd)[2] = 'annual_burn_defor'
df_bd$year = as.numeric(df_bd$year)

# cumulative data
df_bd = df_bd %>% mutate(cum_burn_defor = cumsum(annual_burn_defor))


# burned and standing, cumulative burned area (Km2)
path = '/data/Desf_Fire_freq/burn_std_for_area-YYYY'

# read all sheets at once and organize 

file.list = list();
df.list = list();

file.list <- list.files(path = path, full.names = TRUE, recursive = TRUE, pattern = "*.csv")
df.list <- sapply(file.list, function(x) read.csv(file = x, header = TRUE), simplify = FALSE)
df_bs = bind_rows(df.list)
rm(path, df.list, file.list)

df_bs = df_bs[,c(4,2)]
names(df_bs)[2] = 'cum_burn_standing'   # It is already standing.
df_bs$year = as.numeric(df_bs$year)



## Box b - 2nd axis - Cumulated burned area x cumulated burned deforested area

# stacked barplot

df = df_bd
df$cum_burn_standing = df_bs$cum_burn_standing

df_boxb = melt(df[,c(1,3,4)], id='year')
names(df_boxb) = c('Year','Status','Area')
df_boxb$Year = as.factor(df_boxb$Year)

df_boxb$Status = factor(df_boxb$Status, levels = c("cum_burn_standing","cum_burn_defor"),
                         labels = c('burned and remain standing', 'burned and later deforested'))
rm(df)


## -- Box b - 1st axis - Burned forests: % standing x % deforested

df_boxb = df_boxb %>%
  group_by(Year) %>%
  mutate(area_perc = Area/sum(Area) * 100)

df_boxb = as.data.frame(df_boxb)

df_boxb$area_perc_scaled = df_boxb$area_perc*4000                            
df_boxb_line = subset(df_boxb, Status == "burned and remain standing")
df_boxb_line = droplevels(df_boxb_line)


## Plotting Box b

pb = ggplot() +
  geom_col(data = df_boxb, aes(x=Year, y=Area, fill=Status, alpha=Status)) +
  geom_line(data=df_boxb_line, aes(x=Year, y=area_perc_scaled, group = Status),
            col='#145CE1', linewidth=1.0, alpha=0.7) +
  geom_hline(yintercept = 0) +
  scale_y_continuous(expand = c(0,0), 
                     limits = c(0,400000),
                     breaks = c(0,100000,200000,300000,400000),
                     labels = c(0,100000,200000,300000,400000)/4000,
                     sec.axis = sec_axis(~ ./1000 , name = expression("Cumulated burned forest area (x1000 km"^{2}*")"))) +
  scale_fill_manual(values = c('#56B4E9','#CC79A7'), drop=T) +
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
  annotate('text', x='1991', y=386000, label = "(b)", size=5)

pb

# exporting tables
write.xlsx(df_boxb_line, "/results/Tables/Figure1_boxb_axis1_data.xlsx")
write.xlsx(df_boxb, "/results/Tables/Figure1_boxb_axis2_data.xlsx")


# Manuscript statistics ----

# total forest area, burned at least once
sum(subset(df_boxb, Year == '2021')$Area) # ~336700 km2

# percentage standing
subset(df_boxb, Year == '2021' & Status == 'burned and remain standing')$area_perc #55.8%

# range of percentage standing
subset(df_boxb, Status == 'burned and remain standing')$area_perc #55.8%
range(subset(df_boxb, Status == 'burned and remain standing')[(length(levels(df_boxb$Year))-20):32,]$area_perc)

# area standing
subset(df_boxb, Year == '2021' & Status == 'burned and remain standing')$Area #187900 km2


## Box c - Fire frequency, standing vs. deforested -----------------------------

# burn frequency when deforested ----
path = '/data/Desf_Fire_freq/freq_desf-YYYY'

# read all sheets at once and organize 

file.list = list();
df.list = list();

file.list <- list.files(path = path, full.names = TRUE, recursive = TRUE, pattern = "*.csv")
df.list <- sapply(file.list, function(x) read.csv(file = x, header = TRUE), simplify = FALSE)
df3_desf_freq = bind_rows(df.list)
rm(path, df.list, file.list)

df3_desf_freq$status = 'burned and later deforested'
df3_desf_freq = df3_desf_freq[,c(6,4,3,2)]
df3_desf_freq$freq_desf = as.factor(df3_desf_freq$freq_desf)
df3_desf_freq$year = as.factor(df3_desf_freq$year)


# average +- sd ----

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


# now adding fire frequency for standing forests ----    

path = '/data/Desf_Fire_freq/freq_std-YYYY'

# read all sheets at once and organize 

file.list = list();
df.list = list();

file.list <- list.files(path = path, full.names = TRUE, recursive = TRUE, pattern = "*.csv")
df.list <- sapply(file.list, function(x) read.csv(file = x, header = TRUE), simplify = FALSE)
df4_std_freq = bind_rows(df.list)
rm(path, df.list, file.list)

df4_std_freq$status = 'burned and remain standing'
df4_std_freq = df4_std_freq[,c(6,4,3,2)]
df4_std_freq$freq_std = as.factor(df4_std_freq$freq_std)
df4_std_freq$year = as.factor(df4_std_freq$year)
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
                                      levels = c('burned and remain standing', 'burned and later deforested'))

# plot
pc_v2 = ggplot(df.freq_bd_bs_summary, aes(x=year,y=wg_avg_freq, group=status, fill=status)) +
  geom_ribbon(aes(ymin=sd_lo, ymax=sd_up), alpha=0.5) +
  geom_line(aes(col=status), linewidth=0.7) +
  geom_hline(yintercept = 0) +
  scale_x_continuous(limits = c(1990,2021), breaks = seq(1990,2021,1),expand = c(0.01,0.01)) +
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
  annotate('text', x=1991, y=3.9, label = "(c)", size=5)

pc_v2 # now both 'burned and later deforested' & 'burned and remain standing'

# exporting table
write.xlsx(df.freq_bd_bs_summary, "/results/Tables/Figure1_boxc_data.xlsx")



## Box d - Year since the last fire when deforested ----------------------------

path = '/data/Desf_Fire_freq/ysf_desf-YYYY'

# read all sheets at once and organize 

file.list = list();
df.list = list();

file.list <- list.files(path = path, full.names = TRUE, recursive = TRUE, pattern = "*.csv")
df.list <- sapply(file.list, function(x) read.csv(file = x, header = TRUE), simplify = FALSE)
df5_desf_ysf = bind_rows(df.list)
rm(path, df.list, file.list)

df5_desf_ysf$status = 'burned and later deforested'
df5_desf_ysf = df5_desf_ysf[,c(6,3,4,2)]
df5_desf_ysf$ysf_desf = as.factor(df5_desf_ysf$ysf_desf)
df5_desf_ysf$year = as.factor(df5_desf_ysf$year)

# average +- sd ------

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


# now adding ysf for standing forests ----

path = '/data/Desf_Fire_freq/ysf_std_for_area-YYYY'

# read all sheets at once and organize 

file.list = list();
df.list = list();

file.list <- list.files(path = path, full.names = TRUE, recursive = TRUE, pattern = "*.csv")
df.list <- sapply(file.list, function(x) read.csv(file = x, header = TRUE), simplify = FALSE)
df6_std_ysf = bind_rows(df.list)
rm(path, df.list, file.list)

df6_std_ysf$status = 'burned and remain standing'
df6_std_ysf = df6_std_ysf[,c(6,3,4,2)]
names(df6_std_ysf)[3] = 'ysf_std'

#df6_std_ysf = subset(df6_std_ysf, ysf_std>0) 
# We must include zero ysf for standing. Zero was excluded for deforested areas when we removed 
# deforestation fires -- which we assumed as the fire occuring in the deforestation year (ysf=0).

df6_std_ysf$ysf_std = as.factor(df6_std_ysf$ysf_std)
df6_std_ysf$year = as.factor(df6_std_ysf$year)
df6_std_ysf$ysf_num = as.numeric(levels(df6_std_ysf$ysf_std))[df6_std_ysf$ysf_std]

#summary 
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
                                      levels = c('burned and remain standing', 'burned and later deforested'))

# plot
pd_v2 = ggplot(df.ysf_bd_bs_summary, aes(x=year,y=wg_avg_ysf, group=status, fill=status)) +
  geom_ribbon(aes(ymin=sd_lo, ymax=sd_up), alpha=0.5) +
  geom_line(aes(col=status), linewidth=0.7) +
  geom_hline(yintercept = 0) +
  scale_x_continuous(limits = c(1990,2021), breaks = seq(1990,2021,1),expand = c(0.01,0.01)) +
  scale_y_continuous(expand = c(0,0), limits = c(0,21.5)) +
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
  annotate('text', x=1991, y=20.8, label = "(d)", size=5)

pd_v2 # now both 'burned and later deforested' & 'burned and remain standing'


# exporting table

write.xlsx(df.ysf_bd_bs_summary, "/results/Tables/Figure1_boxd_data.xlsx")


## queries for the text

# ysf for standing forests in 2021
subset(df.ysf_bd_bs_summary, 
       year == '2021' & status == 'burned and remain standing')$wg_avg_ysf # ~12 years


## FINAL PLOT - Main text - Figure 1 -------------------------------------------

null.df = data.frame(year = rep(seq(1990,2021),2),
                     data = rep(rep(-1,32),2),
                     status = c(rep('burned and remain standing',32),
                                rep('burned and later deforested',32)))

null.df$status = as.factor(null.df$status)
null.df$status = factor(null.df$status,
                        levels = c('burned and remain standing','burned and later deforested'))
null.df$year = as.factor(null.df$year)

colors <- c('burned and remain standing' = '#0000ff', 'burned and later deforested' = '#ff0000')

pa = ggplot(null.df) +
  geom_col(aes(x=year,y=data,fill=status)) +
  scale_y_continuous(lim=c(-2,250),exp=c(0,0)) +
  scale_fill_manual(values=colors) +
  theme_void() +
  theme(legend.position = 'top',legend.direction = 'horizontal') +
  labs(fill='')+
  annotate('text', x='1991', y=240, label = "(a)", size=5) +
  coord_cartesian(ylim=c(1,250))

pa

# as .png
png("/results/Figures/Figure1.png",
    width = 25, height = 19.5, units = 'cm', res = 400)
ggarrange(pa, pb, pc_v2, pd_v2, ncol = 2, nrow = 2, heights = c(2,2))
dev.off()

# # as .svg
# img = ggarrange(pa, pb, pc_v2, pd_v2, ncol = 2, nrow = 2, heights = c(2,2))
# ggsave(file="./figures/Figure1.svg", 
#        plot=img, width = 25, height = 19.5, units = 'cm', dpi = 400)

