
##_____________________________________________________________________________
## SEEG Fire
## GEE emissions from forest fires not related to deforestation
## Integrated model for the final carbon balance from combustion and mortality/decomposition
## for forests only
##
## Sensitivity analysis - Final figures
##
## R version 4.1.2
##
## Made by: Aline Pontes Lopes, 25/05/2022
##
## Called "SEEGFire_SA_5_Final_MainFigure_20220525.R" in my PC
##
##_______________________________________________________________________________


library(deSolve) #supports numerical integration using a range of numerical methods
library(ggplot2) #supports visualization of layered graphics
library(magrittr) #supports pipe function
library(grid) #supports additional graphing capability
library(egg) #supports visualization of multiple plots & graphs
library(dplyr)

options(scipen = 999)


## Sensitivity analysis _____________________________________________________________

# these are the default parameters and stocks from the multi burn script
# initial stocks were standardized to the biome averages (see table in the main manuscript)

fire.years = c(1986) # when the fire events happened                        <--- changed


## --- cwd combustion factor ________________________________________________________

scenarios_combf_cwd <- function(combf.cwd.mean, combf.cwd.sd, n, stocks, simtime, model.multi, auxs, auxs.multiB) {
  vec <- combf.cwd.mean + combf.cwd.sd*scale(rnorm(n)) # generate a normal distribution sample based on a given mean and s.d.
  #vec <- seq(TurnOver, by = step, length.out = n)
  base <- data.frame()
  for (i in 1:length(vec)) {
    auxs["aREF.combF.cwd.sF"] <- vec[i]
    temp <- model.multi(simtime, first.last.years, fire.years, stocks, auxs, auxs.multiB)
    temp$combF.cwd <- vec[i]
    temp$combF.fwd <- auxs$aREF.combF.fwd.sF
    temp$combF.cwd.text <- paste0("cwd combF = ",100*round(vec[i],3),"%")
    temp$combF.fwd.text <- paste0("fwd combF = ",100*auxs$aREF.combF.fwd.sF,"%")
    base <- rbind(base,temp)
  }
  return(base)
}

scen_combf_cwd <- scenarios_combf_cwd(combf.cwd.mean=0.643, combf.cwd.sd=0.0173, n=1000, # aqui
                                      stocks, simtime, model.multi, auxs, auxs.multiB) 
head(scen_combf_cwd)
mean(scen_combf_cwd$combF.cwd)  ## 0.643
sd(scen_combf_cwd$combF.cwd)    ## 0.0173


## Timeline comparison ------------------------------------------------------

#organizing
summary(scen_combf_cwd$combF.fwd)
#scen_combf_fwd = subset(scen_combf_fwd,Decomp.rate>=0)
summary(scen_combf_cwd$combF.cwd)
#scen_combf_fwd$time = as.factor(scen_combf_fwd$time)

#scen_TurnOver_Decomp = subset(scen_TurnOver_Decomp, TOver.rate > 0.02 & TOver.rate < 0.04)

# --- plotting - Carnival
#scen_TurnOver_Decomp = subset(scen_TurnOver_Decomp, TOver.rate > 0.02 & TOver.rate < 0.04)

# scen_combf_fwd_timeplot = 
#   ggplot(scen_combf_fwd, 
#          aes(x = time, y = CBalance, color = combF.fwd)) +
#   geom_line(size = 1) +
#   geom_hline(yintercept = 0, linetype = "dashed") +
#   scale_color_continuous(guide = "colourbar") +
#   theme_minimal() +
#   theme(axis.title = element_text(size = 11, color = 'gray19'),
#         axis.line = element_blank(),
#         panel.grid.minor.y = element_blank(),
#         axis.text.y = element_text(size = 11, color = 'gray19'),
#         axis.text.x = element_text(size = 10, color = 'gray19', angle=45, hjust=0.95, vjust=0.5),
#         legend.position = 'right') + #c(0.8,0.2)) +
#   labs(y=expression("Carbon balance (Mg C ha"^{-1}*')'), x='Time since last fire (years)',
#        color = 'Turnover',
#        title = 'Sensitivity of the model output to varying turnover values') +
#   annotate('text', x=30, y=-9, size=3,
#            label='Decomposition ~ Turnover \nFixed AGN/AGB ratio = 15%' )
# 
# x11(); scen_combf_fwd_timeplot
# 
# png("./figures/SEEGFire_AM_IntModel_SA_Turnover_timelines_20220404.png", 
#     width = 20, height = 8, units = 'cm', res = 400) 
# scen_TurnOver_Decomp_timeplot
# dev.off()


# --- plotting - Shaded background as s.d.

scen_combf_cwd_summary = 
  scen_combf_cwd[,c("time","CBalance")] %>% 
  group_by(time) %>%
  dplyr::summarise(mean.CB = mean(CBalance),
                   sd.CB = sd(CBalance),
                   n.CB = dplyr::n()) %>%
  mutate(upper.sd.CB = mean.CB + sd.CB,
         lower.sd.CB = mean.CB - sd.CB,
         se.CB = sd.CB / sqrt(n.CB),
         lower.ci.CB = mean.CB - qt(1 - (0.005 / 2), n.CB - 1) * se.CB,
         upper.ci.CB = mean.CB + qt(1 - (0.005 / 2), n.CB - 1) * se.CB)


# #library(gmodels) -- I got the same result.
# scen_TurnOver_Decomp[,c("time","CBalance")] %>%
#   group_by(time) %>%
#   summarise(mean = ci(CBalance, confidence=0.995)[1],
#             lowCI = ci(CBalance, confidence=0.995)[2],
#             hiCI = ci(CBalance, confidence=0.995)[3],
#             sd = ci(CBalance, confidence=0.995)[4])

scen_combf_cwd_summary = as.data.frame(scen_combf_cwd_summary)
#scen_TurnOver_Decomp_summary$time = as.factor(scen_TurnOver_Decomp_summary$time)

#plot
# scen_combf_fwd_timeplot2 = 
#   ggplot() +
#   # geom_line(data = scen_TurnOver_Decomp, 
#   #           aes(x = time, y = CBalance, group = Ref.turn_decomp, color = Ref.turn_decomp), size = 1) +
#   geom_ribbon(data = scen_combf_fwd_summary,
#              aes(x = time, ymin = lower.sd.CB, ymax = upper.sd.CB), 
#              #aes(x = time, ymin = lower.ci.CB, ymax = upper.ci.CB), 
#              fill='blue', alpha = 0.4) +
#   geom_line(data = scen_combf_fwd_summary,
#             aes(x = time, y = mean.CB)) +
#   geom_hline(yintercept = 0, linetype = "dashed") +
#   theme_minimal() +
#   theme(axis.title = element_text(size = 11, color = 'gray19'),
#         axis.line = element_blank(),
#         panel.grid.minor.y = element_blank(),
#         axis.text.y = element_text(size = 11, color = 'gray19'),
#         axis.text.x = element_text(size = 10, color = 'gray19', angle=45, hjust=0.95, vjust=0.5),
#         legend.position = 'none') + #c(0.8,0.2)) +
#   labs(y=expression("Carbon balance (Mg C ha"^{-1}*')'), x='Time since last fire (years)') +
#   annotate('text', x=30, y=-9, size=3,
#            label='Average C balance ? 1 s.d. \nDecomposition ~ Turnover \nFixed AGN/AGB ratio = 15%' )
# 
# x11(); scen_combf_fwd_timeplot2
# 
# png("./figures/SEEGFire_AM_IntModel_SA_Turnover_timelines_sd_20220404.png", 
#     width = 20, height = 8, units = 'cm', res = 400) 
# scen_TurnOver_Decomp_timeplot2
# dev.off()



## Final balance - line plot -------------------------------------------------------

scen_combf_cwd_total = scen_combf_cwd %>%
  group_by(combF.cwd) %>%
  dplyr::summarise(total = sum(CBalance))

total.stock = stocks[2] + stocks[3]

scen_combf_cwd_total$total = - scen_combf_cwd_total$total

scen_combf_cwd_total$total_perc = scen_combf_cwd_total$total/total.stock*100 

scen_combf_cwd_total = as.data.frame(scen_combf_cwd_total)
scen_combf_cwd_total$combf.cwd.pc = scen_combf_cwd_total$combF.cwd*100


#plot

scen_combf_cwd_totalplot = 
  ggplot(scen_combf_cwd_total, 
         aes(x = combf.cwd.pc, y = total_perc)) +
  geom_line(size = 1.2, color = '#CC79A7', alpha=0.9) +
  #scale_x_continuous(limits=c(1,5), breaks = seq(1,5,1)) + 
  scale_y_continuous(limits = c(30,33)) +
  # scale_y_continuous(sec.axis = sec_axis(~ . /total.stock*100, name = "Carbon loss (%)"),
  #                    limits=c(33,55), breaks = seq(35,55,5)) +
  theme_bw() +
  theme(axis.title = element_text(size = 11, color = 'gray19'),
        axis.line = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.text = element_text(size = 11, color = 'gray19'),
        legend.position = 'bottom') +
  labs(y="Total carbon loss (%)", x='Cwd combustion factor (%)')
#labs(y=expression("Total carbon loss (Mg C ha"^{-1}*')'), x='Cwd combustion factor (%)')
# annotate('text', x=4, y=36, size=3.5,
#          label='Decomposition ~ Turnover \nFixed AGN/AGB ratio = 15%' )

x11(); scen_combf_cwd_totalplot



## --- fwd combustion factor ________________________________________________________

scenarios_combf_fwd <- function(combf.fwd.mean, combf.fwd.sd, n, stocks, simtime, model.multi, auxs, auxs.multiB) {
  vec <- combf.fwd.mean + combf.fwd.sd*scale(rnorm(n)) # generate a normal distribution sample based on a given mean and s.d.
  #vec <- seq(TurnOver, by = step, length.out = n)
  base <- data.frame()
  for (i in 1:length(vec)) {
    auxs["aREF.combF.fwd.sF"] <- vec[i]
    temp <- model.multi(simtime, first.last.years, fire.years, stocks, auxs, auxs.multiB)
    temp$combF.fwd <- vec[i]
    temp$combF.cwd <- auxs$aREF.combF.cwd.sF
    temp$combF.fwd.text <- paste0("fwd combF = ",100*round(vec[i],3),"%")
    temp$combF.cwd.text <- paste0("cwd combF = ",100*auxs$aREF.combF.cwd.sF,"%")
    base <- rbind(base,temp)
  }
  return(base)
}

scen_combf_fwd <- scenarios_combf_fwd(combf.fwd.mean=0.683, combf.fwd.sd=0.138, n=1000, 
                                      stocks, simtime, model.multi, auxs, auxs.multiB) 
head(scen_combf_fwd)
mean(scen_combf_fwd$combF.fwd)  ## 0.683
sd(scen_combf_fwd$combF.fwd)    ## 0.138


## Timeline comparison ------------------------------------------------------

#organizing
summary(scen_combf_fwd$combF.fwd)
#scen_combf_fwd = subset(scen_combf_fwd,Decomp.rate>=0)
summary(scen_combf_fwd$combF.cwd)
#scen_combf_fwd$time = as.factor(scen_combf_fwd$time)

#scen_TurnOver_Decomp = subset(scen_TurnOver_Decomp, TOver.rate > 0.02 & TOver.rate < 0.04)

# --- plotting - Carnival
#scen_TurnOver_Decomp = subset(scen_TurnOver_Decomp, TOver.rate > 0.02 & TOver.rate < 0.04)

# scen_combf_fwd_timeplot = 
#   ggplot(scen_combf_fwd, 
#          aes(x = time, y = CBalance, color = combF.fwd)) +
#   geom_line(size = 1) +
#   geom_hline(yintercept = 0, linetype = "dashed") +
#   scale_color_continuous(guide = "colourbar") +
#   theme_minimal() +
#   theme(axis.title = element_text(size = 11, color = 'gray19'),
#         axis.line = element_blank(),
#         panel.grid.minor.y = element_blank(),
#         axis.text.y = element_text(size = 11, color = 'gray19'),
#         axis.text.x = element_text(size = 10, color = 'gray19', angle=45, hjust=0.95, vjust=0.5),
#         legend.position = 'right') + #c(0.8,0.2)) +
#   labs(y=expression("Carbon balance (Mg C ha"^{-1}*')'), x='Time since last fire (years)',
#        color = 'Turnover',
#        title = 'Sensitivity of the model output to varying turnover values') +
#   annotate('text', x=30, y=-9, size=3,
#            label='Decomposition ~ Turnover \nFixed AGN/AGB ratio = 15%' )
# 
# x11(); scen_combf_fwd_timeplot
# 
# png("./figures/SEEGFire_AM_IntModel_SA_Turnover_timelines_20220404.png", 
#     width = 20, height = 8, units = 'cm', res = 400) 
# scen_TurnOver_Decomp_timeplot
# dev.off()


# --- plotting - Shaded background as s.d.

scen_combf_fwd_summary = 
  scen_combf_fwd[,c("time","CBalance")] %>% 
  group_by(time) %>%
  dplyr::summarise(mean.CB = mean(CBalance),
                   sd.CB = sd(CBalance),
                   n.CB = n()) %>%
  mutate(upper.sd.CB = mean.CB + sd.CB,
         lower.sd.CB = mean.CB - sd.CB,
         se.CB = sd.CB / sqrt(n.CB),
         lower.ci.CB = mean.CB - qt(1 - (0.005 / 2), n.CB - 1) * se.CB,
         upper.ci.CB = mean.CB + qt(1 - (0.005 / 2), n.CB - 1) * se.CB)


# #library(gmodels) -- I got the same result.
# scen_TurnOver_Decomp[,c("time","CBalance")] %>%
#   group_by(time) %>%
#   summarise(mean = ci(CBalance, confidence=0.995)[1],
#             lowCI = ci(CBalance, confidence=0.995)[2],
#             hiCI = ci(CBalance, confidence=0.995)[3],
#             sd = ci(CBalance, confidence=0.995)[4])

scen_combf_fwd_summary = as.data.frame(scen_combf_fwd_summary)
#scen_TurnOver_Decomp_summary$time = as.factor(scen_TurnOver_Decomp_summary$time)

#plot
# scen_combf_fwd_timeplot2 = 
#   ggplot() +
#   # geom_line(data = scen_TurnOver_Decomp, 
#   #           aes(x = time, y = CBalance, group = Ref.turn_decomp, color = Ref.turn_decomp), size = 1) +
#   geom_ribbon(data = scen_combf_fwd_summary,
#              aes(x = time, ymin = lower.sd.CB, ymax = upper.sd.CB), 
#              #aes(x = time, ymin = lower.ci.CB, ymax = upper.ci.CB), 
#              fill='blue', alpha = 0.4) +
#   geom_line(data = scen_combf_fwd_summary,
#             aes(x = time, y = mean.CB)) +
#   geom_hline(yintercept = 0, linetype = "dashed") +
#   theme_minimal() +
#   theme(axis.title = element_text(size = 11, color = 'gray19'),
#         axis.line = element_blank(),
#         panel.grid.minor.y = element_blank(),
#         axis.text.y = element_text(size = 11, color = 'gray19'),
#         axis.text.x = element_text(size = 10, color = 'gray19', angle=45, hjust=0.95, vjust=0.5),
#         legend.position = 'none') + #c(0.8,0.2)) +
#   labs(y=expression("Carbon balance (Mg C ha"^{-1}*')'), x='Time since last fire (years)') +
#   annotate('text', x=30, y=-9, size=3,
#            label='Average C balance ? 1 s.d. \nDecomposition ~ Turnover \nFixed AGN/AGB ratio = 15%' )
# 
# x11(); scen_combf_fwd_timeplot2
# 
# png("./figures/SEEGFire_AM_IntModel_SA_Turnover_timelines_sd_20220404.png", 
#     width = 20, height = 8, units = 'cm', res = 400) 
# scen_TurnOver_Decomp_timeplot2
# dev.off()



## Final balance - line plot -------------------------------------------------------

scen_combf_fwd_total = scen_combf_fwd %>%
  group_by(combF.fwd) %>%
  dplyr::summarise(total = sum(CBalance))

scen_combf_fwd_total$total = - scen_combf_fwd_total$total
scen_combf_fwd_total$total_perc = scen_combf_fwd_total$total/total.stock*100

scen_combf_fwd_total = as.data.frame(scen_combf_fwd_total)
scen_combf_fwd_total$combf.fwd.pc = scen_combf_fwd_total$combF.fwd*100

scen_combf_fwd_total = subset(scen_combf_fwd_total, combf.fwd.pc<=100)


#plot

#total.stock = stocks[2] + stocks[3]

scen_combf_fwd_totalplot = 
  ggplot(scen_combf_fwd_total, 
         aes(x = combf.fwd.pc, y = total_perc)) +
  geom_line(size = 1.2, color = '#CC79A7', alpha=0.9) +
  #scale_x_continuous(limits=c(1,5), breaks = seq(1,5,1)) + 
  scale_y_continuous(limits = c(30,33)) +
  # scale_y_continuous(sec.axis = sec_axis(~ . /total.stock*100, name = "Carbon loss (%)"),
  #                    limits=c(33,55), breaks = seq(35,55,5)) +
  theme_bw() +
  theme(axis.title = element_text(size = 11, color = 'gray19'),
        axis.line = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.text = element_text(size = 11, color = 'gray19'),
        legend.position = 'bottom') +
  labs(y="Total carbon loss (%)", x='Fwd combustion factor (%)') 
# labs(y=expression("Total carbon loss (Mg C ha"^{-1}*')'), x='Fwd combustion factor (%)') +
# annotate('text', x=4, y=36, size=3.5,
#          label='Decomposition ~ Turnover \nFixed AGN/AGB ratio = 15%' )

x11(); scen_combf_fwd_totalplot



## turnover ~ decomposition ______________________________________________________

# using a fixed AGB/AGN rate 
# and setting decomposition as a function of turnover

# set decomposition as a function of turnover
# 0.03*300 = 0.2*45 #i.e., 3% of 300 Mg = 20% of 45Mg
# aRef.turnover*300 = aREF.decomposition*45 
# aREF.decomposition = aRef.turnover*300/45
# aREF.decomposition = aRef.turnover*300/45 # i.e., it is also a function of the AGB/AGN proportion
# aREF.decomposition = aRef.turnover/(AGN/AGB) # ~ AGN/AGB proportion (changed order)
# This is the rule: aREF.decomposition = aRef.turnover/(AGN/AGB)

scenarios_turnover <- function(TurnOver.mean, TurnOver.sd, n, stocks, AGN.AGB.prop, simtime, model.multi, auxs, auxs.multiB) {
  vec <- TurnOver.mean + TurnOver.sd*scale(rnorm(n)) # generate a normal distribution sample based on a given mean and s.d.
  #vec <- seq(TurnOver, by = step, length.out = n)
  base <- data.frame()
  for (i in 1:length(vec)) {
    auxs["aRef.turnover"] <- vec[i]
    auxs["aREF.decomposition"] <- vec[i]/AGN.AGB.prop # decomposition as a function of turnover, with fixed AGN/AGB 
    temp <- model.multi(simtime, first.last.years, fire.years, stocks, auxs, auxs.multiB)
    temp$TOver.rate <- vec[i]
    temp$Decomp.rate <- vec[i]/AGN.AGB.prop
    temp$TOver.text <- paste0("Turnover = ",100*round(vec[i],3),"%")
    temp$Decomp.text <- paste0("Decomposition = ",100*round((vec[i]/AGN.AGB.prop),3),"%") # decomposition as a function of turnover,with fixed AGN/AGB 
    base <- rbind(base,temp)
  }
  return(base)
}

scen_TurnOver_Decomp <- scenarios_turnover(TurnOver.mean=0.03, TurnOver.sd=0.004, n=1000, AGN.AGB.prop=45/300,
                                           stocks, simtime, model.multi, auxs, auxs.multiB) 
head(scen_TurnOver_Decomp)
mean(scen_TurnOver_Decomp$TOver.rate)  ## 0.03
sd(scen_TurnOver_Decomp$TOver.rate)    ## 0.004


## Timeline comparison ------------------------------------------------------

#organizing
scen_TurnOver_Decomp$Ref.turn_decomp = paste0(scen_TurnOver_Decomp$TOver.text,' ', scen_TurnOver_Decomp$Decomp.text)
scen_TurnOver_Decomp$Ref.turn_decomp = as.factor(scen_TurnOver_Decomp$Ref.turn_decomp)
summary(scen_TurnOver_Decomp$Decomp.rate)
scen_TurnOver_Decomp = subset(scen_TurnOver_Decomp,Decomp.rate>=0)
summary(scen_TurnOver_Decomp$TOver.rate)
#scen_TurnOver_Decomp$time = as.factor(scen_TurnOver_Decomp$time)

#scen_TurnOver_Decomp = subset(scen_TurnOver_Decomp, TOver.rate > 0.02 & TOver.rate < 0.04)

# --- plotting - Carnival
#scen_TurnOver_Decomp = subset(scen_TurnOver_Decomp, TOver.rate > 0.02 & TOver.rate < 0.04)

scen_TurnOver_Decomp_timeplot = 
  ggplot(scen_TurnOver_Decomp, 
         aes(x = time, y = CBalance, group = Ref.turn_decomp, color = TOver.rate)) +
  geom_line(size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_continuous(guide = "colourbar") +
  theme_minimal() +
  theme(axis.title = element_text(size = 11, color = 'gray19'),
        axis.line = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.text.y = element_text(size = 11, color = 'gray19'),
        axis.text.x = element_text(size = 10, color = 'gray19', angle=45, hjust=0.95, vjust=0.5),
        legend.position = 'right') + #c(0.8,0.2)) +
  labs(y=expression("Carbon balance (Mg C ha"^{-1}*')'), x='Time since last fire (years)',
       color = 'Turnover') + #,
  #title = 'Sensitivity of the model output to varying turnover values') +
  annotate('text', x=30, y=-5, size=3,
           label='Decomposition ~ Turnover \nFixed AGN/AGB ratio = 15%' )

x11(); scen_TurnOver_Decomp_timeplot

png("./Figures/SM_Figure5.png",
    width = 20, height = 10, units = 'cm', res = 400)
scen_TurnOver_Decomp_timeplot
dev.off()


# # --- plotting - Shaded background - not yet!
# 
# scenarios_turnover <- function(TurnOver.mean, TurnOver.sd, stocks, AGB.AGN.prop, simtime, model, auxs) {
#   #vec <- TurnOver.mean + TurnOver.sd*scale(rnorm(n)) # generate a normal distribution sample based on a given mean and s.d.
#   vec <- c(TurnOver.mean-TurnOver.sd, TurnOver.mean, TurnOver.mean+TurnOver.sd)
#   base <- data.frame()
#   for (i in 1:length(vec)) {
#     auxs["aRef.turnover"] <- vec[i]
#     auxs["aREF.decomposition"] <- vec[i]*AGB.AGN.prop # decomposition as a function of turnover, with fixed AGB/AGN 
#     temp <- data.frame(ode(y = stocks, times = simtime, func = model, parms = auxs, method = "euler"))
#     temp$TOver.rate <- vec[i]
#     temp$Decomp.rate <- vec[i]*AGB.AGN.prop
#     temp$TOver.text <- paste0("Turnover = ",100*round(vec[i],3),"%")
#     temp$Decomp.text <- paste0("Decomposition = ",100*round((vec[i]*AGB.AGN.prop),3),"%") # decomposition as a function of turnover,with fixed AGB/AGN 
#     base <- rbind(base,temp)
#   }
#   return(base)
# }
# 
# scen_TurnOver_Decomp <- scenarios_turnover(TurnOver.mean=0.03, TurnOver.sd=0.01, AGB.AGN.prop=300/45,
#                                            stocks, simtime, model, auxs) 
# scen_TurnOver_Decomp_sub = subset(scen_TurnOver_Decomp, TOver.rate != 0.03)
# scen_TurnOver_Decomp_sub$TOver.rate = as.factor(scen_TurnOver_Decomp_sub$TOver.rate)
# #scen_TurnOver_Decomp_sub$time = as.factor(scen_TurnOver_Decomp_sub$time)
# scen_TurnOver_Decomp_sub = scen_TurnOver_Decomp_sub[,-c(2:9,12:14)]
# levels(scen_TurnOver_Decomp_sub$TOver.rate) = c('a','b')
# 
# library(tidyverse)
# 
# bounds <- scen_TurnOver_Decomp_sub %>%
#   pivot_wider(names_from = TOver.rate, values_from = CBalance) %>%
#   mutate(
#     ymax = pmax(a, b),
#     ymin = pmin(a, b),
#     #fill = a >= b
#     )
# 
# bounds = as.data.frame(bounds)
# 
# scen_TurnOver_Decomp_timeplot2 = 
#   ggplot() +
#   geom_ribbon(data = bounds, aes(time, ymin = ymin, ymax = ymax), fill='blue', alpha = 0.4) +
#   geom_line(data=subset(scen_TurnOver_Decomp, TOver.rate ==0.03),
#             aes(x = time, y = CBalance)) +
#   geom_hline(yintercept = 0, linetype = "dashed") +
#   theme_minimal() +
#   theme(axis.title = element_text(size = 11, color = 'gray19'),
#         axis.line = element_blank(),
#         panel.grid.minor.y = element_blank(),
#         axis.text.y = element_text(size = 11, color = 'gray19'),
#         axis.text.x = element_text(size = 10, color = 'gray19', angle=45, hjust=0.95, vjust=0.5),
#         legend.position = 'none') + #c(0.8,0.2)) +
#   labs(y=expression("Final carbon balance (Mg C ha"^{-1}*')'), x='Time since last fire (years)')
# 
# x11(); scen_TurnOver_Decomp_timeplot2


# --- plotting - Shaded background as s.d.

scen_TurnOver_Decomp_summary = 
  scen_TurnOver_Decomp[,c("time","CBalance")] %>% 
  group_by(time) %>%
  dplyr::summarise(mean.CB = mean(CBalance),
            sd.CB = sd(CBalance),
            n.CB = dplyr::n()) %>%
  dplyr::mutate(upper.sd.CB = mean.CB + sd.CB,
         lower.sd.CB = mean.CB - sd.CB,
         se.CB = sd.CB / sqrt(n.CB),
         lower.ci.CB = mean.CB - qt(1 - (0.005 / 2), n.CB - 1) * se.CB,
         upper.ci.CB = mean.CB + qt(1 - (0.005 / 2), n.CB - 1) * se.CB)


# #library(gmodels) -- I got the same result.
# scen_TurnOver_Decomp[,c("time","CBalance")] %>%
#   group_by(time) %>%
#   summarise(mean = ci(CBalance, confidence=0.995)[1],
#             lowCI = ci(CBalance, confidence=0.995)[2],
#             hiCI = ci(CBalance, confidence=0.995)[3],
#             sd = ci(CBalance, confidence=0.995)[4])

scen_TurnOver_Decomp_summary = as.data.frame(scen_TurnOver_Decomp_summary)
#scen_TurnOver_Decomp_summary$time = as.factor(scen_TurnOver_Decomp_summary$time)

#plot
scen_TurnOver_Decomp_timeplot2 = 
  ggplot() +
  # geom_line(data = scen_TurnOver_Decomp, 
  #           aes(x = time, y = CBalance, group = Ref.turn_decomp, color = Ref.turn_decomp), size = 1) +
  geom_ribbon(data = scen_TurnOver_Decomp_summary,
              aes(x = time, ymin = lower.sd.CB, ymax = upper.sd.CB), 
              #aes(x = time, ymin = lower.ci.CB, ymax = upper.ci.CB), 
              fill='blue', alpha = 0.4) +
  geom_line(data = scen_TurnOver_Decomp_summary,
            aes(x = time, y = mean.CB)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_bw() +
  theme(axis.title = element_text(size = 11, color = 'gray19'),
        axis.line = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.text.y = element_text(size = 11, color = 'gray19'),
        axis.text.x = element_text(size = 10, color = 'gray19', angle=45, hjust=0.95, vjust=0.5),
        legend.position = 'none') + #c(0.8,0.2)) +
  labs(y=expression("Carbon balance (Mg C ha"^{-1}*')'), x='Time since last fire (years)') +
  annotate('text', x=30, y=-9, size=3,
           label='Average C balance ? 1 s.d. \nDecomposition ~ Turnover \nFixed AGN/AGB ratio = 15%' )

x11(); scen_TurnOver_Decomp_timeplot2

# png("./figures/SEEGFire_AM_IntModel_SA_Turnover_timelines_sd_20220404.png", 
#     width = 20, height = 8, units = 'cm', res = 400) 
# scen_TurnOver_Decomp_timeplot2
# dev.off()



## Final balance - Bar plot -------------------------------------------------------

scen_TurnOver_Decomp_total <- scen_TurnOver_Decomp %>%
  dplyr::group_by(TOver.rate, Decomp.rate) %>%
  dplyr::summarise(
    total = sum(CBalance),
    .groups = "drop"
  )

# total.stock = stocks[2] + stocks[3]

scen_TurnOver_Decomp_total$total = - scen_TurnOver_Decomp_total$total
scen_TurnOver_Decomp_total$total_perc = scen_TurnOver_Decomp_total$total/total.stock*100
mean(scen_TurnOver_Decomp_total$total_perc)

scen_TurnOver_Decomp_total = as.data.frame(scen_TurnOver_Decomp_total)
scen_TurnOver_Decomp_total$TOver.rate.pc = scen_TurnOver_Decomp_total$TOver.rate*100
scen_TurnOver_Decomp_total$Decomp.rate.pc = scen_TurnOver_Decomp_total$Decomp.rate*100


#plot

AGN.AGB.prop = 0.15 ## <---------! 
# Model turnover and decomposition are linked by the above AGN/AGB proportion despite the AGB and AGN stocks 
# from QCN do not follow this proportion.
# AGN.AGB.prop = stocks[3] / stocks[2]


scen_TurnOver_Decomp_totalplot = 
  ggplot(scen_TurnOver_Decomp_total, 
         aes(x = TOver.rate.pc, y = total_perc)) +
  geom_line(size = 1.2, color = '#CC79A7', alpha=0.9) +
  scale_x_continuous(sec.axis = sec_axis(~ . /AGN.AGB.prop, name = "Decomposition rate (%)"),
                     limits=c(1.5,4.5), breaks = seq(2,4,1)) + 
  scale_y_continuous(limits = c(30,33)) +
  # scale_y_continuous(sec.axis = sec_axis(~ . /total.stock*100, name = "Carbon loss (%)"),
  #                    limits=c(33,55), breaks = seq(35,55,5)) +
  theme_bw() +
  theme(axis.title = element_text(size = 11, color = 'gray19'),
        axis.line = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.text = element_text(size = 11, color = 'gray19'),
        legend.position = 'bottom') +
  labs(y="Total carbon loss (%)", x='Turnover rate (%)') 
# labs(y=expression("Total carbon loss (Mg C ha"^{-1}*')'), x='Turnover rate (%)') +
# annotate('text', x=4, y=36, size=3.5,
#          label='Decomposition ~ Turnover \nFixed AGN/AGB ratio = 15%' )

x11(); scen_TurnOver_Decomp_totalplot


png("./Figures/SM_Figure4.png",
    width = 20, height = 8, units = 'cm', res = 400)

ggarrange(scen_combf_fwd_totalplot +
            annotate('text', x=100, y=33, size=3.5, label='(a)')
          , 
          
          scen_combf_cwd_totalplot +
            theme(axis.title.y = element_blank(),
                  axis.text.y = element_blank()) +
            annotate('text', x=70, y=33, size=3.5, label='(b)')
          , 
          scen_TurnOver_Decomp_totalplot +
            theme(axis.title.y = element_blank(),
                  axis.text.y = element_blank()) +
            annotate('text', x=4.5, y=33, size=3.5, label='(c)')
          
          , 
          ncol=3)

dev.off()
