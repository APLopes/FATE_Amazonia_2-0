
##_____________________________________________________________________________
## SEEG Fire
## GEE emissions from forest fires not related to deforestation
## Integrated model for the final carbon balance from combustion and mortality/decomposition
## for forestts only
##
## Heatmap - Carbon loss - fire frequency vs fire interval
##
## R version 4.1.2
##
## Made by: Aline Pontes Lopes, 29/04/2022
##
## called "SEEGFire_SA_4_Freq_vs_Interval_Heatmap_20220429.R" in my PC
##
##_______________________________________________________________________________


library(deSolve) #supports numerical integration using a range of numerical methods
library(ggplot2) #supports visualization of layered graphics
library(plyr) #supports data frame merging
library(magrittr) #supports pipe function
library(grid) #supports additional graphing capability
library(egg) #supports visualization of multiple plots & graphs
library(dplyr)

options(scipen = 999)


## Sensitivity analysis _____________________________________________________________

#fire.freq = ((2-2):(2+2))[-1] # (average-sd):(average+sd)
#fire.int = ((4-4):(4+4))[-1] # (average-sd):(average+sd)
fire.freq = 1:4 # (average-sd):(average+sd)
fire.int = 1:9 # (average-sd):(average+sd)

df = data.frame(freq = rep(fire.freq,each=length(fire.int)),
                interval = rep(fire.int,length(fire.freq),each=1))
df[1,2] = 0
df = df[-(2:length(fire.int)),]

df$years = vector(mode='list', length=nrow(df))

# df = split(df,1:nrow(df))
# class(df[[1]])

for (n in 1:nrow(df)) {
  
  i = 1
  y = 1986
  
  while (i < df[n,1]) {
    y = c(y, max(y) + df[n,2]) 
    i = i+1
  }
  
  df$years[[n]] = y
  
}

#df$years[-1][120]

# run model for each combination of fire years

df$year = vector(mode='list', length=nrow(df))

df$leftAGB.stock = vector(mode='list', length=nrow(df))

df$leftAGN.stock = vector(mode='list', length=nrow(df))

df$annual.CBalance = vector(mode='list', length=nrow(df))

df$total.CBalance = NA

first.last.years = c(1986,2200)

for (n in 1:nrow(df)) {
  
  fire.years = df$years[[n]]
  
  temp = model.multi(simtime, first.last.years, fire.years, stocks, auxs, auxs.multiB)
  
  df$year[[n]] = temp$year
  
  df$leftAGB.stock[[n]] = temp$LeftAGB
  
  df$leftAGN.stock[[n]] = temp$LeftAGN
  
  df$annual.CBalance[[n]] = temp$CBalance

  df$total.CBalance[n] = sum(temp$CBalance)  
  
}

df = as.data.frame(df)
df$freq = as.factor(df$freq)
df$interval = as.factor(df$interval)
df$total.Closs = -df$total.CBalance
df$total.Closs.pc = df$total.Closs/sum(stocks[[2]],stocks[[3]])*100


# heatmap plot

png("./Figures/SM_Figure8.png", 
    width = 18, height = 7, units = 'cm', res = 400) 

ggplot(df, aes(interval, freq, fill=total.Closs.pc)) +
  geom_tile() +
  geom_text(aes(label = round(total.Closs.pc, 0))) +
  #scale_fill_viridis_c()
  scale_fill_distiller(palette = 'Spectral') +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  labs(y='Fire frequency', x='Fire interval (years)', fill='Total C loss (%)')

dev.off()


# # understanding the patterns --------------------------------------------------------------------
# # plotting the same frequencies and different intervals
# 
# subs = subset(df, freq == 2)
# subs = subset(subs, interval == 1 | interval == 5 | interval == 10 | interval == 15)
# subs = droplevels(subs)
# 
# temp.list = list();
# 
# for (i in 1:nrow(subs)) {
#   
#   temp.list[[i]] = data.frame(freq = rep(subs$freq[i],length(subs$annual.CBalance[[i]])),
#                     interval = rep(subs$interval[i],length(subs$annual.CBalance[[i]])),
#                     year = subs$year[[i]],
#                     leftAGB.stock = subs$leftAGB.stock[[i]],
#                     leftAGN.stock = subs$leftAGN.stock[[i]],
#                     annual.CBalance = subs$annual.CBalance[[i]])
#   
# }
# 
# # unlist
# subs_freq2 = dplyr::bind_rows(temp.list)
# 
# subs_freq2$freq = as.factor(subs_freq2$freq)
# subs_freq2$interval = as.factor(subs_freq2$interval)
# subs_freq2$year = as.factor(subs_freq2$year)
# 
# subs_freq2$cum.AGBloss.pc = (stocks[[2]]-subs_freq2$leftAGB.stock)/stocks[[2]]
# subs_freq2$cum.AGNloss.pc = (stocks[[3]]-subs_freq2$leftAGN.stock)/stocks[[3]]
# subs_freq2$cum.Closs.pc = (sum(stocks[[2]],stocks[[3]])-(subs_freq2$leftAGB.stock+subs_freq2$leftAGN.stock))/sum(stocks[[2]],stocks[[3]])
# 
# 
# # plot
# 
# ggplot(subs_freq2, aes(x=year, y=-annual.CBalance, group=interval, fill=interval)) +
#   geom_bar(aes(fill=interval), stat="identity", alpha=0.5) +
#   geom_text(aes(label=round(-annual.CBalance,0)), 
#             vjust = 1.5, size=3) +
#   facet_wrap(.~interval, ncol=1)
# 
# x11();
# ggplot(subs_freq2, aes(x=year, y=leftAGB.stock, group=interval)) +
#   geom_bar(aes(fill=interval), stat="identity", alpha=0.5) +
#   geom_text(aes(label=round(leftAGB.stock,0)), 
#             vjust = 1.5, size=3) +
#   facet_wrap(.~interval, ncol=1)
# 
# x11();
# ggplot(subs_freq2, aes(x=year, y=leftAGN.stock, group=interval)) +
#   geom_bar(aes(fill=interval), stat="identity", alpha=0.5) +
#   geom_text(aes(label=round(leftAGN.stock,0)), 
#             vjust = 1.5, size=3) +
#   facet_wrap(.~interval, ncol=1)
# 
# x11();
# ggplot(subs_freq2, aes(x=year, y=cum.Closs.pc, group=interval)) +
#   geom_bar(aes(fill=interval), stat="identity", alpha=0.5) +
#   geom_text(aes(label=paste0(round(cum.Closs.pc,2)*100,'%')), 
#             vjust = 1.5, size=2) +
#   facet_wrap(.~interval, ncol=1)
# 
# x11();
# ggplot(subs_freq2, aes(x=year, y=cum.AGBloss.pc, group=interval)) +
#   geom_bar(aes(fill=interval), stat="identity", alpha=0.5) +
#   geom_text(aes(label=paste0(round(cum.AGBloss.pc,2)*100,'%')), 
#             vjust = 1.5, size=2) +
#   facet_wrap(.~interval, ncol=1)
# 
# x11();
# ggplot(subs_freq2, aes(x=year, y=cum.AGNloss.pc, group=interval)) +
#   geom_bar(aes(fill=interval), stat="identity", alpha=0.5) +
#   geom_text(aes(label=paste0(round(cum.AGNloss.pc,2)*100,'%')), 
#             vjust = 1.5, size=2) +
#   facet_wrap(.~interval, ncol=1)
# 
# 
# # Closs ~interval
# 
# x11();
# ggplot(droplevels(subset(df, freq == 2)), aes(x=interval, y=total.Closs.pc, group=freq)) +
#   geom_line()


