rm(list = ls());
gc(); 

##_____________________________________________________________________________
## SEEG Fire
## GEE emissions from forest fires not related to deforestation
## Integrated model for the final carbon balance from combustion and mortality/decomposition
## for forestts only
##
## For all fire frequencies - Multiburn
## Manuscript figure - Single-point timeline
##
## R version 4.1.2
##
## Made by: Aline Pontes Lopes, 10/02/2022, eds 25/05/2022
##_______________________________________________________________________________


library(deSolve) #supports numerical integration using a range of numerical methods
library(ggplot2) #supports visualization of layered graphics
library(plyr) #supports data frame merging
library(magrittr) #supports pipe function
library(grid) #supports additional graphing capability
library(egg) #supports visualization of multiple plots & graphs

## TUTORIAL
## https://rpubs.com/rsmard05/sysDynR

# Other resources:
# https://exchange.iseesystems.com/models/editor/aline-pontes-lopes/seegfireemissionmodel20220210
# https://onlinelibrary.wiley.com/doi/abs/10.1002/sdr.1638

options(scipen = 999)


## Single-burn function -----------------------------------------------------------

# the function
model.single <- function(time, stocks, auxs){
  with(as.list(c(stocks, auxs)),{
    
    # Selecting AGB loss rate
    max.YSF = length(AGBloss.lookup) - 1
    aRef.netAGBloss = ifelse(time > max.YSF, 0, -AGBloss.lookup[as.character(time)])
    aRef.netAGNgain = ifelse(time > max.YSF, 0, AGBloss.lookup[as.character(time)])
    
    # Remaing AGB
    fNetAGBloss = s.inAGBstock * aRef.netAGBloss
    fLeftAGB = s.inAGBstock + fNetAGBloss
    
    # Necromass inflows
    fAGNinput.Mortality = s.inAGBstock * aRef.netAGNgain
    fAGNinput.TurnOver = s.inAGBstock * aRef.turnover #ifelse(time==0,0,aRef.turnover)
    fAGNinput = fAGNinput.Mortality + fAGNinput.TurnOver
    
    # Necromass outflows
    # Immediate combustion
    fAGNcomb.fwd = s.inAGNstock * ifelse(time==0, (1-aREF.AGNpart.sF)*aREF.combF.fwd.sF, 0)
    fAGNcomb.cwd = s.inAGNstock * ifelse(time==0, aREF.AGNpart.sF    *aREF.combF.cwd.sF, 0)
    fAGNcomb = fAGNcomb.fwd + fAGNcomb.cwd
    # Decomposition
    fAGNdecomp = (s.inAGNstock + fAGNinput - fAGNcomb)*aREF.decomposition #ifelse(time==0,0,aREF.decomposition)
    #Remaining necromass
    fLeftAGN = (s.inAGNstock + fAGNinput - fAGNcomb)*(1-aREF.decomposition) #(1-ifelse(time==0,0,aREF.decomposition)) 
    
    #Final Carbon balance
    fCBalance = (fLeftAGB - s.inAGBstock) + (fLeftAGN - s.inAGNstock)
    
    # Changes in reference stocks
    dYear_dt = 1
    dAGB_dt = fNetAGBloss
    dAGN_dt = fAGNinput - fAGNcomb - fAGNdecomp
    
    return(list(c(dYear_dt,dAGB_dt, dAGN_dt), 
                NetAGBloss = fNetAGBloss,
                Ref.netAGBloss = aRef.netAGBloss,
                LeftAGB = fLeftAGB,
                AGNinput = fAGNinput,
                AGNcomb = fAGNcomb,
                AGNdecomp = fAGNdecomp,
                LeftAGN = fLeftAGN,
                CBalance = fCBalance
                ))
  })
}



## Multiburn loop -----------------------------------------------------------
# example following the second sheet of the excel 'simulation_netemissions_update4_20220223_tests.xlsx' - second sheet
# available at: https://drive.google.com/drive/u/0/folders/1-Yv82rGGu3HhFHJ7umSBDI-MvBI-OkoG


# multiburn function using the single-burn function ----

model.multi <- function(simtime, first.last.years, fire.years, stocks, auxs, auxs.multiB){
  
  for(i in 1:length(fire.years)) {
    
    if(i==1) { # Single-burn
      
      # update the first-fire year
      stocks[1] =  fire.years[i] 
      
      # run the model
      base = data.frame(ode(y = stocks, times = simtime, func = model.single, parms = auxs, method = "euler"))
      
    } else { # Multiburn
      
      # update the first-fire year
      stocks[1] =  fire.years[i] 
      
      # update AGB and AGN stocks
      new.refYear.data = subset(base, year == fire.years[i])
      stocks[2] = new.refYear.data[[3]] # s.inAGBstock
      stocks[3] = new.refYear.data[[4]] # s.inAGNstock
      
      # update AGB loss for year 0, continuing the previous loop
      auxs[[1]]["0"] = -new.refYear.data[[6]] # AGB Loss Rate 
      
      # update cwd/fwd partioning
      auxs[4] = auxs.multiB[[1]]   
      
      # update cwd combustion factors
      auxs[6] = auxs.multiB[[2]]
      
      # run the model
      temp = data.frame(ode(y = stocks, times = simtime, func = model.single, parms = auxs, method = "euler"))
      
      # blend output dataframe
      
      base = subset(base, year < fire.years[i])
      
      base = rbind(base,temp) 
      
    }
    
    base = subset(base, year>=first.last.years[1] & year <= first.last.years[2])
    
  }
  
  return(base)
}



# Start values ---------------------------------------------------------------------
# example following the second sheet of the excel 'simulation_netemissions_update4_20220223_tests.xlsx' - thrid sheet
# available at: https://drive.google.com/drive/u/0/folders/1-Yv82rGGu3HhFHJ7umSBDI-MvBI-OkoG
# values were rounded


# Set the time period and step. Define the stocks and auxiliaries.

START <- 0
FINISH <- 35
STEP <- 1
simtime <- seq(START, FINISH, by = STEP)

first.last.years = c(1986,2021) # according to the Mapbiomas fire

fire.years = c(1986,1991,2010) # when the fire events happened                             <--- change here

stocks <- c(year = first.last.years[1],                  # start year
            s.inAGBstock = 104.6,       # start AGB stock                                <--- change here
            s.inAGNstock = 9.4)    # start AGN stock = cwd + fwd qcn maps              <--- change here

#old AGBloss.lookup = c(0,0.105,0.069,0.044,0.028,0.017,0.010,0.006,0.004,0.002)
AGBloss.lookup = c(0,0.104,0.080,0.060,0.044,0.032,0.023,0.016,0.011,0.008,0.006,0.004,0.003,0.002,0.001,0.001,0.001)
#names(AGBloss.lookup) = seq(0,9,1)
names(AGBloss.lookup) = seq(0,16,1)

auxs <- list(AGBloss.lookup = AGBloss.lookup,
             aRef.turnover = 0.03,            
             aREF.decomposition = 0.19,        
             aREF.AGNpart.sF = 9.4/(5.0+9.4),    # cwd stock / (fwd + cwd stock)               <--- change here
             aREF.combF.fwd.sF = 0.683,
             aREF.combF.cwd.sF = 0.643
)

# just the variable that are different for single and multiburn
auxs.multiB = list(aREF.AGNpart.sF = 0.757,    # cwd stock / (fwd + cwd stock)
                   aREF.combF.cwd.sF = 0.824)

# APL: 20220525: fire freq, interval and initial stocks changed according to the biome average from QCN maps

# running the function
sModel = model.multi(simtime, first.last.years, fire.years, stocks, auxs, auxs.multiB)

sModel
sum(sModel$CBalance)  


# Saving ------------------------------------------------------------------
# save(list=c("AGBloss.lookup","auxs","auxs.multiB","fire.years","first.last.years",'sModel',
#             "model.multi","model.single","simtime","START","STEP","FINISH","stocks"),
#      file='SEEGmodel_20220525.Rdata')

#load(file='SEEGmodel_20220404.Rdata')
#load(file='SEEGmodel_20220525.Rdata')


# Plotting ---------------------------------------------------------------------

# old plot with initial and final stocks

# pallete SEEG1 = ["#ffc000","#f68b32","#4f6128","#92d050","#d6e3bc"]

# library(tidyr)
# sModel.AGB = sModel[,c(1:3,7)] %>% gather(key='type', value='AGB',s.inAGBstock:LeftAGB) 
# sModel.AGB = as.data.frame(sModel.AGB)  
# sModel.AGB$type = as.factor(sModel.AGB$type)
# sModel.AGB$type = factor(sModel.AGB$type, levels = c("s.inAGBstock","LeftAGB"))

# # AGB stocks
# 
#   AGBstocks.TimePlot <- 
#     sModel.AGB %>% 
#     ggplot(aes(x=year, y=AGB, fill=type, alpha=type)) +
#     geom_bar(stat = "identity",position="identity") +
#     scale_fill_manual(values=c("#68A12B","#4f6128"),labels=c('initial','final')) +
#     scale_alpha_manual(values = c(0.6,0.7),labels=c('initial','final')) +
#     geom_hline(yintercept = 0) +
#     scale_x_continuous(breaks=sModel$year,
#                      labels = as.character(sModel$time),
#                      expand = c(0.01,0)) +
#     scale_y_continuous(expand = c(0,0)) +
#     theme_minimal() +
#     theme(axis.title = element_text(size = 11, color = 'gray19'),
#           axis.line = element_blank(),
#           panel.grid.minor.y = element_blank(),
#           panel.grid.major.x = element_blank(),
#           panel.grid.minor.x = element_blank(),
#           axis.text.y = element_text(size = 11, color = 'gray19'),
#           axis.text.x = element_text(size = 8, color = 'gray19'),
#           plot.title = element_text(size=13, face="bold", color = 'gray19'),
#           legend.position = c(0.93,0.75)) +
#     labs(x = 'Years since last fire (YSLF)', 
#          y = expression("AGB stock (Mg C year"^{-1}*")"),
#          fill = 'AGB stock',
#          alpha = 'AGB stock') +
#     theme(axis.text.x = element_blank(),
#           axis.title.x = element_blank()) + 
#     annotate("text", x = 2020.5, y = 115, label = "(a)")
# 
# # AGN stocks
#   
#   sModel.AGN = sModel[,c(1:2,4,11)] %>% gather(key='type', value='AGN',s.inAGNstock:LeftAGN) 
#   sModel.AGN = as.data.frame(sModel.AGN)  
#   sModel.AGN$type = as.factor(sModel.AGN$type)
#   sModel.AGN$type = factor(sModel.AGN$type, levels = c("s.inAGNstock","LeftAGN"))
#   
#   AGNstocks.TimePlot <- sModel.AGN %>% 
#     ggplot(aes(x=year, y=AGN, fill=type, alpha=type)) +
#     geom_bar(stat = "identity",position="identity") +
#     scale_fill_manual(values=c("#FFC000","#F7903B"),labels=c('initial','final')) +
#     scale_alpha_manual(values = c(0.7,0.5),labels=c('initial','final')) +
#     geom_hline(yintercept = 0) +
#     scale_x_continuous(breaks=sModel$year,
#                        labels = as.character(sModel$time),
#                        expand = c(0.01,0)) +
#     scale_y_continuous(limits=c(0,30), expand = c(0,0)) +
#     theme_minimal() +
#     theme(axis.title = element_text(size = 11, color = 'gray19'),
#           axis.line = element_blank(),
#           panel.grid.minor.y = element_blank(),
#           panel.grid.major.x = element_blank(),
#           panel.grid.minor.x = element_blank(),
#           axis.text.y = element_text(size = 11, color = 'gray19'),
#           axis.text.x = element_text(size = 8, color = 'gray19'),
#           plot.title = element_text(size=13, face="bold", color = 'gray19'),
#           legend.position = c(0.93,0.75)) +
#     labs(x = 'Years since last fire (YSLF)', 
#          y = expression("AGN stock (Mg C year"^{-1}*")"),
#          fill = 'AGN stock',
#          alpha = 'AGN stock') +
#   theme(axis.text.x = element_blank(),
#         axis.title.x = element_blank()) +
#   annotate("text", x = 2020.5, y = 29, label = "(b)")
#   
#   
#   # AGNinput.TimePlot <- sModel %>% 
#   #   ggplot() +
#   #   geom_bar(aes(x=year, y=AGNinput), stat = 'identity', fill = "brown") +
#   #   theme_minimal() +
#   #   theme(axis.text.x = element_blank(),
#   #         axis.title.x = element_blank()) +
#   #   labs(y='AGN input (Mg/ha)')
#   
#   # AGNcomb.TimePlot <- sModel %>% 
#   #   ggplot() +
#   #   geom_bar(aes(x=year, y=AGNcomb), stat = 'identity', fill = "orange") +
#   #   theme_minimal() +
#   #   theme(axis.text.x = element_text(angle=45, hjust=0.95, vjust=0.8)) +
#   #   labs(y='Combusted AGN (Mg/ha)')
#   # 
#   # AGNdecomp.TimePlot <- sModel %>% 
#   #   ggplot() +
#   #   geom_bar(aes(x=year, y=AGNdecomp), stat = 'identity', fill = "orange") +
#   #   theme_minimal() +
#   #   theme(axis.text.x = element_text(angle=45, hjust=0.95, vjust=0.8)) +
#   #   labs(y='decomposed AGN (Mg/ha)')
# 
# # C Balance
#   
#   FCBalance.TimePlot = sModel %>%
#     ggplot() +
#     geom_bar(aes(x=year, y = CBalance),stat = "identity", alpha=0.9, fill = "#B98DE2") +
#     geom_hline(yintercept = 0) +
#     scale_x_continuous(breaks=sModel$year,
#                        labels = as.character(sModel$time),
#                        expand = c(0.01,0)) +
#     scale_y_continuous(expand = c(0,0)) +
#     theme_minimal() +
#     theme(axis.title = element_text(size = 11, color = 'gray19'),
#           axis.line = element_blank(),
#           panel.grid.minor.y = element_blank(),
#           panel.grid.major.x = element_blank(),
#           panel.grid.minor.x = element_blank(),
#           axis.text.y = element_text(size = 11, color = 'gray19'),
#           axis.text.x = element_text(size = 10, color = 'gray19'),
#           plot.title = element_text(size=13, face="bold", color = 'gray19')) +
#     labs(x = 'Years since last fire (YSLF)', 
#          y = expression("Carbon balance (Mg C year"^{-1}*")")) +
#     annotate("text", x = 2020.5, y = -2, label = "(c)")
#     
#   
#   # timeline.plots = ggarrange(LeftAGB.TimePlot, AGNinput.TimePlot, LeftAGN.TimePlot,
#   #                            AGNcomb.TimePlot, AGNdecomp.TimePlot, FCBalance.TimePlot,
#   #                            ncol = 3)
#  
# 
# 
# 
# # save figure
# png("./figures/ManuscriptFigure4_SinglePoint_3fires_20220324.png",
#      width = 22, height = 20, units = 'cm', res = 300)
# timeline.plots = ggarrange(AGBstocks.TimePlot, 
#                            AGNstocks.TimePlot,
#                            FCBalance.TimePlot,
#                            ncol = 1)
# dev.off()


# plot with the final stocks only


cbPal <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# Final AGB stocks

  AGBstocks.TimePlot <-
    sModel %>%
    ggplot(aes(x=year, y=LeftAGB)) +
    geom_bar(stat = "identity", position="identity", fill="#009E73",alpha=0.7) + #old color: #68A12B
    geom_hline(yintercept = 0) +
    geom_vline(xintercept = fire.years, linetype = "dashed",color = 'gray19') +
    scale_x_continuous(breaks=sModel$year,
                     labels = as.character(sModel$time),
                     expand = c(0.01,0)) +
    scale_y_continuous(expand = c(0,0)) +
    theme_minimal() +
    theme(axis.title = element_text(size = 14, color = 'gray19'),
          axis.line = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          axis.text.y = element_text(size = 9, color = 'gray19'),
          axis.text.x = element_text(size = 8, color = 'gray19'),
          plot.title = element_text(size=13, face="bold", color = 'gray19'),
          legend.position = c(0.93,0.75),
          plot.subtitle = element_text(size = 8,color = "gray19",face = "italic")) +
    labs(x = 'Years since last fire (YSLF)',
         y = expression(atop(NA,atop("Final AGB stock",paste("(Mg C year"^{-1}*")")))),
         subtitle = '    fire 1                fire 2                                                                                     fire 3') +
    theme(axis.text.x = element_blank(),
          axis.title.x = element_blank()) +
    annotate("text", x = 2020.5, y = 103, label = "(a)",size=4,color = 'gray19')

# AGN input

  AGNinput.TimePlot <- 
    sModel %>%
    ggplot(aes(x=year, y=AGNinput)) +
    geom_bar(stat = 'identity', fill = "#D55E00",alpha=0.7) +
    geom_hline(yintercept = 0) +
    geom_vline(xintercept = fire.years, linetype = "dashed",color = 'gray19') +
    scale_x_continuous(breaks=sModel$year,
                       labels = as.character(sModel$time),
                       expand = c(0.01,0)) +
    scale_y_continuous(expand = c(0,0)) +
    theme_minimal() +
    theme(axis.title = element_text(size = 14, color = 'gray19'),
          axis.line = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          axis.text.y = element_text(size = 9, color = 'gray19'),
          axis.text.x = element_text(size = 8, color = 'gray19'),
          plot.title = element_text(size=13, face="bold", color = 'gray19'),
          legend.position = c(0.93,0.75)) +
    labs(x = 'Years since last fire (YSLF)',
         y = expression(atop(NA,atop("AGN input",paste("(Mg C year"^{-1}*")"))))) +
    theme(axis.text.x = element_blank(),
          axis.title.x = element_blank()) +
    annotate("text", x = 2020.5, y = 13, label = "(b)",size=4,color = 'gray19')
    #annotate("text", x = 2020.5, y = 13, label = "(c)",size=4,color = 'gray19')

# AGN combusted
  
  AGNcomb.TimePlot <- 
    sModel %>%
    ggplot(aes(x=year, y=AGNcomb)) +
    geom_bar(stat = 'identity', fill = "#0072B2", alpha=0.7) +
    geom_hline(yintercept = 0) +
    geom_vline(xintercept = fire.years, linetype = "dashed",color = 'gray19') +
    scale_x_continuous(breaks=sModel$year,
                       labels = as.character(sModel$time),
                       expand = c(0.01,0)) +
    scale_y_continuous(expand = c(0,0)) +
    theme_minimal() +
    theme(axis.title = element_text(size = 14, color = 'gray19'),
          axis.line = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          axis.text.y = element_text(size = 9, color = 'gray19'),
          axis.text.x = element_text(size = 8, color = 'gray19'),
          plot.title = element_text(size=13, face="bold", color = 'gray19'),
          legend.position = c(0.93,0.75)) +
    labs(x = 'Years since last fire (YSLF)',
         y = expression(atop(NA,atop("Combusted AGN",paste("(Mg C year"^{-1}*")"))))) +
    theme(axis.text.x = element_blank(),
          axis.title.x = element_blank()) +
    annotate("text", x = 2020.5, y = 18, label = "(c)",size=4,color = 'gray19')
    #annotate("text", x = 2020.5, y = 18, label = "(b)",size=4,color = 'gray19')
    
# AGN decomp

  AGNdecomp.TimePlot <- 
    sModel %>%
    ggplot(aes(x=year, y=AGNdecomp)) +
    geom_bar(stat = 'identity', fill = "#56B4E9", alpha = 0.7) +
    geom_hline(yintercept = 0) +
    geom_vline(xintercept = fire.years, linetype = "dashed",color = 'gray19') +
    scale_x_continuous(breaks=sModel$year,
                       labels = as.character(sModel$time),
                       expand = c(0.01,0)) +
    scale_y_continuous(expand = c(0,0)) +
    theme_minimal() +
    theme(axis.title = element_text(size = 14, color = 'gray19'),
          axis.line = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          axis.text.y = element_text(size = 9, color = 'gray19'),
          axis.text.x = element_text(size = 8, color = 'gray19'),
          plot.title = element_text(size=13, face="bold", color = 'gray19'),
          legend.position = c(0.93,0.75)) +
    labs(x = 'Years since last fire (YSLF)',
         y = expression(atop(NA,atop("Decomposed AGN",paste("(Mg C year"^{-1}*")"))))) +
    theme(axis.text.x = element_blank(),
          axis.title.x = element_blank()) +
    annotate("text", x = 2020.5, y = 5, label = "(d)",size=4,color = 'gray19')
  
# Final AGN stocks

  AGNstocks.TimePlot <- 
    sModel %>%
    ggplot(aes(x=year, y=LeftAGN)) +
    geom_bar(stat = "identity",position="identity",fill='#E69F00',alpha=0.7) +  #old: #F7903B
    geom_hline(yintercept = 0) +
    geom_vline(xintercept = fire.years, linetype = "dashed",color = 'gray19') +
    scale_x_continuous(breaks=sModel$year,
                       labels = as.character(sModel$time),
                       expand = c(0.01,0)) +
    scale_y_continuous(limits=c(0,30), expand = c(0,0)) +
    theme_minimal() +
    theme(axis.title = element_text(size = 14, color = 'gray19'),
          axis.line = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          axis.text.y = element_text(size = 9, color = 'gray19'),
          axis.text.x = element_text(size = 8, color = 'gray19'),
          plot.title = element_text(size=13, face="bold", color = 'gray19'),
          legend.position = c(0.93,0.75)) +
    labs(x = 'Years since last fire (YSLF)',
         y = expression(atop(NA,atop("Final AGN stock",paste("(Mg C year"^{-1}*")")))),
         fill = 'AGN stock',
         alpha = 'AGN stock') +
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank()) +
  annotate("text", x = 2020.5, y = 29, label = "(e)",size=4,color = 'gray19')

# C Balance

  FCBalance.TimePlot = sModel %>%
    ggplot() +
    geom_bar(aes(x=year, y = CBalance),stat = "identity", alpha=0.9, fill = "#CC79A7") +  #old: #B98DE2
    geom_hline(yintercept = 0) +
    geom_vline(xintercept = fire.years, linetype = "dashed",color = 'gray19') +
    scale_x_continuous(breaks=sModel$year,
                       labels = as.character(sModel$time),
                       expand = c(0.01,0)) +
    scale_y_continuous(expand = c(0,0)) +
    theme_minimal() +
    theme(axis.title.y = element_text(size = 14, color = 'gray19'),
          axis.title.x = element_text(size = 10, color = 'gray19'),
          axis.line = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          axis.text.y = element_text(size = 9, color = 'gray19'),
          axis.text.x = element_text(size = 8, color = 'gray19'),
          plot.title = element_text(size=13, face="bold", color = 'gray19')) +
    labs(x = 'Years since fire (YSF)',
         y = expression(atop(NA,atop("Carbon balance",paste("(Mg C year"^{-1}*")"))))) +
         #y = expression("Carbon balance (Mg C year"^{-1}*")")) +
    annotate("text", x = 2020.5, y = -3, label = "(f)", size=4,color = 'gray19')
    # annotate("text", x = 2020.5, y = -3, label = "(e)", size=4,color = 'gray19')


  # timeline.plots = ggarrange(LeftAGB.TimePlot, AGNinput.TimePlot, LeftAGN.TimePlot,
  #                            AGNcomb.TimePlot, AGNdecomp.TimePlot, FCBalance.TimePlot,
  #                            ncol = 3)


# save figure
png("./figures/ManuscriptFigure4_SinglePoint_3fires_20220525.png",
     width = 16, height = 22, units = 'cm', res = 300)
timeline.plots = ggarrange(AGBstocks.TimePlot,
                           AGNinput.TimePlot,
                           AGNcomb.TimePlot,
                           AGNdecomp.TimePlot,
                           AGNstocks.TimePlot,
                           FCBalance.TimePlot,
                           ncol = 1)
dev.off()


# #RCGI poster
# 
# png("./figures/ManuscriptFigure4_SinglePoint_3fires_20220525_RCGIposter.png",
#     width = 18, height = 16, units = 'cm', res = 300)
# timeline.plots = ggarrange(AGBstocks.TimePlot,
#                            AGNcomb.TimePlot,
#                            AGNinput.TimePlot,
#                            AGNdecomp.TimePlot,
#                            FCBalance.TimePlot,
#                            ncol = 1)
# dev.off()

