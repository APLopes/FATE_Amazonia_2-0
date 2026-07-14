
## ============================================================================
## FATE-SEEG Fire manuscript
## Figure 2 – Aboveground Biomass Change Following Fire
##
## Description:
## Processes field inventory data from burned forest plots in the
## Brazilian Amazon to quantify changes in aboveground biomass (AGB)
## as a function of time since fire. Fits the biomass change model and
## generates the visualization used in Figure 2.
##
## Inputs:
## - Plot-level aboveground biomass change data
## - Time-since-fire information for burned forest plots
##
## Outputs:
## - Figure 2 (main text)
## - Fitted AGB change function
## - Summary statistics by time since fire
##
## Author:
## Aline Pontes Lopes
##
## Public repository version.
## ============================================================================


#load packages
library(plyr)
library(viridis)
library(ggplot2)


#load data
agb_ch = read.csv('/data/AGB_change/agb_change_allplots_corrected.csv')

# remove empty cells
agb_ch = agb_ch[complete.cases(agb_ch),]

# rename corrected AGB change variable
names(agb_ch)[7] = 'CH'  # corrected data

# nonlinear model fitted to observed AGB change
fit_ch<- nls(y ~ a*exp(-b*x)-a , 
               start=list(a=23.5, b=0.3), 
               data = data.frame(x=agb_ch$TSF, y=agb_ch$CH))

summary(fit_ch)
AIC(fit_ch)

#AGB change function
agb_lossF = function (t){
  return((0.33702*exp(-0.36867*t)-0.33702))
} # function with corrected data

# summary statistics by time since fire (TSF)
ch_mean = ddply(agb_ch, c("TSF"), summarise,
                mean = mean(CH),
                sd = sd(CH),
                se = sd(CH)/sqrt(length(CH)),
                ci.up = mean(CH)+(1.96*(sd(CH)/sqrt(length(CH)))),
                ci.low=mean(CH)-(1.96*(sd(CH)/sqrt(length(CH)))))


# Obtaining relative loss rate
rates=agb_lossF(1:30)
AGB_init= c(450, (rep(NA, 30)))
rel_prev=rep(NA, 30)
for(i in 1:length(AGB_init)){
  AGB_init[i+1]=AGB_init[1]+(rates[i]*AGB_init[1])
  
  rel_prev[i] =(AGB_init[i+1]-AGB_init[i])/AGB_init[i]
}


# ggplot

line_df = data.frame(x = 0:16, y=agb_lossF(t=0:16))

nlsTxt <- "italic(y) == italic(a) (e ^ italic(-b*x)) - italic(a)"

p1 = ggplot() +
  geom_hline(yintercept = 0, color = 'gray50') +
  geom_point(data=agb_ch, aes(x=TSF,y=CH,color=State),size=2) +
  annotate("text",x=13,y=-0.27,label=nlsTxt,color='gray19',parse=T) +
  geom_errorbar(data=ch_mean, aes(x=TSF, ymin=mean-sd, ymax=mean+sd), width=.4,color = 'gray19') +
  geom_point(data=ch_mean, aes(x=TSF,y=mean), col='gray19', size=3, shape=21, stroke = 1) +
  geom_line(data=line_df, aes(x=x, y=y), linetype=2,size=0.8,color='gray19') + 
  scale_color_discrete(labels=c('Acre','Amazonas','Mato Grosso','Pará')) +
  scale_x_continuous(limits = c(0,16), breaks = seq(0,16,2)) +
  scale_y_continuous(labels = scales::percent,expand = c(0.02,0.02)) +
  theme_bw() +
  theme(panel.border = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_text(size = 11, color = 'gray19'),
        panel.grid.minor.y = element_blank(),
        axis.text = element_text(size = 11, color = 'gray19'),
        legend.direction = 'vertical',
        legend.box.background = element_rect(colour = "gray19"),
        legend.text = element_text(size = 9, color = 'gray19'),
        legend.title = element_text(size = 10, color = 'gray19'),
        legend.position = c(0.85,0.23)) +
  labs(y="Net cumulative C change (%)", x='Time since fire (years)',
       color='Brazilian state')

png("/results/Figures/Figure2.png", width = 14, height = 10, units = 'cm', res = 300) 
print(p1)
dev.off()
