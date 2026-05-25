
##_____________________________________________________________________________
## SEEG Fogo 
## AGB change equation - from field data
##
## Load spreadsheets from Google Earth, join, and plot
##
## Made by: Aline Pontes Lopes, 24/03/2024
##
## APL: version called:
## "AGB_changePA_AM_MT_AC_20220509.R"
##
##_______________________________________________________________________________


#load data
agb_ch = read.csv('./Data/AGB_change/agb_change_allplots_corrected.csv')

#load packages
library(plyr)
#install.packages('Deriv')
library (Deriv)
library(viridis)
library(ggplot2)
library(ggpmisc)

# remove empty cells
agb_ch = agb_ch[complete.cases(agb_ch),]

#rename var
names(agb_ch)[7] = 'CH'  # corrected data

#function for necromass production
nec_p<- function(x,t){
  return(x*exp(-0.32*t))
}

#Nonlinear fitting of AGBB change function
fit_ch<- nls(y ~ a*exp(-b*x)-a , 
               start=list(a=23.5, b=0.3), 
               data = data.frame(x=agb_ch$TSF, y=agb_ch$CH))
summary(fit_ch)

library(stats)
summary(fit_ch)
AIC(fit_ch)

#AGB change function
agb_lossF = function (t){
  return((0.33702*exp(-0.36867*t)-0.33702))
} # function with corrected data

#getting AGB change stats by TSF
ch_mean = ddply(agb_ch, c("TSF"), summarise,
                mean = mean(CH),
                sd = sd(CH),
                se = sd(CH)/sqrt(length(CH)),
                ci.up = mean(CH)+(1.96*(sd(CH)/sqrt(length(CH)))),
                ci.low=mean(CH)-(1.96*(sd(CH)/sqrt(length(CH)))))


#Obtaining relative loss rate
rates=agb_lossF(1:30)
AGB_init= c(450, (rep(NA, 30)))
rel_prev=rep(NA, 30)
for(i in 1:length(AGB_init)){
  AGB_init[i+1]=AGB_init[1]+(rates[i]*AGB_init[1])
  
  rel_prev[i] =(AGB_init[i+1]-AGB_init[i])/AGB_init[i]
}


#plotting

# # regular R plot
# names(agb_ch)[1] <- "State"
# agb_ch$State=factor(agb_ch$State)
# col = viridis(4,0.6)
# palette(col)
# plot(agb_ch$TSF, agb_ch$CH, xlim=c(0, 16), 
#      xlab="Time since fire (years)", ylab= "net AGB change (%)", 
#      main = 'net AGB cumulative change relative to pre-fire stocks',
#      pch=19, col = agb_ch$State, 
#      cex.axis=0.8, cex.main=1.0, cex.sub=0.8)
# lines(0:16,agb_lossF(t=0:16)) # decidimos que iamos para em 16 anos
# par(new = T)
# arrows(ch_mean$TSF, ch_mean$ci.low, ch_mean$TSF, 
#        ch_mean$ci.up, length=0.05, angle=90, code=3, lwd=1)
# points(ch_mean$TSF, ch_mean$mean, pch=19)
# abline(h=0, lty=3)
# legend(8.8,-0.5, legend=unique(agb_ch$State),
#        col = palette(col), cex=0.7, pch=19)


# ggplot

line_df = data.frame(x = 0:16, y=agb_lossF(t=0:16))
formula = y ~ a*exp(-b*x)-a

fit_ch

nlsTxt <- "italic(y) == italic(a) (e ^ italic(-b*x)) - italic(a)"

p1 = ggplot() +
  geom_hline(yintercept = 0, color = 'gray50') +
  geom_point(data=agb_ch, aes(x=TSF,y=CH,color=State),size=2) +
  annotate("text",x=13,y=-0.27,label=nlsTxt,color='gray19',parse=T) +
  geom_errorbar(data=ch_mean, aes(x=TSF, ymin=mean-sd, ymax=mean+sd), width=.4,color = 'gray19') +
  geom_point(data=ch_mean, aes(x=TSF,y=mean), col='gray19', size=3, shape=21, stroke = 1) +
  geom_line(data=line_df, aes(x=x, y=y), linetype=2,size=0.8,color='gray19') + # o smooth est? igual
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

x11(); p1

png("./Figures/Figure2.png", width = 14, height = 10, units = 'cm', res = 300) 
p1
dev.off()
