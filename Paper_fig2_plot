#Last edited 23/05/22

rm(list = ls());

setwd('C:/Users/liepl/OneDrive - inpe.br/IPAM/Codes/AM_AGBchange_data_newModels')

#load data
agb_ch = read.csv('agb_change_allplots_corrected.csv')

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
#names(agb_ch)[6] = 'CH' # old data
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
# agb_lossF = function (t){
#   return((0.25773*exp(-0.52062*t)-0.25773))
# }   # old function
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

# regular R plot

#png("SEEGFire_AM_PrimFor_netAGBdyn_20210927.png", width = 12, height = 10, units = 'cm', res = 300) # old fig

names(agb_ch)[1] <- "State"
agb_ch$State=factor(agb_ch$State)
col = viridis(4,0.6)
palette(col)
plot(agb_ch$TSF, agb_ch$CH, xlim=c(0, 16), 
     xlab="Time since fire (years)", ylab= "net AGB change (%)", 
     main = 'net AGB cumulative change relative to pre-fire stocks',
     pch=19, col = agb_ch$State, 
     cex.axis=0.8, cex.main=1.0, cex.sub=0.8)
lines(0:16,agb_lossF(t=0:16)) # decidimos que iamos para em 16 anos
par(new = T)
arrows(ch_mean$TSF, ch_mean$ci.low, ch_mean$TSF, 
       ch_mean$ci.up, length=0.05, angle=90, code=3, lwd=1)
points(ch_mean$TSF, ch_mean$mean, pch=19)
abline(h=0, lty=3)
legend(8.8,-0.5, legend=unique(agb_ch$State),
       col = palette(col), cex=0.7, pch=19)

dev.off()            


# ggplot

line_df = data.frame(x = 0:16, y=agb_lossF(t=0:16))
formula = y ~ a*exp(-b*x)-a

fit_ch

# nlsParams <-
#   fit_ch$m$getAllPars()
# 
# nlsEqn <-
#   substitute(italic(y) == a (e ^ italic(-b*x)) -a, 
#              list(a = format(nlsParams[['a']], digits = 4), 
#                   b = format(nlsParams[['b']], digits = 4)))
# 
# nlsTxt <- as.character(as.expression(nlsEqn))
nlsTxt <- "italic(y) == italic(a) (e ^ italic(-b*x)) - italic(a)"

p1 = ggplot() +
  geom_hline(yintercept = 0, color = 'gray50') +
  geom_point(data=agb_ch, aes(x=TSF,y=CH,color=State),size=2) +
  # geom_smooth(data=agb_ch, aes(x=TSF,y=CH), 
  #             method = 'nls', 
  #             method.args = list(start = c(a=23.5, b=0.3)), 
  #             formula = formula, se = FALSE, colour='blue') +
  # stat_poly_eq(data=agb_ch, aes(x=TSF,y=CH),   # n?o funciona para nls
  #              method = 'nls', 
  #              method.args = list(start = c(a=23.5, b=0.3)), 
  #              aes(label =  paste(..eq.label.., ..adj.rr.label.., sep = "~~~")),
  #              formula = formula, parse=T,
  #              label.y.npc = 0.5, label.x.npc = 0.5) +
  # stat_fit_augment(method = "nls",
  #                  method.args = args) + 
  #geom_text(x = 13, y = -0.27, label = nlsTxt, color='gray50', parse = TRUE) +
  annotate("text",x=13,y=-0.27,label=nlsTxt,color='gray19',parse=T) +
  geom_errorbar(data=ch_mean, aes(x=TSF, ymin=mean-sd, ymax=mean+sd), width=.4,color = 'gray19') +
  geom_point(data=ch_mean, aes(x=TSF,y=mean), col='gray19', size=3, shape=21, stroke = 1) +
  geom_line(data=line_df, aes(x=x, y=y), linetype=2,size=0.8,color='gray19') + # o smooth est? igual
  scale_color_discrete(labels=c('Acre','Amazonas','Mato Grosso','Par?')) +
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
  labs(y="Net cumulative AGB change (%)", x='Time since fire (years)',
       color='Brazilian state')

x11(); p1

png("SEEGFire_AM_PrimFor_netAGBdyn_20220523.png", width = 14, height = 10, units = 'cm', res = 300) 
p1
dev.off()



# #Find the derivative to get annual rate
# deriv_agbCH = Deriv(~a*exp(-b*t)-a, "t")
# 
# #Rate of AGB change function
# agb_lossRate = function (a=0.25773,b=0.52062, t){
#   return(-(a * b * exp(-(b * t))))
# }
# 
# #poltting
# plot(agb_lossF(t=1:30), type='l', ylim=c(-0.25, 0), xlab='Time since fire',
#      ylab='AGB change (%)')
# lines(agb_lossRate(t=1:30), col='red')
# legend(17, -0.05, legend = c("annual rate (%.y-1)", 'acumulated change (%)'),
#        col=c("red", 'black'), lty=c(1,1), cex=0.5)


