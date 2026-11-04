###
# test code for US stocks

###
# setup
rm(list=ls())
gc()
options(scipen = 99999)

###
# packages - see README for install instructions
library(FLasher) 
library(FLBRP)
library(tidyverse)
library(viridis)
library(readxl)
library(DescTools)
library(patchwork)
library(r4ss)
library(ss3om)

sessionInfo()
#other attached packages:
#[1] ss3om_0.5.3.9004     r4ss_1.52.0          patchwork_1.3.0      DescTools_0.99.60    readxl_1.4.5         viridis_0.6.5        viridisLite_0.4.2    lubridate_1.9.4     
#[9] forcats_1.0.0        stringr_1.5.1        dplyr_1.1.4          purrr_1.0.4          readr_2.1.5          tidyr_1.3.1          tibble_3.3.0         tidyverse_2.0.0     
#[17] FLBRP_2.5.9.9051     FLasher_0.7.1.9225   FLFishery_0.3.8.9012 ggplotFL_2.7.0.9139  ggplot2_3.5.2        FLCore_2.6.20.9336   iterators_1.0.14     lattice_0.22-6      

###
# set dir

dir <-"C:\\Users\\andre\\Documents\\ABI\\SS3\\Quillback_rockfish_california"
setwd(dir)

####
# run SS3
# run(dir = dir, exe = 'ss3', extras = "-nox", skipfinished = F, show_in_console = T)

###
# load ss3 model
out_ass = SS_output(dir=file.path(dir))
# attempt to extract Fmsy or F = biomass target equivalent 
bla = out_ass$derived_quants
#fbtg = filter(bla, Label %in% c('annF_Btgt'))$Value
#fmsy = filter(bla, Label %in% c('annF_MSY'))$Value


(B0 <- bla %>%
  filter(Label == "SSB_unfished") %>%
  pull(Value))
(blim <- 0.25 * B0)

###
# convert ss3 object to FLR
(stk_ass <- readFLSss3(dir=file.path(dir)))
head(stk_ass)
(stk_ass <- nounit(stk_ass)) # needed to remove sex component (if present) so you aren't attempting to fit a SRR to the male part 

###
# extract numbers-at-age F (maybe at-age) and ssb in the final year to provide a comparison
(obsAge = as.data.frame(FLQuants(n=stock.n(stk_ass))))
(obsAgeend = filter(obsAge, year %in% max(obsAge$year)))

(obsSSB = as.data.frame(FLQuants(ssb=ssb(stk_ass))))
(obsSSBend = filter(obsSSB, year %in% max(obsSSB$year)))

obsF = as.data.frame(FLQuants(f=harvest(stk_ass)))
obsFend = filter(obsF, year %in% max(obsF$year))
median(obsFend$data) # simply taken the median across age here - would be better to extract F from the SS3 report file 

###
# plots - get a feel for the stock assessment 
# ssb v rec
ggplot(aes(ssb, rec), data=model.frame(FLQuants(stk_ass, "ssb", "rec"))) +
  geom_point() + geom_smooth(method="loess")+
  theme_bw()

# plot - ssb v time
ggplot(aes(year, y=data), data=obsSSB) +
  geom_point() + geom_smooth(method="loess")+
  theme_bw()

###
# run sr
s = 'Quillback rockfish california'
fbtg = 0.0417134
blim = 24.27085 # strange scale used, I think the model is in tons or thousand tons or something (might need to divide by 1000)
sr = fmle(as.FLSR(stk_ass,model=segreg), fixed=list(b=blim)) # could also use bevholt or ricker, you can also estimate all parameters and not fix
plot(sr)

###
# set F values for deterministic forecast
Fmsy = fbtg
F0 = 0.000001 # needs to very small and not exactly 0

###
# compute forecast and equilibrium (eq) conditions using FLBRP
bp = FLBRP(stk_ass,sr)
fbar(bp)[,1][] = F0
fbar(bp)[,2:101][] = Fmsy 

###
# extract age structure and other values (e.g. ssb) at eq
eqstk = window(as(bp, "FLStock")) 
ageEq = as.data.frame(FLQuants(n=stock.n(eqstk)))
ageEq = filter(ageEq, year %in% c(1, 101))

#bioEq = as.data.frame(FLQuants(spawning_biomass=ssb(bp)))
#bioEq = filter(bioEq, year %in% c(1, 101))

# add F variable so it makes more sense
ageEq$F <- factor(ageEq$year, levels = c("1", "101"), # add level for F
                  labels = c("F0", "Fmsy"))
ageEq$stock = s

#bioEq$F <- factor(bioEq$year, levels = c("1", "101"), # add level for F
#                  labels = c("F0", "Fmsy"))
#bioEq$stock = 'Petrale Sole'

###
# plot age structure to compare
obsAgeend$F <- factor('current AgeStr')
obsAgeend$stock = ageEq$stock[1]

ageStr = rbind(ageEq, obsAgeend)
g = ggplot(aes(x = age, y = data, group = as.factor(year), fill = as.factor(year)), data = ageStr)+
  geom_bar(stat = 'identity', col = 'grey40', alpha = 0.4)+
  scale_fill_manual(values = c('lightskyblue2','red3','mediumseagreen'), labels = c('F=0', 'F=Fbtg','current'), name = '')+
  theme_bw()+
  facet_wrap(~year)+
  ggtitle(paste0(ageStr$stock[1]))+
  theme(strip.background = element_blank(),
        strip.text.x = element_blank(),
        plot.title = element_text(hjust = 0.5))+
  xlab('Age')+
  ylab('Numbers-at-age')

g

ggsave(g, filename = paste0(ageStr$stock[1],'.png'), width = 8, height = 6)

###
# may also want to save out ageEq object so it can be used later for ABImsy calc
setwd("C:\\Users\\andre\\Documents\\ABI\\Draft paper\\Third step\\Data")
write.csv(ageEq, paste0(ageStr$stock[1],'.csv'))

#################################################################################################################
# ###
# # collate datEq objects and save
# setwd("~/Documents/Indicator study/Application to MED and US stocks/US data/ABI run/Eq data")
# ls = list.files()
# dat <- do.call(rbind,lapply(ls, function(x) read.csv(x,header=T, sep=',')))
# setwd("~/Documents/Indicator study/Application to MED and US stocks/US data/ABI run")
# write.csv(dat, 'US age structure at Eq.csv')

#################################################################################################################
###
# run ABI
setwd("C:\\Users\\andre\\Documents\\ABI\\Draft paper\\Third step\\Data")
eqstk = read.csv('Quillback rockfish california.csv')

dir <-"C:\\Users\\andre\\Documents\\ABI\\SS3\\Quillback_rockfish_california"
stk_ass <- readFLSss3(dir=file.path(dir))
stk_ass <- nounit(stk_ass)
head(stk_ass)

# set percentile threshold for Amsy and A0 calc
thres = 0.90
# assessment age structure
dat_stock = as.data.frame(FLQuants(Numbers=stock.n(stk_ass)))

##
# calc Amsy and Pmsy
msy_dat = filter(eqstk, F == "Fmsy") # extract age structure at Fmsy
msy_dat = filter(msy_dat, !age == min(msy_dat$age)) # remove first age class to limit impact of incoming recruitment

msy_thres = sum(msy_dat$data)*thres # calc n at 90 percentile

msy_dat$cumsum = cumsum(msy_dat$data) # cumsum
Amsy = filter(msy_dat, cumsum == Closest(msy_dat$cumsum, msy_thres))$age # extract age closest to 90% percentile

if(Amsy == max(msy_dat$age)){ # catch to ensure that Amsy != Amax
  Amsy = Amsy-1
}

msy_old <- filter(msy_dat, age > Amsy) # filter for above Amsy
Pmsy <- sum(msy_old$data)/sum(msy_dat$data) # calc proportion above Amsy 
Nmsy <- sum(msy_old$data) # absolute number above Amsy in reference case

##
# calc A0 and P0
NF_dat = filter(eqstk, F == "F0") # extract age structure at F0
NF_dat = filter(NF_dat, !age == min(NF_dat$age)) # remove first age class to limit impact of incoming recruitment

NF_thres = sum(NF_dat$data)*thres # calc n at 90 percentile

NF_dat$cumsum = cumsum(NF_dat$data) # cumsum
A0 = filter(NF_dat, cumsum == Closest(NF_dat$cumsum, NF_thres))$age # extract age closest to 90% percentile

if(A0 == max(NF_dat$age)){ # catch to ensure that A0 != Amax
  A0 = A0-1
}

NF_old <- filter(NF_dat, age > A0) # filter for above A0
P0 <- sum(NF_old$data)/sum(NF_dat$data) # calc proportion above Amsy 
N0 <- sum(NF_old$data) # absolute number above A0 in reference case

##
# build stock specific indicator data frame
ind = data.frame(matrix(NA, ncol=14, nrow=NROW(unique(dat_stock$year))))
colnames(ind) = c('stock', 'year', 'Amsy', 'A0', 'Pmsy', 'P0', 'Nmsy', 'N0', 'Ptmsy', 'Pt0', 'ABImsy', 'ABI0', 'Ntmsy', 'Nt0')

##
# add fixed variables
ind$stock = eqstk$stock[1]
ind$year = unique(dat_stock$year)
ind$Amsy = Amsy
ind$A0 = A0
ind$Pmsy = Pmsy
ind$P0 = P0

ind$Nmsy = Nmsy
ind$N0 = N0

##
# loop over years and calc ABImsy and ABI0
for(i in 1:NROW(unique(dat_stock$year))){
  year_dat = filter(dat_stock, year == unique(dat_stock$year)[i]) # filter by year
  year_dat = filter(year_dat, !age == min(year_dat$age)) # remove first age class to limit impact of incoming recruitment
  
  year_thres = sum(year_dat$data)*thres # calc n at 90 percentile
  year_dat$cumsum = cumsum(year_dat$data) # cumsum
  
  year_msy = filter(year_dat, age > Amsy) # filter for above Amsy
  year_NF = filter(year_dat, age > A0) # filter for above A0
  
  prop_msy = sum(year_msy$data)/sum(year_dat$data) # proportion above Amsy
  prop_NF = sum(year_NF$data)/sum(year_dat$data) # proportion above A0
  
  ABImsy = prop_msy/Pmsy # calc ABImsy
  ABI0 = prop_NF/P0 # calc A0
  
  ind$Ptmsy[i] = prop_msy # store proportion above Amsy
  ind$Pt0[i] = prop_NF # store proportion above A0
  
  ind$ABImsy[i] = ABImsy # store ABImsy
  ind$ABI0[i] = ABI0 # store ABI0
  
  ind$Ntmsy[i] = sum(year_msy$data) # absolute numbers above Amsy at year t
  ind$Nt0[i] = sum(year_NF$data) # absolute numbers above A0 at year t
  
}

##
# add SSB, F and R
ind$SSB = as.data.frame(FLQuants(ssb=ssb(stk_ass)))$data # store SSB
ind$F = as.data.frame(FLQuants(f = fbar(stk_ass)))$data # store F
ind$R = filter(dat_stock, age == min(dat_stock$age))$data # store R (abundance at age in the smallest age bin)

ind

##
# export stock specific indicator data frame
setwd("C:\\Users\\andre\\Documents\\ABI\\Draft paper\\Third step\\Data")
write.csv(ind, file = paste0(ind$stock[1],'_ABI.csv'))

#################################################################################################################
###
# # collate ABI files and save
# setwd("~/Documents/Indicator study/Application to MED and US stocks/US data/ABI run/ABI data")
# ls = list.files()
# dat <- do.call(rbind,lapply(ls, function(x) read.csv(x,header=T, sep=',')))
# setwd("~/Documents/Indicator study/Application to MED and US stocks/US data/ABI run")
# write.csv(dat, 'US_ABI_data.csv')

#################################################################################################################
###
require(ggpubr)
# plot
setwd("C:/Users/andre/Documents/ABI/Draft paper/Third step/Data/US_elaboration")
dat = read.csv('US_ABI_data_new.csv')
ref = read.csv('US_reference_points.csv')

num = NROW(unique(dat$stock)) # vector of stock IDs for looping

#i = 1

for(i in 1:num){
  
  out_dat = filter(dat, stock == unique(dat$stock)[i])
  out_ref = filter(ref, stock == out_dat$stock[1])
  
  #out_dat$loessMSY <- predict(loess(ABImsy ~ year, data=out_dat, span=0.5))
  #out_dat$loessNF <- predict(loess(ABI0 ~ year, data=out_dat, span=0.5))
  
  # plots
  p1 = ggplot(data = filter(out_dat, year %in% c(max(year-30):max(year))), aes(x = year, y = log(ABImsy)))+ # ABImsy
    geom_point(fill='firebrick3', size = 3, shape = 21)+
    #geom_line(aes(x = year, y = log(loessMSY)),col='firebrick3', size = 1.0)+
    theme_bw()+
    xlab('Year')+
    ylab(expression(paste('log(AB',I[MSY],')')))+
    scale_x_continuous(breaks=seq(min(out_dat$year),max(out_dat$year)+1,2))+
    geom_hline(yintercept=log(1), col='black', size=1.0, linetype = 'dashed')+
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          plot.title = element_text(hjust = 0.5),
          text = element_text(size = 14))
  
  p2 = ggplot(data = filter(out_dat, year %in% c(max(year-30):max(year))), aes(x = year, y = log(ABI0)))+ # ABI0
    geom_point(fill='royalblue1', size = 3, shape = 21)+
    #geom_line(aes(x = year, y = log(loessNF)),col='royalblue1', size = 1.0)+
    theme_bw()+
    xlab('Year')+
    ylab(expression(paste('log(AB',I[0],')')))+
    scale_x_continuous(breaks=seq(min(out_dat$year),max(out_dat$year)+1,2))+
    geom_hline(yintercept=log(1), col='black', size=1.0, linetype = 'dashed')+
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          plot.title = element_text(hjust = 0.5),
          text = element_text(size = 14))
  
  p3 = ggplot(data = filter(out_dat, year %in% c(max(year-30):max(year))), aes(x = year, y = SSB))+
    geom_bar(stat="identity", alpha=0.3, fill = 'lightseagreen', col='black')+
    theme_bw()+
    xlab('Year')+
    ylab('Estimated SSB')+
    geom_hline(yintercept=out_ref$Blim, col = 'red', linetype = 'dashed', size=1.0)+
    geom_hline(yintercept=out_ref$Bmsy, col = 'blue', linetype = 'dashed', size=1.0)+
    scale_x_continuous(breaks=seq(min(out_dat$year),max(out_dat$year)+1,2))+
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          text = element_text(size = 14))
  
  p4 = ggplot(data = filter(out_dat, year %in% c(max(year-30):max(year))), aes(x = year, y = F))+ # F 
    geom_point(fill='gray60', size = 3, shape = 21)+
    geom_line(color= "gray20", size = 1.0)+
    theme_bw()+
    xlab('Year')+
    ylab(paste0('F'))+
    geom_hline(yintercept=out_ref$Ftarget, col = 'blue', linetype = 'dashed', size=1.0)+
    scale_x_continuous(breaks=seq(min(out_dat$year),max(out_dat$year)+1,2))+
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          text = element_text(size = 14))
  
  p5 = ggplot(data = filter(out_dat, year %in% c(max(year-30):max(year))), aes(x = year, y = R))+ 
    geom_bar(stat="identity", fill= "mediumblue", alpha=0.3, col='black')+
    theme_bw()+
    xlab('Year')+
    ylab(paste0('Recruitment'))+
    scale_x_continuous(breaks=seq(min(out_dat$year),max(out_dat$year)+1,2))+
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          text = element_text(size = 14))
  
  ###
  # merge plots and export
  p = (p1 + p2) /(p3 + p4 + p5)
  
  setwd("C:\\Users\\andre\\Documents\\ABI\\Draft paper\\Third step\\Data\\US_elaboration\\plots")
  ggsave(p, filename = paste0('stock_plot_',out_dat$stock[1],'.png'), width = 12, height = 11)
  
  #print(paste0(out_dat$stock[1],', no = ',i))
}
