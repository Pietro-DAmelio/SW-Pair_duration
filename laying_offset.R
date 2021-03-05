#Aim: Potential reasons for increased breeding success in case of long term monogamy: do already established pair breed later?
#Author: P. D'Amelio



# x= pair duration in days at the time of the last breeding in the season
# y= days to the last breeding attempt of the year for the population

# One value per pair per year.


### libraries  ####
library(brms)
library(loo)
library(sjPlot)
library(ggplot2)



##LONGITUDINAL database  #####


#load database
dat<- read.csv("YOUR_DIRECTORY/Longitudinal_dataset.csv")


#CHOOSE natural OR protected colonies

#subset for natural colonies
dat_subset<-dat[,c( "days_to_last_brood","breeding_exp_pair_days_season"  , 
                    "Pair", "Season", "predation", "breeding_exp_Mom_days_season", "breeding_exp_Dad_days_season",
                    "Col_Size", "Min_Age_Mom_season_days", "Min_Age_Dad_season_days", "Colony",
                    "BreederMom", "BreederDad", "season_length", "season_length_population")] 
dat_subset<-subset(dat_subset, predation=="natural")



#subset for protected colonies
dat_subset<-dat[,c( "days_to_last_brood","breeding_exp_pair_days_season" , 
                    "Pair", "Season", "predation", "breeding_exp_Mom_days_season", "breeding_exp_Dad_days_season",
                    "Col_Size", "Min_Age_Mom_season_days", "Min_Age_Dad_season_days", "Colony",
                    "BreederMom", "BreederDad", "season_length", "season_of_protection", "season_length_population")] 
dat_subset<-subset(dat_subset, predation=="protected")


# after selection of colony protection "dat_subset" follows a common pathway

#remove duplicates
dat_subset<- unique(dat_subset)

#select only complete cases
dat_subset<-dat_subset[complete.cases(dat_subset), ] 

# variables that are factors
dat_subset$Colony<- as.factor(dat_subset$Colony)



#Scale continuos variables
dat_subset$breeding_exp_pair_days_season.z <-scale(dat_subset$breeding_exp_pair_days_season)
dat_subset$breeding_exp_Dad_days_season.z <-scale(dat_subset$breeding_exp_Dad_days_season)
dat_subset$breeding_exp_Mom_days_season.z <-scale(dat_subset$breeding_exp_Mom_days_season)
dat_subset$Min_Age_Mom_season_days.z <-scale(dat_subset$Min_Age_Mom_season_days)
dat_subset$Min_Age_Dad_season_days.z <- scale(dat_subset$Min_Age_Dad_season_days)

dat_subset$Col_Size.z<- scale(dat_subset$Col_Size)
dat_subset$season_length.z <- scale(dat_subset$season_length)
dat_subset$season_length_population.z <- scale(dat_subset$season_length_population)



## Natural conditions
# 2 link functions compared:
# 1- poisson
# 2- negbinomial


#poisson
#### FINAL MODEL  NATURAL LONG####
mod1 = brms::brm(days_to_last_brood ~ breeding_exp_pair_days_season.z+
                   breeding_exp_Dad_days_season.z+
                   breeding_exp_Mom_days_season.z+
                   Min_Age_Mom_season_days.z+
                   Min_Age_Dad_season_days.z+
                   Col_Size.z+
                   (1|Season)  +(breeding_exp_pair_days_season.z|Pair) +
                   (1|Colony)+ (breeding_exp_Dad_days_season.z|BreederDad) + 
                   (breeding_exp_Mom_days_season.z|BreederMom),
                 control = list(adapt_delta = 0.9999, max_treedepth = 17),
                 iter = 8000,
                 cores = 4,
                 #prior = set_prior('normal(0, 3)'),
                 data = dat_subset,
                 family = "poisson",
                 file="breeding_offset2_nat1")
mod1<-add_criterion(mod1, "loo", cores=4)


plot(mod1)
#chains of main population factors mixed well
pp_check(mod1)
#fit is quite good



bayes_R2(mod1)
#     Estimate   Est.Error      Q2.5     Q97.5
# R2 0.9713921 0.002549758 0.9660575 0.9760798



# negbinomial
mod2 = brms::brm(days_to_last_brood ~ breeding_exp_pair_days_season.z+
                   breeding_exp_Dad_days_season.z+
                   breeding_exp_Mom_days_season.z+
                   Min_Age_Mom_season_days.z+
                   Min_Age_Dad_season_days.z+
                   Col_Size.z+
                   (1|Season)  +(breeding_exp_pair_days_season.z|Pair) +
                   (1|Colony)+ (breeding_exp_Dad_days_season.z|BreederDad) + 
                   (breeding_exp_Mom_days_season.z|BreederMom),
                 control = list(adapt_delta = 0.9999, max_treedepth = 14),
                 iter = 8000,
                 cores = 4,
                 #prior = set_prior('normal(0, 3)'),
                 data = dat_subset,
                 family = "poisson",
                 file="breeding_offset2_nat2")
mod2<-add_criterion(mod2, "loo", cores=4)
#Found 14 observations with a pareto_k > 0.7 in model 'mod2'

pp_check(mod2)


loo_compare(mod1, mod2)
#mod1 much better






#### main variable plot - NAT LONG####

#extract Plotting values
marg<-marginal_effects(mod1, resolution=1000)
#select the variable of interest
breeding_offset<-marg$breeding_exp_pair_days_season.z


ggplot()+
  geom_line(data=breeding_offset,aes(x=((sd(dat_subset$breeding_exp_pair_days_season)*
                                           breeding_offset$breeding_exp_pair_days_season.z+
                                      mean(dat_subset$breeding_exp_pair_days_season))/365),
                                y=estimate__, ymax=upper__ , ymin=lower__),
            size=1.3, color="#007373")+
  geom_ribbon(data=breeding_offset,aes(x=((sd(dat_subset$breeding_exp_pair_days_season)*
                                        breeding_offset$breeding_exp_pair_days_season.z+
                                        mean(dat_subset$breeding_exp_pair_days_season))/365),
                                  y=estimate__, ymax=upper__, ymin=lower__),
              fill="#007373",alpha=0.3)+
  geom_point(data=dat_subset,aes(x=((sd(dat_subset$breeding_exp_pair_days_season)*
                                       dat_subset$breeding_exp_pair_days_season.z+
                                       mean(dat_subset$breeding_exp_pair_days_season))/365),
                                 y=days_to_last_brood), 
             color="#007373",alpha=0.1) + 
  ylim(0, 315)+
  xlim(0,4.15)+
  xlab(element_blank())+
  ylab(element_blank())+
  theme(text = element_text(size=15))+
  #theme(legend.position = c(0.9, 0.9))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))

# ggsave("breeding_offset_exp_nat1_v2.eps", device=cairo_ps, width = 8, height = 8,
#        units ="cm", fallback_resolution = 600)



####Forest Plot - NAT LONG ####

plot_model(mod1,type="std2", vline.color = "#007373", line.size = .35, 
           sort.est = TRUE, transform = NULL, show.values = TRUE,
           ci.style="whisker", value.size=1.9,
           prob.inner=0,prob.outer=.95, width=0.2, 
           bpe.style = "dot", bpe.color = "black")+  
  scale_color_sjplot("simply")+ 
  ylim(-0.6, 0.6)+ 
  theme(text=element_blank())+
  theme(panel.grid = element_blank())+
  theme(panel.background = element_blank())+
  theme(panel.background = element_rect(color = NA)) + #basic theme
  theme( plot.title = element_blank())+
  theme(axis.text.y=element_blank(),axis.ticks.y = element_blank())+    #remove the label
  theme(axis.text.x=element_blank()) +
  theme(axis.line.x = element_line())








## protected conditions
# 2 link functions compared:
# 1- poisson
# 2- negbinomial


#poisson
#### FINAL MODEL PROTECTED LONG####
mod1 = brms::brm(days_to_last_brood ~ breeding_exp_pair_days_season.z+
                   breeding_exp_Dad_days_season.z+
                   breeding_exp_Mom_days_season.z+
                   Min_Age_Mom_season_days.z+
                   Min_Age_Dad_season_days.z+
                   Col_Size.z+
                   mo(season_of_protection)+
                   (1|Season)  +(breeding_exp_pair_days_season.z|Pair) +
                   (1|Colony)+ (breeding_exp_Dad_days_season.z|BreederDad) + 
                   (breeding_exp_Mom_days_season.z|BreederMom),
                 control = list(adapt_delta = 0.999, max_treedepth = 14),
                 iter = 8000,
                 cores = 4,
                 #prior = set_prior('normal(0, 3)'),
                 data = dat_subset,
                 family = "poisson",
                 file="breeding_offset2_prot1") 
mod1<-add_criterion(mod1, "loo", cores=4)#, reloo = TRUE
#Found 299 observations with a pareto_k > 0.7 in model 'mod1'.
#very odd


plot(mod1)
#chains of main population factors mixed well
pp_check(mod1)
#very good fitting

bayes_R2(mod1)
# Estimate   Est.Error      Q2.5     Q97.5
# R2 0.9537033 0.003573801 0.9461733 0.9602599





# negative binomial
mod2 = brms::brm(days_to_last_brood ~ breeding_exp_pair_days_season.z+
                   breeding_exp_Dad_days_season.z+
                   breeding_exp_Mom_days_season.z+
                   Min_Age_Mom_season_days.z+
                   Min_Age_Dad_season_days.z+
                   Col_Size.z+
                   mo(season_of_protection)+
                   (1|Season)  +(breeding_exp_pair_days_season.z|Pair) +
                   (1|Colony)+ (breeding_exp_Dad_days_season.z|BreederDad) + 
                   (breeding_exp_Mom_days_season.z|BreederMom),
                 control = list(adapt_delta = 0.999, max_treedepth = 14),
                 iter = 8000,
                 cores = 4,
                 #prior = set_prior('normal(0, 3)'),
                 data = dat_subset,
                 family = "negbinomial",
                 file="breeding_offset2_prot2") 
mod2<-add_criterion(mod2, "loo", cores=4)#, reloo = TRUE
#Found 10 observations with a pareto_k > 0.7 in model 'mod2'


plot(mod2)
#chains of main population factors mixed well

pp_check(mod2)
#good

bayes_R2(mod2)
# Estimate  Est.Error      Q2.5     Q97.5
# R2 0.3313135 0.06603881 0.2070646 0.4676969


loo_compare(mod1,mod2)
# huge overlap




#### main variable plot - PROT - LONG. ####

#extract Plotting values
marg<-marginal_effects(mod1, resolution=1000)
#select the variable of interest
breeding_offset<-marg$breeding_exp_pair_days_season.z

#plot

ggplot()+
  geom_line(data=breeding_offset,aes(x=((sd(dat_subset$breeding_exp_pair_days_season)*
                                           breeding_offset$breeding_exp_pair_days_season.z+
                                           mean(dat_subset$breeding_exp_pair_days_season))/365),
                                     y=estimate__, ymax=upper__ , ymin=lower__),
            size=1.3, color="#D55E00")+
  geom_ribbon(data=breeding_offset,aes(x=((sd(dat_subset$breeding_exp_pair_days_season)*
                                             breeding_offset$breeding_exp_pair_days_season.z+
                                             mean(dat_subset$breeding_exp_pair_days_season))/365),
                                       y=estimate__, ymax=upper__, ymin=lower__),
              fill="#D55E00",alpha=0.3)+
  geom_point(data=dat_subset,aes(x=((sd(dat_subset$breeding_exp_pair_days_season)*
                                       dat_subset$breeding_exp_pair_days_season.z+
                                       mean(dat_subset$breeding_exp_pair_days_season))/365),
                                 y=days_to_last_brood), 
             color="#D55E00",alpha=0.1) + 
  ylim(0, 315)+
  xlim(0,4.15)+
  xlab(element_blank())+
  ylab(element_blank())+
  theme(text = element_text(size=15))+
  #theme(legend.position = c(0.9, 0.9))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))

ggsave("breeding_offset_exp_prot1_v2.eps", device=cairo_ps, width = 8, height = 8,
       units ="cm", fallback_resolution = 600)



####Forest Plot - PROT - LONG. ####

plot_model(mod1,type="std2", vline.color = "#D55E00", line.size = .35, 
           sort.est = TRUE, transform = NULL, show.values = TRUE,
           ci.style="whisker", value.size=1.9,
           prob.inner=0,prob.outer=.95, width=0.2, 
           bpe.style = "dot", bpe.color = "black")+  
  scale_color_sjplot("simply")+ 
  ylim(-1, 1)+ 
  theme(text=element_blank())+
  theme(panel.grid = element_blank())+
  theme(panel.background = element_blank())+
  theme(panel.background = element_rect(color = NA)) + #basic theme
  theme( plot.title = element_blank())+
  theme(axis.text.y=element_blank(),axis.ticks.y = element_blank())+    #remove the label
  theme(axis.text.x=element_blank()) +
  theme(axis.line.x = element_line())

## dimensions: 130 - 180






##CROSS-SECTIONAL database  #####

#Load database

dat3<- read.csv("YOUR_DIRECTORY/CrossSectional_dataset_season.csv")



###Breeding offset 

#natural
dat3_season<-dat3[,c("days_to_last_brood", "breeding_exp_pair_days_season", "predation",
                            "sex", "Season", "breeding_exp_Mate_days_season", "Colony", "Col_Size",
                            "Min_Age_Mate_season_days", "Min_Age_Focal_season_days", 
                            "Pair", "ring", "Mate_ID")]
dat3_season<-subset(dat3_season, predation=="natural")



#protected

dat3_season<-dat3[,c("days_to_last_brood", "breeding_exp_pair_days_season", "predation",
                            "sex", "Season", "breeding_exp_Mate_days_season", "Colony", "Col_Size",
                            "Min_Age_Mate_season_days", "Min_Age_Focal_season_days", "season_of_protection",
                            "Pair", "ring", "Mate_ID")]
dat3_season<-subset(dat3_season, predation=="protected")


#remove NAs, duplicates
dat3_season<-unique(dat3_season)

#select only complete cases
dat3_season<-dat3_season[complete.cases(dat3_season), ] 



#variable tranformation
dat3_season$Colony<- as.factor(dat3_season$Colony)
dat3_season$sex<- as.factor(dat3_season$sex)



#scaling
dat3_season$breeding_exp_Mate_days_season.z<- scale(dat3_season$breeding_exp_Mate_days_season)
dat3_season$breeding_exp_pair_days_season.z<- scale(dat3_season$breeding_exp_pair_days_season)
dat3_season$Min_Age_Focal_season_days.z<- scale(dat3_season$Min_Age_Focal_season_days)
dat3_season$Min_Age_Mate_season_days.z<- scale(dat3_season$Min_Age_Mate_season_days)
dat3_season$Col_Size.z<- scale(dat3_season$Col_Size)


## Natural conditions
# 2 link functions compared:
# 1- poisson
# 2- negbinomial




#### FINAL MODEL NATURAL - CROSS. ####
mod1 = brms::brm(days_to_last_brood ~ breeding_exp_pair_days_season.z+
                   breeding_exp_Mate_days_season.z+
                   Min_Age_Mate_season_days.z+
                   Min_Age_Focal_season_days.z+
                   Col_Size.z+
                   sex+
                   (1|Season)  +(1|Pair) +
                   (1|Colony),
                 control = list(adapt_delta = 0.9999, max_treedepth = 14),
                 iter = 8000,
                 cores = 4,
                 #prior = set_prior('normal(0, 3)'),
                 data = dat3_season,
                 family = "poisson",
                 file="breeding_offset2_3yrs_nat1")
mod1<-add_loo(mod1)
#Found 46 observations with a pareto_k > 0.7 in model 'mod1'

plot(mod1)

pp_check(mod1)
#quite good fit!

bayes_R2(mod1)
#    Estimate   Est.Error      Q2.5     Q97.5
# R2 0.949254 0.008890021 0.9293042 0.9641979


#negbinomial
mod2 = brms::brm(days_to_last_brood ~ breeding_exp_pair_days_season.z+
                   breeding_exp_Mate_days_season.z+
                   Min_Age_Mate_season_days.z+
                   Min_Age_Focal_season_days.z+
                   Col_Size.z+
                   sex+
                   (1|Season)  +(1|Pair) +
                   (1|Colony),#+(1|Mate_ID),
                 control = list(adapt_delta = 0.9999, max_treedepth = 14),
                 iter = 8000,
                 cores = 4,
                 #prior = set_prior('normal(0, 3)'),
                 data = dat3_season,
                 family = "negbinomial",
                 file="breeding_offset2_3yrs_nat2")
#There were 1 divergent transitions after warmup.
mod2<-add_loo(mod2)
#Found 40 observations with a pareto_k > 0.7 in model 'mod2'

loo_compare(mod1,mod2)
#mod1 much better

#### main variable plot - NAT - CROSS. ####

#extract Plotting values
marg<-marginal_effects(mod1, resolution=1000)
#select the variable of interest
breeding_offset<-marg$breeding_exp_pair_days_season.z

#plot
ggplot()+  
  geom_line(data = breeding_offset,
            aes(x=((sd(dat3_season$breeding_exp_pair_days_season)*
                      breeding_offset$breeding_exp_pair_days_season.z+
                      mean(dat3_season$breeding_exp_pair_days_season)+1)/365),
                y=estimate__, ymax=upper__ , ymin=lower__),size=1.3, color="#007373")+
  geom_ribbon(data = breeding_offset,
              aes(x=((sd(dat3_season$breeding_exp_pair_days_season)*
                        breeding_offset$breeding_exp_pair_days_season.z+
                        mean(dat3_season$breeding_exp_pair_days_season)+1)/365),
                  y=estimate__, ymax=upper__, ymin=lower__),fill="#007373",alpha=0.3)+
  geom_point(data = dat3_season,
             aes(x=((sd(dat3_season$breeding_exp_pair_days_season)*
                       dat3_season$breeding_exp_pair_days_season.z+
                       mean(dat3_season$breeding_exp_pair_days_season)+1)/365),
                 y=days_to_last_brood), 
             color="#007373",alpha=0.1)+
  ylim(0, 280)+
  xlim(0,2.7)+
  xlab(element_blank())+
  ylab(element_blank())+
  theme(text = element_text(size=15)) +
  #theme(legend.position = c(0.9, 0.9))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))



ggsave("breeding_offset_exp_nat1_3y_v1.eps", device=cairo_ps, width = 8, height = 8,
       units ="cm", fallback_resolution = 600)





####Forest Plot - NAT. - CROSS. ####

plot_model(mod1,type="std2", vline.color = "#007373", line.size = .35, 
           sort.est = TRUE, transform = NULL, show.values = TRUE,
           ci.style="whisker", value.size=1.9,
           prob.inner=0,prob.outer=.95, width=0.2, 
           bpe.style = "dot", bpe.color = "black")+  
  scale_color_sjplot("simply")+ 
  ylim(-1.2, .6)+ 
  theme(text=element_blank())+
  theme(panel.grid = element_blank())+
  theme(panel.background = element_blank())+
  theme(panel.background = element_rect(color = NA)) + #basic theme
  theme( plot.title = element_blank())+
  theme(axis.text.y=element_blank(),axis.ticks.y = element_blank())+    #remove the label
  theme(axis.text.x=element_blank()) +
  theme(axis.line.x = element_line())

## dimensions: 130 - 180





## Protected conditions
# 2 link functions compared:
# 1- poisson
# 2- negbinomial





#### FINAL MODEL PROTECTED CROSS ####
mod1 = brms::brm(days_to_last_brood ~ breeding_exp_pair_days_season.z+
                   breeding_exp_Mate_days_season.z+
                   Min_Age_Mate_season_days.z+
                   Min_Age_Focal_season_days.z+
                   Col_Size.z+
                   #sex+
                   mo(season_of_protection)+
                   (1|Season)  +(1|Pair) +
                   (1|Colony),#+(1|Mate_ID),
                 control = list(adapt_delta = 0.9999, max_treedepth = 15),
                 iter = 8000,
                 cores = 4,
                 #prior = set_prior('normal(0, 3)'),
                 data = dat3_season,
                 family = "poisson",
                 file="breeding_offset2_3yrs_prot1")

mod1<-add_loo(mod1)
#Found 42 observations with a pareto_k > 0.7 in model 'mod1'

plot(mod1)

pp_check(mod1)
#good

bayes_R2(mod1)
# Estimate  Est.Error      Q2.5     Q97.5
# R2 0.9801483 0.00441914 0.9703008 0.9874971


#negative binomial
mod2 = brms::brm(days_to_last_brood ~ breeding_exp_pair_days_season.z+
                   breeding_exp_Mate_days_season.z+
                   Min_Age_Mate_season_days.z+
                   Min_Age_Focal_season_days.z+
                   Col_Size.z+
                   sex+
                   mo(season_of_protection)+
                   (1|Season)  +(1|Pair) +
                   (1|Colony),#+(1|Mate_ID),
                 control = list(adapt_delta = 0.9999, max_treedepth = 12),
                 iter = 8000,
                 cores = 4,
                 #prior = set_prior('normal(0, 3)'),
                 data = dat3_season,
                 family = "negbinomial",
                 file="breeding_offset2_3yrs_prot2")
# There were 5572 transitions after warmup that exceeded the maximum treedepth
mod2<-add_loo(mod2)


loo_compare(mod1, mod2)
#mod1 better




#### main variable plot - PROT - CROSS. ####

#extract Plotting values
marg<-marginal_effects(mod1, resolution=1000)
#select the variable of interest
breeding_offset<-marg$breeding_exp_pair_days_season.z

#plot

ggplot()+  
  geom_line(data = breeding_offset,
            aes(x=((sd(dat3_season$breeding_exp_pair_days_season)*
                      breeding_offset$breeding_exp_pair_days_season.z+
                      mean(dat3_season$breeding_exp_pair_days_season)+1)/365),
                y=estimate__, ymax=upper__ , ymin=lower__),size=1.3, color="#D55E00")+
  geom_ribbon(data = breeding_offset,
              aes(x=((sd(dat3_season$breeding_exp_pair_days_season)*
                        breeding_offset$breeding_exp_pair_days_season.z+
                        mean(dat3_season$breeding_exp_pair_days_season)+1)/365),
                  y=estimate__, ymax=upper__, ymin=lower__),fill="#D55E00",alpha=0.3)+
  geom_point(data = dat3_season,
             aes(x=((sd(dat3_season$breeding_exp_pair_days_season)*
                       dat3_season$breeding_exp_pair_days_season.z+
                       mean(dat3_season$breeding_exp_pair_days_season)+1)/365),
                 y=days_to_last_brood), 
             color="#D55E00",alpha=0.1)+
  ylim(0, 280)+
  xlim(0,2.7)+
  xlab(element_blank())+
  ylab(element_blank())+
  theme(text = element_text(size=15)) +
  #theme(legend.position = c(0.9, 0.9))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))



ggsave("breeding_offset_exp_prot1_3y_v1.eps", device=cairo_ps, width = 8, height = 8,
       units ="cm", fallback_resolution = 600)







####Forest Plot - PROT. - CROSS. ####

plot_model(mod1,type="std2", vline.color = "#D55E00", line.size = .35, 
           sort.est = TRUE, transform = NULL, show.values = TRUE,
           ci.style="whisker", value.size=1.9,
           prob.inner=0,prob.outer=.95, width=0.2, 
           bpe.style = "dot", bpe.color = "black")+  
  scale_color_sjplot("simply")+ 
  ylim(-.6, .6)+ 
  theme(text=element_blank())+
  theme(panel.grid = element_blank())+
  theme(panel.background = element_blank())+
  theme(panel.background = element_rect(color = NA)) + #basic theme
  theme( plot.title = element_blank())+
  theme(axis.text.y=element_blank(),axis.ticks.y = element_blank())+    #remove the label
  theme(axis.text.x=element_blank()) +
  theme(axis.line.x = element_line())

## dimensions: 130 - 180


