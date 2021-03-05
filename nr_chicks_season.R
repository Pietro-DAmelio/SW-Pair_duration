#Aim:  study if the number of chicks in a season is influenced by pair duration (while accounting for other covariates)
#Author: P. D'Amelio

### libraries  ####
library(brms)
library(loo)
library(sjPlot)
library(ggplot2)



##LONGITUDINAL database  #####


#load database
dat<- read.csv("YOUR_DIRECTORY/Longitudinal_dataset.csv")


#NATURAL
dat_subset<-dat[,c( "tot_fitness", "breeding_exp_pair_nogaps" , "season_length_population",
                    "Pair", "Season", "predation", "breeding_exp_Mom_nogaps", "breeding_exp_Dad_nogaps",
                    "Col_Size", "Mom_min_Seasons", "Dad_min_Seasons", "Colony",
                    "BreederMom", "BreederDad", "season_length")] 
dat_subset<-subset(dat_subset, predation=="natural")


#PROTECTED

#use this for protected colonies
dat_subset<-dat[,c( "tot_fitness", "breeding_exp_pair_nogaps" , "season_length_population",
                    "Pair", "Season", "predation", "breeding_exp_Mom_nogaps", "breeding_exp_Dad_nogaps",
                    "Col_Size", "Mom_min_Seasons", "Dad_min_Seasons", "Colony",
                    "BreederMom", "BreederDad", "season_length", "season_of_protection")] 
dat_subset<-subset(dat_subset, predation=="protected")




#from here run for both natural and protected
#delete duplicates
dat_subset<- unique(dat_subset)

#select only complete cases
dat_subset<-dat_subset[complete.cases(dat_subset), ] 

# variables that are factors
dat_subset$Colony<- as.factor(dat_subset$Colony)


#Scale continuos variables
dat_subset$breeding_exp_pair_nogaps.z <-scale(dat_subset$breeding_exp_pair_nogaps)
dat_subset$breeding_exp_Dad_nogaps.z <-scale(dat_subset$breeding_exp_Dad_nogaps)
dat_subset$breeding_exp_Mom_nogaps.z <-scale(dat_subset$breeding_exp_Mom_nogaps)
dat_subset$Mom_min_Seasons.z <-scale(dat_subset$Mom_min_Seasons)
dat_subset$Dad_min_Seasons.z <- scale(dat_subset$Dad_min_Seasons)

dat_subset$Col_Size.z<- scale(dat_subset$Col_Size)
dat_subset$season_length_population.z<- scale(dat_subset$season_length_population)



#NATURAL



#### FINAL MODEL NATURAL - LONG. ####
mod1 = brms::brm(tot_fitness ~  breeding_exp_pair_nogaps.z +
                   breeding_exp_Dad_nogaps.z+
                   breeding_exp_Mom_nogaps.z+
                   Mom_min_Seasons.z+
                   Dad_min_Seasons.z+
                   season_length_population.z+
                   Col_Size.z+
                   (1|Season)  +(breeding_exp_pair_nogaps.z|Pair) +
                   (1|Colony)+ (breeding_exp_Dad_nogaps.z|BreederDad) + 
                   (breeding_exp_Mom_nogaps.z|BreederMom),
                 control = list(adapt_delta = 0.999, max_treedepth = 15),
                 iter = 8000,
                 cores = 4,
                 #prior = set_prior('normal(0, 3)'),
                 data = dat_subset,
                 family = "zero_inflated_poisson",
                 file="tot_fitness_seasons_nat2")
plot(mod1)

pp_check(mod1)
#ok


bayes_R2(mod1)
#   Estimate  Est.Error       Q2.5     Q97.5
# R2 0.1568071 0.04501183 0.08001205 0.2556427





#### main variable plot - NAT - LONG. ####

#extract Plotting values
marg<-marginal_effects(mod1, resolution=1000)

#select the variable of interest
pair_duration<-marg$breeding_exp_pair_nogaps.z

dat_subset <- within(dat_subset, Count <- ave(tot_fitness, list(tot_fitness, breeding_exp_pair_nogaps), FUN=length))  

#plot


ggplot()+
  geom_line(data = pair_duration, aes(x=sd(dat_subset$breeding_exp_pair_nogaps)*pair_duration$breeding_exp_pair_nogaps.z+mean(dat_subset$breeding_exp_pair_nogaps),y=estimate__, ymax=upper__ , ymin=lower__),size=1.3, color="#007373")+
  geom_ribbon(data = pair_duration, aes(x=sd(dat_subset$breeding_exp_pair_nogaps)*pair_duration$breeding_exp_pair_nogaps.z+mean(dat_subset$breeding_exp_pair_nogaps),y=estimate__, ymax=upper__, ymin=lower__),fill="#007373",alpha=0.3)+
  geom_point(data = dat_subset, aes(size = Count,x=sd(dat_subset$breeding_exp_pair_nogaps)*dat_subset$breeding_exp_pair_nogaps.z+mean(dat_subset$breeding_exp_pair_nogaps),y=tot_fitness), color="#007373",alpha=0.15) +
  ylim(0, 11)+
  # xlim(1,5)+
  xlab(element_blank())+
  ylab(element_blank())+
  theme(text = element_text(size=20)) +
  theme(legend.title = element_text(size = 13),
        legend.text = element_text(size = 13))+
  #theme(legend.position = c(0.9, 0.9))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))


ggsave("tot_fit_pair_durat_nat1_v7.eps", device=cairo_ps, width = 10, height = 9,
       units ="cm", fallback_resolution = 600)





####Forest Plot - NAT - LONG. ####
plot_model(mod1,type="std2", vline.color = "#007373", line.size = .35, 
           sort.est = TRUE, transform = NULL, show.values = TRUE,
           ci.style="whisker", value.size=1.9,
           prob.inner=0,prob.outer=.95, width=0.2, 
           bpe.style = "dot", bpe.color = "black")+  
  #scale_color_sjplot("simply")+ 
  ylim(-.4, .45)+ 
  theme(text=element_blank())+
  theme(panel.grid = element_blank())+
  theme(panel.background = element_blank())+
  theme(panel.background = element_rect(color = NA)) + #basic theme
  theme( plot.title = element_blank())+
  theme(axis.text.y=element_blank(),axis.ticks.y = element_blank())+    #remove the label
  theme(axis.text.x=element_blank()) +
  theme(axis.line.x = element_line())

#
## dimension: 130 - 180







#PROTECTED

## Protected conditions
# I compared 2 models
# I removed "season_length_population" because its credible intervals are huge
# 





#### FINAL MODEL PROTECTED - LONG. ####

mod3 = brms::brm(tot_fitness ~ breeding_exp_pair_nogaps.z +
                   breeding_exp_Dad_nogaps.z+
                   breeding_exp_Mom_nogaps.z+
                   Mom_min_Seasons.z+
                   Dad_min_Seasons.z+
                   season_length_population.z+
                   Col_Size.z+
                   mo(season_of_protection)+
                   (1|Season) + (1|Colony)+
                   (breeding_exp_pair_nogaps.z|Pair)+
                   (breeding_exp_Dad_nogaps.z|BreederDad) + 
                   (breeding_exp_Mom_nogaps.z|BreederMom),
                 control = list(adapt_delta = 0.999, max_treedepth = 12),
                 iter = 8000,
                 cores = 4,
                 #prior = set_prior('normal(0, 3)'),
                 data = dat_subset,
                 family = "zero_inflated_poisson",
                 file="tot_fitness_seasons_prot3")

plot(mod3)

pp_check(mod3)
#ok, given the strange distribution of N of chicks per season


bayes_R2(mod3)
#     Estimate  Est.Error       Q2.5     Q97.5
#    0.3384008 0.05560267  0.2292409 0.4460942





#remove season length (huge variance)
mod2 = brms::brm(tot_fitness ~ breeding_exp_pair_nogaps.z +
                   breeding_exp_Dad_nogaps.z+
                   breeding_exp_Mom_nogaps.z+
                   Mom_min_Seasons.z+
                   Dad_min_Seasons.z+
                   #season_length_population.z+
                   Col_Size.z+
                   mo(season_of_protection)+
                   (1|Season) + (1|Colony)+
                   (breeding_exp_pair_nogaps.z|Pair)+
                   (breeding_exp_Dad_nogaps.z|BreederDad) + 
                   (breeding_exp_Mom_nogaps.z|BreederMom),
                 control = list(adapt_delta = 0.999, max_treedepth = 12),
                 iter = 8000,
                 cores = 4,
                 #prior = set_prior('normal(0, 3)'),
                 data = dat_subset,
                 family = "zero_inflated_poisson",
                 file="tot_fitness_seasons_prot2")
mod2<-add_criterion(mod2, "loo", cores=4)
#Found 3 observations with a pareto_k > 0.7 in model 'mod2'.

loo_compare(mod1, mod2)
#mod 1 better






#### main variable plot - PROT. - LONG. ####

#extract Plotting values
marg<-marginal_effects(mod3, resolution=1000)


#select the variable of interest
pair_duration<-marg$breeding_exp_pair_nogaps.z

dat_subset <- within(dat_subset, Count <- ave(tot_fitness, list(tot_fitness, breeding_exp_pair_nogaps), FUN=length))  

#plot

ggplot()+
 geom_line(data=pair_duration,aes(x=sd(dat_subset$breeding_exp_pair_nogaps)*
           pair_duration$breeding_exp_pair_nogaps.z+mean(dat_subset$breeding_exp_pair_nogaps),
           y=estimate__, ymax=upper__ , ymin=lower__),
           size=1.3, color="#D55E00")+
 geom_ribbon(data=pair_duration,aes(x=sd(dat_subset$breeding_exp_pair_nogaps)*
             pair_duration$breeding_exp_pair_nogaps.z+
             mean(dat_subset$breeding_exp_pair_nogaps),
             y=estimate__, ymax=upper__, ymin=lower__),
             fill="#D55E00",alpha=0.3)+
 geom_point(data=dat_subset,aes(size=Count,x=sd(dat_subset$breeding_exp_pair_nogaps)*
            dat_subset$breeding_exp_pair_nogaps.z+
            mean(dat_subset$breeding_exp_pair_nogaps),
            y=tot_fitness), color="#D55E00",alpha=0.15) +
 ylim(0, 11)+
 #   xlim(1,5)+
 xlab(element_blank())+
 ylab(element_blank())+
 theme(text = element_text(size=20)) +
 theme(legend.title = element_text(size = 13),
       legend.text = element_text(size = 13))+
 #theme(legend.position = c(0.9, 0.9))+
 theme(panel.grid.major = element_blank(), 
       panel.grid.minor = element_blank(),
       panel.background = element_blank(), 
       axis.line = element_line(colour = "black"))



ggsave("tot_fit_pair_durat_prot1_v6.eps", device=cairo_ps, width = 10, height = 9,
       units ="cm", fallback_resolution = 600)



####Forest Plot - PROT. - LONG. ####
plot_model(mod3,type="std2", vline.color = "#D55E00", line.size = .35, 
           sort.est = TRUE, transform = NULL, show.values = TRUE,
           ci.style="whisker", value.size=1.9,
           prob.inner=0,prob.outer=.95, width=0.2, 
           bpe.style = "dot",  bpe.color = "black",
           rm.terms = c("simo_moseason_of_protection1.1.",
                        "simo_moseason_of_protection1.2.",
                        "simo_moseason_of_protection1.3.",
                        "simo_moseason_of_protection1.4.",
                        "simo_moseason_of_protection1.5."))+  
  #scale_color_sjplot("simply")+ 
  ylim(-.6, .85)+ 
  theme(text=element_blank())+
  theme(panel.grid = element_blank())+
  theme(panel.background = element_blank())+
  theme(panel.background = element_rect(color = NA)) + #basic theme
  theme( plot.title = element_blank())+
  theme(axis.text.y=element_blank(),axis.ticks.y = element_blank())+    #remove the label
  theme(axis.text.x=element_blank()) +
  theme(axis.line.x = element_line())





##CROSS-SECTIONAL database  #####


#Load database

dat3<- read.csv("YOUR_DIRECTORY/CrossSectional_dataset_season.csv")




#NATURAL
dat3_season<-dat3[,c("tot_fitness", "breeding_exp_pair_nogaps", "predation",
                            "sex", "Season", "breeding_exp_Mate_nogaps", "Colony", "Col_Size",
                            "Focal_min_Seasons", "Mate_min_Seasons", 
                            "Pair", "ring", "Mate_ID", "season_length_population", "season_length")]
dat3_season<-subset(dat3_season, predation=="natural")


#PROTECTED
dat3_season<-dat3[,c("tot_fitness", "breeding_exp_pair_nogaps", "predation",
                            "sex", "Season", "breeding_exp_Mate_nogaps", "Colony", "Col_Size",
                            "Focal_min_Seasons", "Mate_min_Seasons",  "season_of_protection",
                            "Pair", "ring", "Mate_ID", "season_length_population", "season_length")]
dat3_season<-subset(dat3_season, predation=="protected")


##Now the databases follow the same path
## Eliminate duplicates #select only complete cases
dat3_season<- unique(dat3_season)

dat3_season<-dat3_season[complete.cases(dat3_season), ] 


# variables that are factors
dat3_season$Colony<- as.factor(dat3_season$Colony)
dat3_season$sex<- as.factor(dat3_season$sex)

#variable tranformation, scaling
dat3_season$breeding_exp_Mate_nogaps.z<- scale(dat3_season$breeding_exp_Mate_nogaps)
dat3_season$breeding_exp_pair_nogaps.z<- scale(dat3_season$breeding_exp_pair_nogaps)
dat3_season$Focal_min_Seasons.z<- scale(dat3_season$Focal_min_Seasons)
dat3_season$Mate_min_Seasons.z<- scale(dat3_season$Mate_min_Seasons)
dat3_season$Col_Size.z<- scale(dat3_season$Col_Size)
dat3_season$season_length_population.z<-scale(dat3_season$season_length_population)
dat3_season$season_length.z <- scale(dat3_season$season_length)




#NATURAL

## Natural conditions
# I compared 3 models
# 2 model families  compared (zero-inflated poisson and poisson)
# best model did not converge (divergent transitions), model simplified
# 
# 1- zero inflated poisson, full
# 2- poisson, full
# 3- poisson, remove mate ID



#zero inflated poisson link function
mod1a = brms::brm(tot_fitness ~  breeding_exp_pair_nogaps.z+
                   breeding_exp_Mate_nogaps.z+
                   Mate_min_Seasons.z+
                   Focal_min_Seasons.z+
                   season_length_population.z+
                   Col_Size.z+
                   sex+
                   (1|Season)  +(1|Pair) +
                   (1|Colony) + (1|Mate_ID),
                 control = list(adapt_delta = 0.999, max_treedepth = 13),
                 iter = 8000,
                 cores = 4,
                 #prior = set_prior('normal(0, 3)'),
                 data = dat3_season,
                 family = "zero_inflated_poisson",
                 file="tot_fit_3yrs_Season_nat1a")
#There were 14 divergent transitions after warmup.

mod1a<-add_criterion(mod1a, "loo", cores=4)
#Found 2 observations with a pareto_k > 0.7 in model 'mod1'

plot(mod1a)

pp_check(mod1a)
#okish fit

#poisson link function
mod1 = brms::brm(tot_fitness ~  breeding_exp_pair_nogaps.z+
                   breeding_exp_Mate_nogaps.z+
                   Mate_min_Seasons.z+
                   Focal_min_Seasons.z+
                   season_length_population.z+
                   Col_Size.z+
                   sex+
                   (1|Season)  +(1|Pair) +
                   (1|Colony)+ (1|Mate_ID),
                 control = list(adapt_delta = 0.9999, max_treedepth = 12),
                 iter = 8000,
                 cores = 4,
                 #prior = set_prior('normal(0, 3)'),
                 data = dat3_season,
                 family = "poisson",
                 file="tot_fit_3yrs_Season_nat1")
#There were 2 divergent transitions after warmup.

plot(mod1)

pp_check(mod1)
#Good fit for value over 2.5, underestimation of value around 2 aand overestimation of low values

mod1<-add_criterion(mod1, "loo", cores=4)
#Found 4 observations with a pareto_k > 0.7 in model 'mod1'

loo_compare(mod1a, mod1)
#poisson link function is a better fit


#### FINAL MODEL - NATURAL - CROSS. ####
#remove mate ID
mod2 = brms::brm(tot_fitness ~  breeding_exp_pair_nogaps.z+
                   breeding_exp_Mate_nogaps.z+
                   Mate_min_Seasons.z+
                   Focal_min_Seasons.z+
                   season_length_population.z+
                   Col_Size.z+
                   sex+
                   (1|Season)  +(1|Pair) +
                   (1|Colony),#+ (1|Mate_ID),
                 control = list(adapt_delta = 0.9999, max_treedepth = 12),
                 iter = 8000,
                 cores = 4,
                 #prior = set_prior('normal(0, 3)'),
                 data = dat3_season,
                 family = "poisson",
                 file="tot_fit_3yrs_Season_nat2")
plot(mod2)

pp_check(mod2)
#similar but better than mod1

mod2<-add_criterion(mod2, "loo", cores=4)
#Found 3 observations with a pareto_k > 0.7 in model 'mod2'

bayes_R2(mod2)
#     Estimate Est.Error      Q2.5     Q97.5
# R2 0.5356679 0.1006724 0.3113654 0.7002337




#### main variable plot - NAT. - CROSS. ####

#extract Plotting values
marg<-marginal_effects(mod2, resolution=1000)
#select the variable of interest
pair_duration<-marg$breeding_exp_pair_nogaps.z

#plot

dat3_season <- within(dat3_season, Count <- ave(tot_fitness, list(tot_fitness, breeding_exp_pair_nogaps), FUN=length))  


ggplot()+
  geom_line(data = pair_duration, aes(x=sd(dat3_season$breeding_exp_pair_nogaps)*pair_duration$breeding_exp_pair_nogaps.z+mean(dat3_season$breeding_exp_pair_nogaps),y=estimate__, ymax=upper__ , ymin=lower__),size=1.3, color="#007373")+
  geom_ribbon(data = pair_duration, aes(x=sd(dat3_season$breeding_exp_pair_nogaps)*pair_duration$breeding_exp_pair_nogaps.z+mean(dat3_season$breeding_exp_pair_nogaps),y=estimate__, ymax=upper__, ymin=lower__),fill="#007373",alpha=0.3)+
  geom_point(data = dat3_season, aes(size = Count,x=sd(dat3_season$breeding_exp_pair_nogaps)*dat3_season$breeding_exp_pair_nogaps.z+mean(dat3_season$breeding_exp_pair_nogaps),y=tot_fitness), color="#007373",alpha=0.15) +
  #ylim(0, 9)+
  #xlim(1,3)+
  xlab(element_blank())+
  ylab(element_blank())+
    theme(text = element_text(size=20)) +
  theme(legend.title = element_text(size = 13),
        legend.text = element_text(size = 13))+
  #theme(legend.position = c(0.9, 0.9))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))+
  scale_x_continuous(breaks=1:3,
                     labels=c("1","2","3"),limits = c(1,3))+
  scale_y_continuous(breaks=seq(0,9,by=2),
                     labels=c("0","2","4","6","8"),limits = c(0,9))



ggsave("tot_fit_pair_durat_3y_nat2_v4.eps", device=cairo_ps, width = 10, height = 9,
       units ="cm", fallback_resolution = 600)



####Forest Plot - NAT - CROSS. ####

plot_model(mod2,type="std2", vline.color = "#007373", line.size = .35, 
           sort.est = TRUE, transform = NULL, show.values = TRUE,
           ci.style="whisker", value.size=1.9,
           prob.inner=0,prob.outer=.95, width=0.2, 
           bpe.style = "dot", bpe.color = "black")+  
  ylim(-.60, .82)+ 
  theme(text=element_blank())+
  theme(panel.grid = element_blank())+
  theme(panel.background = element_blank())+
  theme(panel.background = element_rect(color = NA)) + #basic theme
  theme( plot.title = element_blank())+
  theme(axis.text.y=element_blank(),axis.ticks.y = element_blank())+    #remove the label
  theme(axis.text.x=element_blank()) +
  theme(axis.line.x = element_line())

#size 130-180




#PROTECTED

## protected conditions
# I compared 4 models
# 2 model families  compared (zero-inflated poisson and poisson)
# best model did not converge (divergent transitions), model simplified
# 
# 1- zero inflated poisson, full
# 2- poisson, full
# 3- poisson, remove mate ID
# 4- poisson, remove sex, mate ID



#zero inflated poisson
mod1a = brms::brm(tot_fitness ~ breeding_exp_pair_nogaps.z+
                   breeding_exp_Mate_nogaps.z+
                   Mate_min_Seasons.z+
                   Focal_min_Seasons.z+
                   season_length_population.z+
                   Col_Size.z+
                   mo(season_of_protection)+
                   sex+
                   (1|Season)  +(1|Pair) +
                   (1|Colony)+ (1|Mate_ID),
                 control = list(adapt_delta = 0.99999, max_treedepth = 12),
                 iter = 8000,
                 cores = 4,
                 #prior = set_prior('normal(0, 3)'),
                 data = dat3_season,
                 family = "zero_inflated_poisson",
                 file="tot_fit_3yrs_Season_pred1")
#There were 5 divergent transitions after warmup.
mod1a<-add_criterion(mod1a, "loo", cores=4)
#Found 1 observations with a pareto_k > 0.7 in model 'mod1a'.

plot(mod1a)

pp_check(mod1a)
#quite bad

bayes_R2(mod1a)
#     Estimate Est.Error      Q2.5     Q97.5
# R2 0.3627865 0.1086263 0.1813233 0.6104156

#poisson
mod1 = brms::brm(tot_fitness ~ breeding_exp_pair_nogaps.z+
                   breeding_exp_Mate_nogaps.z+
                   Mate_min_Seasons.z+
                   Focal_min_Seasons.z+
                   season_length_population.z+
                   Col_Size.z+
                   mo(season_of_protection)+
                   sex+
                   (1|Season)  +(1|Pair) +
                   (1|Colony)+ (1|Mate_ID),
                 control = list(adapt_delta = 0.99999, max_treedepth = 12),
                 iter = 8000,
                 cores = 4,
                 #prior = set_prior('normal(0, 3)'),
                 data = dat3_season,
                 family = "poisson",
                 file="tot_fit_3yrs_Season_pred1a")
#There were 4 divergent transitions after warmup.

mod1<-add_criterion(mod1, "loo", cores=4)
#Found 2 observations with a pareto_k > 0.7 in model 'mod1'

plot(mod1)

pp_check(mod1)
#quite bad

bayes_R2(mod1)
# Estimate  Est.Error      Q2.5     Q97.5
# R2 0.5403126 0.09948582 0.3227617 0.7062129


loo_compare(mod1, mod1a)
#model with poisson link function has a better fit

#not use mate ID
mod2 = brms::brm(tot_fitness ~ breeding_exp_pair_nogaps.z+
                   breeding_exp_Mate_nogaps.z+
                   Mate_min_Seasons.z+
                   Focal_min_Seasons.z+
                   season_length_population.z+
                   Col_Size.z+
                   mo(season_of_protection)+
                   sex+
                   (1|Season)  +(1|Pair) +
                   (1|Colony),#+ (1|Mate_ID),
                 control = list(adapt_delta = 0.99999, max_treedepth = 12),
                 iter = 8000,
                 cores = 4,
                 #prior = set_prior('normal(0, 3)'),
                 data = dat3_season,
                 family = "poisson",
                 file="tot_fit_3yrs_Season_pred2a")
#There were 5 divergent transitions after warmup.

mod2<-add_criterion(mod2, "loo", cores=4)
#Found 3 observations with a pareto_k > 0.7 in model 'mod2'

plot(mod2)

pp_check(mod2)
#quite bad


#### FINAL MODEL - PROTECTED - CROSS. ####
#erase sex (little variance explained) and Mate ID
mod3 = brms::brm(tot_fitness ~ breeding_exp_pair_nogaps.z+
                   breeding_exp_Mate_nogaps.z+
                   Mate_min_Seasons.z+
                   Focal_min_Seasons.z+
                   season_length_population.z+
                   Col_Size.z+
                   mo(season_of_protection)+
                   #sex+
                   (1|Season)  +(1|Pair) +
                   (1|Colony),#+ (1|Mate_ID),
                 control = list(adapt_delta = 0.99999, max_treedepth = 12),
                 iter = 8000,
                 cores = 4,
                 #prior = set_prior('normal(0, 3)'),
                 data = dat3_season,
                 family = "poisson",
                 file="tot_fit_3yrs_Season_pred3a")


mod3<-add_criterion(mod3, "loo", cores=4)
#Found 2 observations with a pareto_k > 0.7 in model 'mod3'

plot(mod3)

pp_check(mod3)
#not great

bayes_R2(mod3)
#     Estimate Est.Error      Q2.5     Q97.5
# R2 0.5344586 0.1068948 0.2973552 0.7109028

loo_compare(mod1,mod2, mod3)
#mod3 is the best





#### main variable plot - PROT. - CROSS. ####
#extract Plotting values
marg<-marginal_effects(mod3, resolution=1000)
#select the variable of interest
pair_duration<-marg$breeding_exp_pair_nogaps.z

#plot

dat3_season <- within(dat3_season, Count <- ave(tot_fitness, list(tot_fitness, breeding_exp_pair_nogaps), FUN=length))  


ggplot()+
  geom_line(data = pair_duration, aes(x=sd(dat3_season$breeding_exp_pair_nogaps)*pair_duration$breeding_exp_pair_nogaps.z+mean(dat3_season$breeding_exp_pair_nogaps),y=estimate__, ymax=upper__ , ymin=lower__),size=1.3, color="#D55E00")+
  geom_ribbon(data = pair_duration, aes(x=sd(dat3_season$breeding_exp_pair_nogaps)*pair_duration$breeding_exp_pair_nogaps.z+mean(dat3_season$breeding_exp_pair_nogaps),y=estimate__, ymax=upper__, ymin=lower__),fill="#D55E00",alpha=0.3)+
  geom_point(data = dat3_season, aes(size = Count,x=sd(dat3_season$breeding_exp_pair_nogaps)*dat3_season$breeding_exp_pair_nogaps.z+mean(dat3_season$breeding_exp_pair_nogaps),y=tot_fitness), color="#D55E00",alpha=0.15) +
  # ylim(0, 9)+
  # xlim(1,3)+
  xlab(element_blank())+
  ylab(element_blank())+
  theme(text = element_text(size=20)) +
  theme(legend.title = element_text(size = 13),
        legend.text = element_text(size = 13))+
  #theme(legend.position = c(0.9, 0.9))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))+
  scale_x_continuous(breaks=1:3,
                     labels=c("1","2","3"),limits = c(1,3))+
  scale_y_continuous(breaks=seq(0,9,by=2),
                     labels=c("0","2","4","6","8"),limits = c(0,9))



ggsave("tot_fit_pair_durat_3y_prot3_v4.eps", device=cairo_ps, width = 10, height = 9,
       units ="cm", fallback_resolution = 600)



####Forest Plot - PROT. - CROSS. ####


plot_model(mod3,type="std2", vline.color = "#D55E00", line.size = .35, 
           sort.est = TRUE, transform = NULL, show.values = TRUE,
           ci.style="whisker", value.size=1.9,
           prob.inner=0,prob.outer=.95, width=0.2, 
           bpe.style = "dot", bpe.color = "black",
           rm.terms = c("simo_moseason_of_protection1.1.",
                        "simo_moseason_of_protection1.2.",
                        "simo_moseason_of_protection1.3.",
                        "simo_moseason_of_protection1.4.",
                        "simo_moseason_of_protection1.5."))+  
  #scale_color_sjplot("simply")+ 
  ylim(-.66, .95)+ 
  theme(text=element_blank())+
  theme(panel.grid = element_blank())+
  theme(panel.background = element_blank())+
  theme(panel.background = element_rect(color = NA)) + #basic theme
  theme( plot.title = element_blank())+
  theme(axis.text.y=element_blank(),axis.ticks.y = element_blank())+    #remove the label
  theme(axis.text.x=element_blank()) +
  theme(axis.line.x = element_line())

#
## dimension: 130 - 180



