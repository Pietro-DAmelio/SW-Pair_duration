#Aim: Potential reasons for increased breeding success in case of long term monogamy: 
# Is relaying interval shorter in more experienced pairs?
#Author: P. D'Amelio



#fate of the clutch influences relaying interval (after predation relay is faster) categorical: successful, predated_eggs, predated_chicks
#at what stage is the clucth lost is probably important (lost eggs are replaced faster than chicks) 

#relaying interval calculated as: "fate date - laying date" within the same season (and extra cut-off at 3 months),
#only pairs with at least one clutch are considered


### libraries  ####
library(brms)
library(loo)
library(sjPlot)
library(ggplot2)



##LONGITUDINAL database  #####


#load database
dat<- read.csv("YOUR_DIRECTORY/Longitudinal_dataset.csv")


#delete impossibly shorts and too long

dat<-subset(dat, Relaying_int<80)
dat<-subset(dat, Relaying_int>6)

##Divide database between natural and protected

#for natural colonies you MUST use this (otherwise all NAs in season of protection)
dat_subset<-dat[,c( "Relaying_int", "Fate_clutch","breeding_exp_pair_days",
                    "Pair", "Season", "predation", "breeding_exp_Mom_days", "breeding_exp_Dad_days",
                    "Col_Size", "Mom_MinAge", "Dad_MinAge", "Colony",
                    "BreederMom", "BreederDad", "season_length")] 
dat_subset<-subset(dat_subset, predation=="natural")




#use this for protected colonies
dat_subset<-dat[,c( "Relaying_int","Fate_clutch","breeding_exp_pair_days",
                    "Pair", "Season", "predation", "breeding_exp_Mom_days", "breeding_exp_Dad_days",
                    "Col_Size", "Mom_MinAge", "Dad_MinAge", "Colony",
                    "BreederMom", "BreederDad", "season_length", "season_of_protection")] 
dat_subset<-subset(dat_subset, predation=="protected")



##Now the databases follow the same path

#remove duplicates
dat_subset<- unique(dat_subset)
#select only complete cases
dat_subset<-dat_subset[complete.cases(dat_subset), ] 


# variables that are factors
dat_subset$Colony<- as.factor(dat_subset$Colony)



#Scale continuos variables
dat_subset$breeding_exp_pair_days.z <-scale(dat_subset$breeding_exp_pair_days)
dat_subset$breeding_exp_Dad_days.z <-scale(dat_subset$breeding_exp_Dad_days)
dat_subset$breeding_exp_Mom_days.z <-scale(dat_subset$breeding_exp_Mom_days)
dat_subset$Mom_MinAge.z <-scale(dat_subset$Mom_MinAge)
dat_subset$Dad_MinAge.z <- scale(dat_subset$Dad_MinAge)

dat_subset$Col_Size.z<- scale(dat_subset$Col_Size)


##Models


# NATURAL
# 2 link functions compared:
# 1- skew normal
# 2- normal


#### FINAL MODEL NATURAL LONG. ####

mod3 = brms::brm(Relaying_int~ breeding_exp_pair_days.z+
                   breeding_exp_Dad_days.z + breeding_exp_Mom_days.z+ 
                   Mom_MinAge.z + Dad_MinAge.z +
                   Col_Size.z+
                   Fate_clutch+
                   (1|Season) + (1|Colony)  + 
                   (breeding_exp_pair_days.z||Pair)+ 
                   (breeding_exp_Dad_days.z||BreederDad)+ 
                   (breeding_exp_Mom_days.z||BreederMom),
                 control = list(adapt_delta = 0.999),
                 iter = 6000,
                 cores = 8,
                 #prior = set_prior('normal(0, 3)'),
                 data = dat_subset,
                 family="skew_normal",
                 file="Relaying_int_nat3")
mod3<-add_criterion(mod3, "loo", cores=4) #add loo , reloo = TRUE

pp_check(mod3)
#not horrible fit

#measure R squared
bayes_R2(mod3)
#    Estimate  Est.Error      Q2.5     Q97.5
#R2 0.3648811 0.02289406 0.3187627 0.4083562



#try normal
mod4 = brms::brm(Relaying_int~ breeding_exp_pair_days.z+
                   breeding_exp_Dad_days.z + breeding_exp_Mom_days.z+ 
                   Mom_MinAge.z + Dad_MinAge.z +
                   Col_Size.z+
                   Fate_clutch+
                   (1|Season) + (1|Colony)  + 
                   (breeding_exp_pair_days.z||Pair)+ 
                   (breeding_exp_Dad_days.z||BreederDad)+ 
                   (breeding_exp_Mom_days.z||BreederMom),
                 control = list(adapt_delta = 0.999),
                 iter = 6000,
                 cores = 8,
                 #prior = set_prior('normal(0, 3)'),
                 data = dat_subset,
                 file="Relaying_int_nat4")
mod4<-add_criterion(mod4, "loo", cores=4) #add loo , reloo = TRUE


pp_check(mod4)


loo_compare(mod3, mod4)
#model 3 is much much better





#### main variable plot - NAT. - LONG. ####

#extract Plotting values
marg<-marginal_effects(mod3, resolution=1000)
#select the variable of interest
relaying_int<-marg$breeding_exp_pair_days.z

#plot

ggplot()+
  geom_line(data=relaying_int,
            aes(x=sd(dat_subset$breeding_exp_pair_days)*
                  relaying_int$breeding_exp_pair_days.z+
                  mean(dat_subset$breeding_exp_pair_days),
                y=estimate__, ymax=upper__ , ymin=lower__),
            size=1.3, color="#007373")+
  geom_ribbon(data=relaying_int,
              aes(x=sd(dat_subset$breeding_exp_pair_days)*
                    relaying_int$breeding_exp_pair_days.z+
                    mean(dat_subset$breeding_exp_pair_days),
                  y=estimate__, ymax=upper__, ymin=lower__),
              fill="#007373",alpha=0.3)+
  geom_point(data = dat_subset,
             aes(x=sd(dat_subset$breeding_exp_pair_days)*
                   dat_subset$breeding_exp_pair_days.z+
                   mean(dat_subset$breeding_exp_pair_days),
                 y=Relaying_int), color="#007373",alpha=0.15) +
  ylim(0, 80)+
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

ggsave("Relaying_int_exp_nat3_v2.eps", device=cairo_ps, width = 10, height = 9,
       units ="cm", fallback_resolution = 600)




####Forest Plot - NAT. - LONG. ####

plot_model(mod3,bpe = "mean",type="std2", vline.color = "#007373", line.size = .35, 
           sort.est = TRUE, transform = NULL, show.values = TRUE,
           ci.style="whisker", value.size=1.9,
           prob.inner=0,prob.outer=.95, width=0.2, 
           bpe.style = "dot", bpe.color = "black")+  
  #scale_color_sjplot("simply")+ 
  ylim(-15, 21)+ 
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





##PROTECTED 



#### FINAL MODEL PROTECTED - LONG. ####
mod2 = brms::brm(Relaying_int~ breeding_exp_pair_days.z+
                   breeding_exp_Dad_days.z + breeding_exp_Mom_days.z+ 
                   Mom_MinAge.z + Dad_MinAge.z +
                   Col_Size.z+
                   Fate_clutch+
                   mo(season_of_protection)+
                   (1|Season) + (1|Colony)  + 
                   (breeding_exp_pair_days.z||Pair)+ 
                   (breeding_exp_Dad_days.z||BreederDad)+ 
                   (breeding_exp_Mom_days.z||BreederMom),
                 control = list(adapt_delta = 0.999),
                 iter = 8000,
                 cores = 4,
                 #prior = set_prior('normal(0, 3)'),
                 data = dat_subset,
                 family="skew_normal",
                 file="Relaying_int_prot2")
mod2<-add_criterion(mod2, "loo", cores=4) #add loo , reloo = TRUE
#Found 10 observations with a pareto_k > 0.7 in model 'mod2'

pp_check(mod2)


#measure R squared
bayes_R2(mod2)
#     Estimate  Est.Error     Q2.5     Q97.5
# R2 0.6596151 0.01659866 0.625719 0.6906019



#### main variable plot - PROT  - LONG. ####

#extract Plotting values
marg<-marginal_effects(mod2, resolution=1000)

#select the variable of interest
relaying_int<-marg$breeding_exp_pair_days.z





ggplot()+
  geom_line(data=relaying_int,
            aes(x=sd(dat_subset$breeding_exp_pair_days)*
                  relaying_int$breeding_exp_pair_days.z+
                  mean(dat_subset$breeding_exp_pair_days),
                y=estimate__, ymax=upper__ , ymin=lower__),
            size=1.3, color="#D55E00")+
  geom_ribbon(data=relaying_int,
              aes(x=sd(dat_subset$breeding_exp_pair_days)*
                    relaying_int$breeding_exp_pair_days.z+
                    mean(dat_subset$breeding_exp_pair_days),
                  y=estimate__, ymax=upper__, ymin=lower__),
              fill="#D55E00",alpha=0.3)+
  geom_point(data = dat_subset,
             aes(x=sd(dat_subset$breeding_exp_pair_days)*
                   dat_subset$breeding_exp_pair_days.z+
                   mean(dat_subset$breeding_exp_pair_days),
                 y=Relaying_int), color="#D55E00",alpha=0.15) +
  ylim(0, 80)+
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

ggsave("Relaying_int_exp_prot2_v2.eps", device=cairo_ps, width = 10, height = 9,
       units ="cm", fallback_resolution = 600)






####Forest Plot - PROT - LONG. ####



plot_model(mod2,bpe = "mean",type="std2", vline.color = "#D55E00", line.size = .35, 
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
  ylim(-16, 28)+ 
  theme(text=element_blank())+
  theme(panel.grid = element_blank())+
  theme(panel.background = element_blank())+
  theme(panel.background = element_rect(color = NA)) + #basic theme
  theme( plot.title = element_blank())+
  theme(axis.text.y=element_blank(),axis.ticks.y = element_blank())+    #remove the label
  theme(axis.text.x=element_blank()) +
  theme(axis.line.x = element_line())

#
## size: 130 - 180


##CROSS-SECTIONAL database  #####


#Load database

dat3<- read.csv("YOUR_DIRECTORY/CrossSectional_dataset_breeding_attempt.csv")


##choose if you want to select natual or protected conditions

#natural codition
dat3_br_attempt<-dat3[,c("Relaying_int", "Fate_clutch", "breeding_exp_pair_days", "predation",  
                                    "sex", "Season", "breeding_exp_Mate_days", "Colony", "Col_Size",
                                    "Focal_MinAge", "Mate_MinAge", 
                                    "Pair", "ring", "Mate_ID", "season_length")]
dat3_br_attempt<- subset(dat3_br_attempt, predation=="natural")

#protected colonies
dat3_br_attempt<-dat3[,c("Relaying_int","Fate_clutch", "breeding_exp_pair_days", "predation", 
                                    "sex", "Season", "breeding_exp_Mate_days", "Colony", "Col_Size",
                                    "Focal_MinAge", "Mate_MinAge", "season_of_protection",
                                    "Pair", "ring", "Mate_ID", "season_length")]
dat3_br_attempt<- subset(dat3_br_attempt, predation=="protected")



#erase the incomplete cases
dat3_br_attempt<-dat3_br_attempt[complete.cases(dat3_br_attempt), ] #select only complete cases

#erase duplicates
dat3_br_attempt<-unique(dat3_br_attempt)


#variable tranformation
dat3_br_attempt$Colony<- as.factor(dat3_br_attempt$Colony)
dat3_br_attempt$sex<- as.factor(dat3_br_attempt$sex)

#limit the dataset
dat3_br_attempt<-subset(dat3_br_attempt, Relaying_int<80)
dat3_br_attempt<-subset(dat3_br_attempt, Relaying_int>6)


##Scaling
dat3_br_attempt$breeding_exp_Mate_days.z<- scale(dat3_br_attempt$breeding_exp_Mate_days)
dat3_br_attempt$breeding_exp_pair_days.z<- scale(dat3_br_attempt$breeding_exp_pair_days)
dat3_br_attempt$Focal_MinAge.z<- scale(dat3_br_attempt$Focal_MinAge)
dat3_br_attempt$Mate_MinAge.z<- scale(dat3_br_attempt$Mate_MinAge)
dat3_br_attempt$Col_Size.z<- scale(dat3_br_attempt$Col_Size)
dat3_br_attempt$season_length.z <- scale(dat3_br_attempt$season_length)




##NATURAL

## Natural conditions
# 3 models ran
# 2 link functions compared and best model simplified for convergence.
# 1- skew_normal
# 2- skew_normal without Mate ID
# 3- normal
#




#Skew normal
mod1 = brms::brm(Relaying_int ~ breeding_exp_pair_days.z +
                   breeding_exp_Mate_days.z+
                   Focal_MinAge.z+
                   Mate_MinAge.z+
                   Col_Size.z+
                   Fate_clutch+
                   sex+
                   (1|Season)  +(1|Pair) + 
                   (1|Colony)+ (1|ring) + (1|Mate_ID),
                 control = list(adapt_delta = 0.999, max_treedepth = 12),
                 iter = 8000,
                 cores = 4,
                 #prior = set_prior('normal(0, 3)'),
                 data = dat3_br_attempt,
                 family="skew_normal",
                 file="Relaying_int_3yrs_nat1")
#There were 2 divergent transitions after warmup. 

mod1<-add_criterion(mod1, "loo", cores=4) #add loo , reloo = TRUE
#Found 11 observations with a pareto_k > 0.7 in model 'mod2'.



#re-run without mate ID (random factor explaining the least variance)
#### FINAL MODEL NATURAL - CROSS. ####
mod2 = brms::brm(Relaying_int ~ breeding_exp_pair_days.z +
                   breeding_exp_Mate_days.z+
                   Focal_MinAge.z+
                   Mate_MinAge.z+
                   Col_Size.z+
                   Fate_clutch+
                   sex+
                   (1|Season)  +(1|Pair) + 
                   (1|Colony)+ (1|ring), # + (1|Mate_ID),
                 control = list(adapt_delta = 0.999, max_treedepth = 12),
                 iter = 8000,
                 cores = 4,
                 #prior = set_prior('normal(0, 3)'),
                 data = dat3_br_attempt,
                 family="skew_normal",
                 file="Relaying_int_3yrs_nat2")
mod2<-add_criterion(mod2, "loo", cores=4)
#Found 8 observations with a pareto_k > 0.7 in model 'mod2'

pp_check(mod2)
#good

bayes_R2(mod2)
#     Estimate  Est.Error      Q2.5     Q97.5
# R2 0.3794406 0.03435228 0.3182265 0.4563184

#normal
mod3 = brms::brm(Relaying_int ~ breeding_exp_pair_days.z +
                   breeding_exp_Mate_days.z+
                   Focal_MinAge.z+
                   Mate_MinAge.z+
                   Col_Size.z+
                   Fate_clutch+
                   sex+
                   (1|Season)  +(1|Pair) + 
                   (1|Colony)+ (1|ring),# + (1|Mate_ID),
                 control = list(adapt_delta = 0.999, max_treedepth = 12),
                 iter = 8000,
                 cores = 4,
                 #prior = set_prior('normal(0, 3)'),
                 data = dat3_br_attempt,
                 family="normal",
                 file="Relaying_int_3yrs_nat3")
mod3<-add_criterion(mod3, "loo", cores=4)
#Found 1 observations with a pareto_k > 0.7 in model 'mod3'
loo_compare(mod2,mod3)
#mod2 much better


#### main variable plot - NAT - CROSS. ####

#extract Plotting values
marg<-marginal_effects(mod2, resolution=1000)
#select the variable of interest
relaying_int<-marg$breeding_exp_pair_days.z

#plot
ggplot()+
  geom_line(data = relaying_int, 
            aes(x=sd(dat3_br_attempt$breeding_exp_pair_days)*
                  relaying_int$breeding_exp_pair_days.z+
                  mean(dat3_br_attempt$breeding_exp_pair_days),
                y=estimate__, ymax=upper__ , ymin=lower__),
            size=1.3, color="#007373")+
  geom_ribbon(data = relaying_int, 
              aes(x=sd(dat3_br_attempt$breeding_exp_pair_days)*
                    relaying_int$breeding_exp_pair_days.z+
                    mean(dat3_br_attempt$breeding_exp_pair_days),
                  y=estimate__, ymax=upper__, ymin=lower__),
              fill="#007373",alpha=0.3)+
  geom_point(data = dat3_br_attempt,
             aes(x=sd(dat3_br_attempt$breeding_exp_pair_days)*
                   dat3_br_attempt$breeding_exp_pair_days.z+
                   mean(dat3_br_attempt$breeding_exp_pair_days),
                 y=Relaying_int), color="#007373",alpha=0.15) +
  ylim(0, 80)+
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
  scale_x_continuous(breaks=c(0,250,500,750),
                     labels=c("0","250","500","750"),limits = c(0,1000))


ggsave("Relaying_int_exp_3yrs_nat2_v2.eps", device=cairo_ps, width = 10, height = 9,
       units ="cm", fallback_resolution = 600)



####Forest Plot - NAT. - CROSS. ####


plot_model(mod2,bpe = "mean",type="std2", vline.color = "#007373", line.size = .35, 
           sort.est = TRUE, transform = NULL, show.values = TRUE,
           ci.style="whisker", value.size=1.9,
           prob.inner=0,prob.outer=.95, width=0.2, 
           bpe.style = "dot", bpe.color = "black")+  
  ylim(-13.5, 23)+ 
  theme(text=element_blank())+
  theme(panel.grid = element_blank())+
  theme(panel.background = element_blank())+
  theme(panel.background = element_rect(color = NA)) + #basic theme
  theme( plot.title = element_blank())+
  theme(axis.text.y=element_blank(),axis.ticks.y = element_blank())+    #remove the label
  theme(axis.text.x=element_blank()) +
  theme(axis.line.x = element_line())

#size 130-180




# PROTECTED

## Protected conditions
# 2 models ran
# 2 link functions compared 
# 1- skew normal
# 2- normal 





mod1 = brms::brm(Relaying_int~ breeding_exp_pair_days.z +
                   breeding_exp_Mate_days.z+
                   Focal_MinAge.z+
                   Mate_MinAge.z+
                   Col_Size.z+
                   Fate_clutch+
                   sex+
                   mo(season_of_protection)+
                   (1|Season)  +(1|Pair) + 
                   (1|Colony)+ (1|ring) + (1|Mate_ID),
                 control = list(adapt_delta = 0.9999, max_treedepth = 12),
                 iter = 8000,
                 cores = 4,
                 #prior = set_prior('normal(0, 3)'),
                 data = dat3_br_attempt,
                 family="skew_normal",
                 file="Relaying_int_3yrs_prot1")
mod1<-add_criterion(mod1, "loo", cores=4)
#Found 6 observations with a pareto_k > 0.7 in model 'mod1'.

#normal
#### FINAL MODEL PROTECTED - CROSS. ####
mod2 = brms::brm(Relaying_int~ breeding_exp_pair_days.z +
                   breeding_exp_Mate_days.z+
                   Focal_MinAge.z+
                   Mate_MinAge.z+
                   Col_Size.z+
                   Fate_clutch+
                   sex+
                   mo(season_of_protection)+
                   (1|Season)  +(1|Pair) + 
                   (1|Colony)+ (1|ring) + (1|Mate_ID),
                 control = list(adapt_delta = 0.999, max_treedepth = 10),
                 iter = 8000,
                 cores = 4,
                 #prior = set_prior('normal(0, 3)'),
                 data = dat3_br_attempt,
                 family="normal",
                 file="Relaying_int_3yrs_prot2")

mod2<-add_criterion(mod2, "loo", cores=4)
#Found 2 observations with a pareto_k > 0.7 in model 'mod2'


loo_compare(mod1,mod2)
#mod2 better

pp_check(mod2)
#quite bad but it reflects the almost uniform distribution...

#measure R squared
bayes_R2(mod2)
#    Estimate  Est.Error      Q2.5     Q97.5
#R2 0.7655252 0.02644483 0.7073286 0.8110128




#### main variable plot - PROT - CROSS. ####
#extract Plotting values
marg<-marginal_effects(mod2, resolution=1000)
#select the variable of interest
relaying_int<-marg$breeding_exp_pair_days.z

#plot

ggplot()+
  geom_line(data = relaying_int, 
            aes(x=sd(dat3_br_attempt$breeding_exp_pair_days)*
                  relaying_int$breeding_exp_pair_days.z+
                  mean(dat3_br_attempt$breeding_exp_pair_days),
                y=estimate__, ymax=upper__ , ymin=lower__),
            size=1.3, color="#D55E00")+
  geom_ribbon(data = relaying_int,
              aes(x=sd(dat3_br_attempt$breeding_exp_pair_days)*
                    relaying_int$breeding_exp_pair_days.z+
                    mean(dat3_br_attempt$breeding_exp_pair_days),
                  y=estimate__, ymax=upper__, ymin=lower__),
              fill="#D55E00",alpha=0.3)+
  geom_point(data = dat3_br_attempt, 
             aes(x=sd(dat3_br_attempt$breeding_exp_pair_days)*
                   dat3_br_attempt$breeding_exp_pair_days.z+
                   mean(dat3_br_attempt$breeding_exp_pair_days),
                 y=Relaying_int), color="#D55E00",alpha=0.15)+
  ylim(0, 80)+
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
  scale_x_continuous(breaks=c(0,250,500,750),
                     labels=c("0","250","500","750"),limits = c(0,1000))


ggsave("Relaying_int_exp_3yrs_prot1_v1.eps", device=cairo_ps, width = 10, height = 9,
       units ="cm", fallback_resolution = 600)


####Forest Plot - PROT. - CROSS. ####


plot_model(mod2,bpe = "mean",type="std2", vline.color = "#D55E00", line.size = .35, 
           sort.est = TRUE, transform = NULL, show.values = TRUE,
           ci.style="whisker", value.size=1.9,
           prob.inner=0,prob.outer=.95, width=0.2, 
           bpe.style = "dot", bpe.color = "black",
           rm.terms = c("simo_moseason_of_protection1.1.",
                        "simo_moseason_of_protection1.2.",
                        "simo_moseason_of_protection1.3.",
                        "simo_moseason_of_protection1.4.",
                        "simo_moseason_of_protection1.5."))+  
  ylim(-29.5, 27.5)+ 
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


