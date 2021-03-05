#Aim:  Study if the number of clutches in a season is influenced by pair duration (and other variables)
#Author: P. D'Amelio




### libraries  ####
library(brms)
library(loo)
library(sjPlot)
library(ggplot2)



##LONGITUDINAL database  #####


#load database
dat<- read.csv("YOUR_DIRECTORY/Longitudinal_dataset.csv")


#for natural colonies you MUST use this (otherwise all NAs in season of protection)
dat_subset<-dat[,c( "N_Clutches_Pair", "breeding_exp_pair_nogaps" , "season_length_population",
                    "Pair", "Season", "predation", "breeding_exp_Mom_nogaps", "breeding_exp_Dad_nogaps",
                    "Col_Size", "Mom_min_Seasons", "Dad_min_Seasons", "Colony",
                    "BreederMom", "BreederDad", "season_length")] 
dat_subset<-subset(dat_subset, predation=="natural")



#use this for protected colonies
dat_subset<-dat[,c( "N_Clutches_Pair", "breeding_exp_pair_nogaps" , "season_length_population",
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






#### FINAL MODEL NATURAL - LONG. ####

mod1 = brms::brm(N_Clutches_Pair ~ breeding_exp_pair_nogaps.z+
                   breeding_exp_Dad_nogaps.z+
                   breeding_exp_Mom_nogaps.z+
                   Mom_min_Seasons.z+
                   Dad_min_Seasons.z+
                   season_length_population.z+
                   Col_Size.z+
                   (1|Season)  +(breeding_exp_pair_nogaps.z|Pair) +
                   (1|Colony)+ (breeding_exp_Dad_nogaps.z|BreederDad) + 
                   (breeding_exp_Mom_nogaps.z|BreederMom),
                 control = list(adapt_delta = 0.999),
                 iter = 8000,
                 cores = 4,
                 #prior = set_prior('normal(0, 3)'),
                 data = dat_subset,
                 family = "lognormal",
                 file="N_Clutches_Pair_season_nat1")

pp_check(mod1)
#ok fit


bayes_R2(mod1)
#    Estimate  Est.Error      Q2.5     Q97.5
# R2 0.497711 0.06349128 0.3653662 0.6092123
# 




#### main variable plot - NAT - LONG.####

#extract Plotting values
marg<-marginal_effects(mod1, resolution=1000)

#I choose to extract the estimates without random factors because graphically better. 
# (provide the scripts in the supplemetary material to be clear about it )

#select the variable of interest
n_clutches<-marg$breeding_exp_pair_nogaps.z

dat_subset <- within(dat_subset, Count <- ave(N_Clutches_Pair, list(N_Clutches_Pair, breeding_exp_pair_nogaps), FUN=length))  

#plot


ggplot()+
  geom_line(data=n_clutches,
            aes(x=sd(dat_subset$breeding_exp_pair_nogaps)*
                  n_clutches$breeding_exp_pair_nogaps.z+
                  mean(dat_subset$breeding_exp_pair_nogaps),
                y=estimate__, ymax=upper__ , ymin=lower__),
            size=1.3, color="#007373")+
  geom_ribbon(data=n_clutches,
              aes(x=sd(dat_subset$breeding_exp_pair_nogaps)*
                    n_clutches$breeding_exp_pair_nogaps.z+
                    mean(dat_subset$breeding_exp_pair_nogaps),
                  y=estimate__, ymax=upper__, ymin=lower__),
              fill="#007373",alpha=0.3)+
  geom_point(data = dat_subset,
             aes(size = Count,
                 x=sd(dat_subset$breeding_exp_pair_nogaps)*
                   dat_subset$breeding_exp_pair_nogaps.z+
                   mean(dat_subset$breeding_exp_pair_nogaps),
                 y=N_Clutches_Pair), color="#007373",alpha=0.15) +
  ylim(0, 13)+
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

ggsave("N_clutches_season_pair_durat_nat1_v2.eps", device=cairo_ps, width = 10, height = 9,
       units ="cm", fallback_resolution = 600)







##PROTECTED  


#### FINAL MODEL PROTECTED - LONG.####

mod1 = brms::brm(N_Clutches_Pair ~ breeding_exp_pair_nogaps.z+
                   breeding_exp_Dad_nogaps.z+
                   breeding_exp_Mom_nogaps.z+
                   Mom_min_Seasons.z+
                   Dad_min_Seasons.z+
                   season_length_population.z+
                   Col_Size.z+
                   mo(season_of_protection)+
                   (1|Season)  +(breeding_exp_pair_nogaps.z|Pair) +
                   (1|Colony)+ (breeding_exp_Dad_nogaps.z|BreederDad) + 
                   (breeding_exp_Mom_nogaps.z|BreederMom),
                 control = list(adapt_delta = 0.999),
                 iter = 8000,
                 cores = 4,
                 #prior = set_prior('normal(0, 3)'),
                 data = dat_subset,
                 family = "lognormal",
                 file="N_Clutches_Pair_season_prot1")

#has the model converged?
plot(mod1)


pp_check(mod1)
#decent

bayes_R2(mod1)
#     Estimate  Est.Error      Q2.5     Q97.5
# R2 0.4651934 0.08203718 0.2943046 0.6133583




#### main variable plot - PROT - LONG. ####
#extract Plotting values
marg<-marginal_effects(mod1, resolution=1000)

#I choose to extract the estimates without random factors because graphically better. 
# (provide the scripts in the supplemetary material to be clear about it )

#select the variable of interest
n_clutches<-marg$breeding_exp_pair_nogaps.z


dat_subset <- within(dat_subset, Count <- ave(N_Clutches_Pair, list(N_Clutches_Pair, breeding_exp_pair_nogaps), FUN=length))  


#plot
#correlation with pair duration back transformed to the original scale

ggplot()+
  geom_line(data=n_clutches,
            aes(x=sd(dat_subset$breeding_exp_pair_nogaps)*
                  n_clutches$breeding_exp_pair_nogaps.z+
                  mean(dat_subset$breeding_exp_pair_nogaps),
                y=estimate__, ymax=upper__ , ymin=lower__),
            size=1.3, color="#D55E00")+
  geom_ribbon(data=n_clutches,
              aes(x=sd(dat_subset$breeding_exp_pair_nogaps)*
                    n_clutches$breeding_exp_pair_nogaps.z+
                    mean(dat_subset$breeding_exp_pair_nogaps),
                  y=estimate__, ymax=upper__, ymin=lower__),
              fill="#D55E00",alpha=0.3)+
  geom_point(data = dat_subset,
             aes(size = Count,
                 x=sd(dat_subset$breeding_exp_pair_nogaps)*
                   dat_subset$breeding_exp_pair_nogaps.z+
                   mean(dat_subset$breeding_exp_pair_nogaps),
                 y=N_Clutches_Pair), color="#D55E00",alpha=0.15) +
  ylim(0, 13)+
  # xlim(1,5)+
  xlab(element_blank())+
  ylab(element_blank())+
  theme(text = element_text(size=20)) +
  theme(legend.title = element_text(size = 13),
        legend.text = element_text(size = 13))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))


ggsave("N_clutches_pair_durat_prot1_v2.eps", device=cairo_ps, width = 10, height = 9,
       units ="cm", fallback_resolution = 600)





####Forest Plot - PROT - LONG. ####

plot_model(mod1,bpe = "mean",type="std2", vline.color = "#D55E00", line.size = .35, 
           sort.est = TRUE, transform = NULL, show.values = TRUE,
           ci.style="whisker", value.size=1.9,
           prob.inner=0,prob.outer=.95, width=0.2, 
           bpe.style = "dot", bpe.color = "black",
           rm.terms = c("simo_moseason_of_protection1.1.",
                        "simo_moseason_of_protection1.2.",
                        "simo_moseason_of_protection1.3.",
                        "simo_moseason_of_protection1.4.",
                        "simo_moseason_of_protection1.5."))+  
  ylim(-0.25, 0.65)+ 
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


#NATURAL
dat3_season<-dat3[,c("N_Clutches_Pair", "breeding_exp_pair_nogaps", "predation",
                            "sex", "Season", "breeding_exp_Mate_nogaps", "Colony", "Col_Size",
                            "Focal_min_Seasons", "Mate_min_Seasons", 
                            "Pair", "ring", "Mate_ID", "season_length_population", "season_length")]
dat3_season<-subset(dat3_season, predation=="natural")


#PROTECTED
dat3_season<-dat3[,c("N_Clutches_Pair", "breeding_exp_pair_nogaps", "predation",
                            "sex", "Season", "breeding_exp_Mate_nogaps", "Colony", "Col_Size",
                            "Focal_min_Seasons", "Mate_min_Seasons",  "season_of_protection",
                            "Pair", "ring", "Mate_ID", "season_length_population", "season_length")]
dat3_season<-subset(dat3_season, predation=="protected")


##Now the databases follow the same path

## Eliminate duplicates 
dat3_season<- unique(dat3_season)

#select only complete cases
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

hist(dat3_season$N_Clutches_Pair, seq(0,max(dat3_season$N_Clutches_Pair),1 ))


#NATURAL

## Natural conditions
# 2 link functions compared:
# 1- poisson
# 2- negbinomial


#poisson full model
#### FINAL MODEL NATURAL - CROSS. ####
mod1 = brms::brm(N_Clutches_Pair ~ breeding_exp_pair_nogaps.z+
                   breeding_exp_Mate_nogaps.z+
                   Mate_min_Seasons.z+
                   Focal_min_Seasons.z+
                   season_length_population.z+
                   Col_Size.z+
                   sex+
                   (1|Season)  +(1|Pair) +
                   (1|Colony)+ (1|Mate_ID),
                 control = list(adapt_delta = 0.9999, max_treedepth = 10),
                 iter = 8000,
                 cores = 4,
                 #prior = set_prior('normal(0, 3)'),
                 data = dat3_season,
                 family = "poisson",
                 file="N_Clutches_3yrs_Season_nat1a")

mod1<-add_criterion(mod1, "loo",reloo=TRUE, cores=4)#

pp_check(mod1)

bayes_R2(mod1)
#     Estimate  Est.Error      Q2.5     Q97.5
# R2 0.5389389 0.09218786 0.3490806 0.7058588


#negative binomial full model
mod2 = brms::brm(N_Clutches_Pair ~ breeding_exp_pair_nogaps.z+
                   breeding_exp_Mate_nogaps.z+
                   Mate_min_Seasons.z+
                   Focal_min_Seasons.z+
                   season_length_population.z+
                   Col_Size.z+
                   sex+
                   (1|Season)  +(1|Pair) +
                   (1|Colony)+ (1|Mate_ID),
                 control = list(adapt_delta = 0.9999, max_treedepth = 11),
                 iter = 8000,
                 cores = 2,
                 #prior = set_prior('normal(0, 3)'),
                 data = dat3_season,
                 family = "negbinomial",
                 file="N_Clutches_3yrs_Season_nat2a")
#There were 3 divergent transitions after warmup
mod2<-add_criterion(mod2, "loo",reloo=TRUE, cores=4)#

pp_check(mod2)

bayes_R2(mod2)
#     Estimate  Est.Error     Q2.5     Q97.5
# R2 0.5116773 0.09248884 0.329534 0.6832531



loo_compare(mod1,mod2)
#mod1, poisson, is better


#### main variable plot - NAT - CROSS. ####

#extract Plotting values
marg<-marginal_effects(mod1, resolution=1000)


#select the variable of interest
n_clutches<-marg$breeding_exp_pair_nogaps.z

dat3_season <- within(dat3_season, Count <- ave(N_Clutches_Pair, list(N_Clutches_Pair, breeding_exp_pair_nogaps), FUN=length))  

#plot
#correlation with pair duration back transformed to the original scale



#new version
ggplot()+
  geom_line(data=n_clutches,
            aes(x=sd(dat3_season$breeding_exp_pair_nogaps)*
                  n_clutches$breeding_exp_pair_nogaps.z+
                  mean(dat3_season$breeding_exp_pair_nogaps),
                y=estimate__, ymax=upper__ , ymin=lower__),
            size=1.3, color="#007373")+
  geom_ribbon(data=n_clutches,
              aes(x=sd(dat3_season$breeding_exp_pair_nogaps)*
                    n_clutches$breeding_exp_pair_nogaps.z+
                    mean(dat3_season$breeding_exp_pair_nogaps),
                  y=estimate__, ymax=upper__, ymin=lower__),
              fill="#007373",alpha=0.3)+
  geom_point(data = dat3_season,
             aes(size = Count,
                 x=sd(dat3_season$breeding_exp_pair_nogaps)*
                   dat3_season$breeding_exp_pair_nogaps.z+
                   mean(dat3_season$breeding_exp_pair_nogaps),
                 y=N_Clutches_Pair), color="#007373",alpha=0.15) +
  ylim(0, 13)+
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
        axis.line = element_line(colour = "black"))+
  scale_x_continuous(breaks=1:3,
                     labels=c("1","2","3"),limits = c(1,3))


ggsave("N_clutches_durat_3y_nat1_v2.eps", device=cairo_ps, width = 10, height = 9,
       units ="cm", fallback_resolution = 600)






####Forest Plot - NAT - CROSS. ####


plot_model(mod1,bpe = "mean",type="std2", vline.color = "#007373", line.size = .35, 
           sort.est = TRUE, transform = NULL, show.values = TRUE,
           ci.style="whisker", value.size=1.9,
           prob.inner=0,prob.outer=.95, width=0.2, 
           bpe.style = "dot", bpe.color = "black")+  
  ylim(-0.3, 0.7)+ 
  theme(text=element_blank())+
  theme(panel.grid = element_blank())+
  theme(panel.background = element_blank())+
  theme(panel.background = element_rect(color = NA)) + #basic theme
  theme( plot.title = element_blank())+
  theme(axis.text.y=element_blank(),axis.ticks.y = element_blank())+    #remove the label
  theme(axis.text.x=element_blank()) +
  theme(axis.line.x = element_line())







#PROTECTED

## Natural conditions
# 3 models ran
# 2 link functions compared:
# 1- poisson
# 2- negbinomial
# full model did not converge, model simplified:
# 3- group factor "Mate ID" removed


#poisson
mod1 = brms::brm(N_Clutches_Pair ~ breeding_exp_pair_nogaps.z+
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
                 file="N_Clutches_3yrs_Season_pred1a")
#There were 12 divergent transitions after warmup.

plot(mod1)
pp_check(mod1)

mod1<-add_criterion(mod1, "loo", cores=4)#,reloo=TRUE



#negbinomial
mod2 = brms::brm(N_Clutches_Pair ~ breeding_exp_pair_nogaps.z+
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
                 family = "negbinomial",
                 file="N_Clutches_3yrs_Season_pred2a")
#There were 1 divergent transitions after warmup.

plot(mod2)
pp_check(mod2)

mod2<-add_criterion(mod2, "loo", cores=4)#,reloo=TRUE
#Found 2 observations with a pareto_k > 0.7 in model 'mod2'

loo_compare(mod1, mod2)
#mod1 better

##convergence of poisson model

#poisson, no mate ID
#### FINAL MODEL 3years PROTECTED ####
mod3 = brms::brm(N_Clutches_Pair ~ breeding_exp_pair_nogaps.z+
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
                 file="N_Clutches_3yrs_Season_pred3a")

mod3<-add_criterion(mod3, "loo", cores=4)#,reloo=TRUE
#Found 2 observations with a pareto_k > 0.7 in model 'mod3'.

plot(mod3)
pp_check(mod3)
#okish

bayes_R2(mod3)
# Estimate Est.Error      Q2.5     Q97.5
# R2 0.4619807 0.1107744 0.2415192 0.6592893



###model without outlyer
mod3b = brms::brm(N_Clutches_Pair ~ breeding_exp_pair_nogaps.z+
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
                 file="N_Clutches_3yrs_Season_pred3b")







#### main variable plot - PROT - CROSS####

#extract Plotting values
marg<-marginal_effects(mod3, resolution=1000)


#select the variable of interest
n_clutches<-marg$breeding_exp_pair_nogaps.z

dat3_season <- within(dat3_season, Count <- ave(N_Clutches_Pair, list(N_Clutches_Pair, breeding_exp_pair_nogaps), FUN=length))  

#plot
#correlation with pair duration back transformed to the original scale

ggplot()+
  geom_line(data = n_clutches, 
            aes(x=sd(dat3_season$breeding_exp_pair_nogaps)*
                  n_clutches$breeding_exp_pair_nogaps.z+
                  mean(dat3_season$breeding_exp_pair_nogaps),
                y=estimate__, ymax=upper__ , ymin=lower__),
            size=1.3, color="#D55E00")+
  geom_ribbon(data = n_clutches,
              aes(x=sd(dat3_season$breeding_exp_pair_nogaps)*
                    n_clutches$breeding_exp_pair_nogaps.z+
                    mean(dat3_season$breeding_exp_pair_nogaps),
                  y=estimate__, ymax=upper__, ymin=lower__),
              fill="#D55E00",alpha=0.3)+
  geom_point(data = dat3_season, 
             aes(size = Count,x=sd(dat3_season$breeding_exp_pair_nogaps)*
                   dat3_season$breeding_exp_pair_nogaps.z+
                   mean(dat3_season$breeding_exp_pair_nogaps),
                 y=N_Clutches_Pair), color="#D55E00",alpha=0.15)+
  ylim(0, 13)+
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
                     labels=c("1","2","3"),limits = c(1,3.1))





ggsave("N_clutches_durat_3y_prot3_v1.eps", device=cairo_ps, width = 10, height = 9,
       units ="cm", fallback_resolution = 600)



####Forest Plot -  PROT - CROSS ####

plot_model(mod3,bpe = "mean",type="std2", vline.color = "#D55E00", line.size = .35, 
           sort.est = TRUE, transform = NULL, show.values = TRUE,
           ci.style="whisker", value.size=1.9,
           prob.inner=0,prob.outer=.95, width=0.2, 
           bpe.style = "dot", bpe.color = "black",
           rm.terms = c("simo_moseason_of_protection1.1.",
                        "simo_moseason_of_protection1.2.",
                        "simo_moseason_of_protection1.3.",
                        "simo_moseason_of_protection1.4.",
                        "simo_moseason_of_protection1.5."))+  
  ylim(-1.4, 1.9)+ 
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


