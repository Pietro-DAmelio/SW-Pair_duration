#Aim: Describe influence of pair experience on the size of the eggs
#Date: May '19
#Author: P. D'Amelio


### libraries  ####
library(brms)
library(loo)
library(sjPlot)
library(ggplot2)


##LONGITUDINAL database  #####


#load database
dat<- read.csv("YOUR_DIRECTORY/Longitudinal_dataset.csv")


#CHOOSE natural OR protected colonies



#natural colonies 
dat_subset<-dat[,c( "egg_mass", "Final_clutch_size","Mom_breeding_attempt_ordinal_season",
                    "breeding_exp_pair_days" , "season_length_population",
                    "Pair", "Season", "predation", "breeding_exp_Mom_days", "breeding_exp_Dad_days",
                    "Col_Size", "Mom_MinAge", "Dad_MinAge", "Colony", "tarsus_mom",
                    "BreederMom", "BreederDad", "season_length","ColNestLaying", "unique_identifier")] 
dat_subset<-subset(dat_subset, predation=="natural")




#protected colonies
dat_subset<-dat[,c( "egg_mass","Final_clutch_size","Mom_breeding_attempt_ordinal_season",
                    "breeding_exp_pair_days" , "season_length_population",
                    "Pair", "Season", "predation", "breeding_exp_Mom_days", "breeding_exp_Dad_days",
                    "Col_Size", "Mom_MinAge", "Dad_MinAge", "Colony", "tarsus_mom",
                    "BreederMom", "BreederDad", "season_length",
                    "season_of_protection","ColNestLaying", "unique_identifier")] 
dat_subset<-subset(dat_subset, predation=="protected")




##Now the dataset follow the same path


#remove NAs, duplicates
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
dat_subset$season_length_population.z<- scale(dat_subset$season_length_population)

dat_subset$tarsus_mom.z<- scale(dat_subset$tarsus_mom)
dat_subset$Final_clutch_size.z<-scale(dat_subset$Final_clutch_size)


## Natural conditions

# full model did not converge because of number of iteration was too low:
# 2 models ran:
# 1- 8000 iteractions
# 2- 10000 iteractions



# eggMass_nat5a had 8000 iteractions and gave a warning message
# Tail Effective Samples Size (ESS) is too low, indicating posterior variances and tail quantiles may be unreliable.
# Running the chains for more iterations may help


#### FINAL MODEL FULL NATURAL ####
#10000 iteractions
mod5 = brms::brm(egg_mass ~ breeding_exp_pair_days.z +
                   breeding_exp_Dad_days.z+
                   breeding_exp_Mom_days.z+
                   Mom_MinAge.z+
                   Dad_MinAge.z+
                   Col_Size.z+
                   tarsus_mom.z+
                   Final_clutch_size.z+
                   mo(Mom_breeding_attempt_ordinal_season)+
                   (1|Season)  + (1|ColNestLaying) + (1|Colony)+
                   (breeding_exp_pair_days.z|Pair) +
                   (breeding_exp_Dad_days.z|BreederDad) + 
                   (breeding_exp_Mom_days.z|BreederMom),
                 control = list(adapt_delta = 0.99, max_treedepth = 12),
                 iter = 10000,
                 cores = 4,
                 #prior = set_prior('normal(0, 3)'),
                 data = dat_subset,
                 file="eggMass_nat5b")

plot(mod5)

pp_check(mod5)
#very good fit

bayes_R2(mod5)
#     Estimate   Est.Error      Q2.5    Q97.5
# R2 0.6751177 0.007610177 0.659285 0.6895212




res <- residuals(mod5, summary = TRUE)
head(res)
hist(res,breaks=seq(-1,1,0.05))




#### main variable plot - NAT - LONG ####

#extract Plotting values
marg<-marginal_effects(mod5, resolution=1000)
#select the variable of interest
Egg_mass<-marg$breeding_exp_pair_days.z

#plot

ggplot()+
  geom_line(data = Egg_mass,
            aes(x=sd(dat_subset$breeding_exp_pair_days)*
                      Egg_mass$breeding_exp_pair_days.z+
                      mean(dat_subset$breeding_exp_pair_days),
                y=estimate__, ymax=upper__ , ymin=lower__),
            size=1.3, color="#007373")+
  geom_ribbon(data = Egg_mass,
              aes(x=sd(dat_subset$breeding_exp_pair_days)*
                        Egg_mass$breeding_exp_pair_days.z+
                        mean(dat_subset$breeding_exp_pair_days),
                  y=estimate__, ymax=upper__, ymin=lower__),
              fill="#007373",alpha=0.3)+
  geom_point(data = dat_subset,
             aes(x=sd(dat_subset$breeding_exp_pair_days)*
                       dat_subset$breeding_exp_pair_days.z+
                       mean(dat_subset$breeding_exp_pair_days),
                 y=egg_mass),color="#007373",alpha=0.1) +
  ylim(1.5,3.4)+
  #xlim(0,1500)+
  xlab(element_blank())+
  ylab(element_blank())+
  theme(text = element_text(size=20))+
  #theme(legend.position = c(0.9, 0.9))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))+
scale_x_continuous(limits= c(0,1500),breaks = c(0,350,700,1050, 1400),
                   labels=c("0","350", "700", "1050", "1400"))

ggsave("eggMass_exp_nat5_v4.eps", device=cairo_ps, width = 8, height = 8,
       units ="cm", fallback_resolution = 600)





####Forest Plot - NAT - LONG. ####


plot_model(mod5,bpe = "mean",type="std2", vline.color = "#007373", line.size = .35, 
           sort.est = TRUE, transform = NULL, show.values = TRUE,
           ci.style="whisker", value.size=1.9,
           prob.inner=0,prob.outer=.95, width=0.2, 
           bpe.style = "dot", bpe.color = "black",
           rm.terms = c("simo_moMom_breeding_attempt_ordinal_season1.12.",
                        "simo_moMom_breeding_attempt_ordinal_season1.11.",
                        "simo_moMom_breeding_attempt_ordinal_season1.10.",
                        "simo_moMom_breeding_attempt_ordinal_season1.9.",
                        "simo_moMom_breeding_attempt_ordinal_season1.8.",
                        "simo_moMom_breeding_attempt_ordinal_season1.7.",
                        "simo_moMom_breeding_attempt_ordinal_season1.6.",
                        "simo_moMom_breeding_attempt_ordinal_season1.5.",
                        "simo_moMom_breeding_attempt_ordinal_season1.4.",
                        "simo_moMom_breeding_attempt_ordinal_season1.3.",
                        "simo_moMom_breeding_attempt_ordinal_season1.2.",
                        "simo_moMom_breeding_attempt_ordinal_season1.1."))+  
  #scale_color_sjplot("simply")+ 
  ylim(-.055, .055)+ 
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



###PROTECTED


#### FINAL MODEL PROTECTED - LONG. ####
mod4 = brms::brm(egg_mass ~ breeding_exp_pair_days.z +
                   breeding_exp_Dad_days.z+
                   breeding_exp_Mom_days.z+
                   Mom_MinAge.z+
                   Dad_MinAge.z+
                   Col_Size.z+
                   tarsus_mom.z+
                   Final_clutch_size.z+
                   mo(season_of_protection)+
                   mo(Mom_breeding_attempt_ordinal_season)+
                   (1|Season)  + (1|ColNestLaying) + (1|Colony)+
                   (breeding_exp_pair_days.z|Pair) +
                   (breeding_exp_Dad_days.z|BreederDad) + 
                   (breeding_exp_Mom_days.z|BreederMom),
                 control = list(adapt_delta = 0.99, max_treedepth = 12),
                 iter = 8000,
                 cores = 4,
                 #prior = set_prior('normal(0, 3)'),
                 data = dat_subset,
                 file="eggMass_prot4a")

plot(mod4)

pp_check(mod4)
#really good



bayes_R2(mod4)
#     Estimate   Est.Error      Q2.5     Q97.5
# R2 0.6810165 0.008668798 0.6634062 0.6971999





#### main variable plot - PROT - LONG. ####

marg<-marginal_effects(mod4, resolution=1000)

Egg_mass<-marg$breeding_exp_pair_days.z



ggplot()+
  geom_line(data = Egg_mass,
            aes(x=sd(dat_subset$breeding_exp_pair_days)*
                      Egg_mass$breeding_exp_pair_days.z+
                      mean(dat_subset$breeding_exp_pair_days),
                y=estimate__, ymax=upper__ , ymin=lower__),
            size=1.3, color="#D55E00")+
  geom_ribbon(data = Egg_mass,
              aes(x=sd(dat_subset$breeding_exp_pair_days)*
                        Egg_mass$breeding_exp_pair_days.z+
                        mean(dat_subset$breeding_exp_pair_days),
                  y=estimate__, ymax=upper__, ymin=lower__),
              fill="#D55E00",alpha=0.3)+
  geom_point(data = dat_subset,
             aes(x=sd(dat_subset$breeding_exp_pair_days)*
                       dat_subset$breeding_exp_pair_days.z+
                       mean(dat_subset$breeding_exp_pair_days),
                 y=egg_mass),color="#D55E00",alpha=0.1) +
  ylim(1.5,3.4)+
  #xlim(0,1500)+
  xlab(element_blank())+
  ylab(element_blank())+
  theme(text = element_text(size=20))+
  #theme(legend.position = c(0.9, 0.9))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))+
  scale_x_continuous(limits= c(0,1500),breaks = c(0,350,700,1050, 1400),
                     labels=c("0","350", "700", "1050", "1400"))

ggsave("eggMass_exp_prot4_v4.eps", device=cairo_ps, width = 8, height = 8,
       units ="cm", fallback_resolution = 600)






####Forest Plot - PROT. - LONG. ####

plot_model(mod4,bpe = "mean",type="std2", vline.color = "#D55E00", line.size = .35, 
           sort.est = TRUE, transform = NULL, show.values = TRUE,
           ci.style="whisker", value.size=1.9,
           prob.inner=0,prob.outer=.95, width=0.2, 
           bpe.style = "dot", bpe.color = "black",
           rm.terms = c("simo_moseason_of_protection1.1.",
                        "simo_moseason_of_protection1.2.",
                        "simo_moseason_of_protection1.3.",
                        "simo_moseason_of_protection1.4.",
                        "simo_moseason_of_protection1.5.",
                        "simo_moMom_breeding_attempt_ordinal_season1.11.",
                        "simo_moMom_breeding_attempt_ordinal_season1.10.",
                        "simo_moMom_breeding_attempt_ordinal_season1.9.",
                        "simo_moMom_breeding_attempt_ordinal_season1.8.",
                        "simo_moMom_breeding_attempt_ordinal_season1.7.",
                        "simo_moMom_breeding_attempt_ordinal_season1.6.",
                        "simo_moMom_breeding_attempt_ordinal_season1.5.",
                        "simo_moMom_breeding_attempt_ordinal_season1.4.",
                        "simo_moMom_breeding_attempt_ordinal_season1.3.",
                        "simo_moMom_breeding_attempt_ordinal_season1.2.",
                        "simo_moMom_breeding_attempt_ordinal_season1.1."))+  
  #scale_color_sjplot("simply")+ 
  ylim(-.06, .05)+ 
  theme(text=element_blank())+
  theme(panel.grid = element_blank())+
  theme(panel.background = element_blank())+
  theme(panel.background = element_rect(color = NA)) + #basic theme
  theme( plot.title = element_blank())+
  theme(axis.text.y=element_blank(),axis.ticks.y = element_blank())+    #remove the label
  theme(axis.text.x=element_blank()) +
  theme(axis.line.x = element_line())

## size: 130 - 180


###Egg size CROSS-SECTIONAL database ####


# keep only females

# choose if you want to select natual or protected conditions

# Do not consider ordinal season because it does not explain any variance in the full datasets


#Load database

dat3<- read.csv("YOUR_DIRECTORY/CrossSectional_dataset_breeding_attempt.csv")


##choose if you want to select natural or protected conditions


#natural codition
dat3_br_attempt<-dat3[,c("egg_mass", "breeding_exp_pair_days", "predation", "tarsus_Focal", 
                            "sex", "Season", "breeding_exp_Mate_days", "Colony", "Col_Size",
                            "Focal_MinAge", "Mate_MinAge", "Final_clutch_size",
                            "Pair", "ring", "Mate_ID", "season_length_population",
                            "season_length","ColNestLaying", "unique_identifier")]
dat3_br_attempt<- subset(dat3_br_attempt, predation=="natural")



#protected colonies
dat3_br_attempt<-dat3[,c("egg_mass", "breeding_exp_pair_days", "predation", "tarsus_Focal", 
                            "sex", "Season", "breeding_exp_Mate_days", "Colony", "Col_Size",
                            "Focal_MinAge", "Mate_MinAge", "season_of_protection",
                            "Pair", "ring", "Mate_ID", "season_length_population", "season_length","ColNestLaying",
                            "Final_clutch_size", "unique_identifier")]
dat3_br_attempt<- subset(dat3_br_attempt, predation=="protected")



#erase the incomplete cases
dat3_br_attempt<-dat3_br_attempt[complete.cases(dat3_br_attempt), ] #select only complete cases

#erase duplicates
dat3_br_attempt<-unique(dat3_br_attempt)


#variable tranformation
dat3_br_attempt$Colony<- as.factor(dat3_br_attempt$Colony)
dat3_br_attempt$sex<- as.factor(dat3_br_attempt$sex)



#run model only for females
dat3_br_attempt_f<-subset(dat3_br_attempt, sex=="2")


##Scaling
dat3_br_attempt_f$breeding_exp_Mate_days.z<- scale(dat3_br_attempt_f$breeding_exp_Mate_days)
dat3_br_attempt_f$breeding_exp_pair_days.z<- scale(dat3_br_attempt_f$breeding_exp_pair_days)
dat3_br_attempt_f$Focal_MinAge.z<- scale(dat3_br_attempt_f$Focal_MinAge)
dat3_br_attempt_f$Mate_MinAge.z<- scale(dat3_br_attempt_f$Mate_MinAge)
dat3_br_attempt_f$Col_Size.z<- scale(dat3_br_attempt_f$Col_Size)
dat3_br_attempt_f$season_length_population.z<-scale(dat3_br_attempt_f$season_length_population)
dat3_br_attempt_f$season_length.z <- scale(dat3_br_attempt_f$season_length)
dat3_br_attempt_f$tarsus_Focal.z <- scale(dat3_br_attempt_f$tarsus_Focal)
dat3_br_attempt_f$Final_clutch_size.z <- scale(dat3_br_attempt_f$Final_clutch_size)



##NATURAL

## Natural conditions
# full model did not converge, model simplified:
# 3 models ran:
# 1- full
# 2- exclude the correlation between slope and intercept
# 3- erased the group factor "Colony"

#full model
mod1 = brms::brm(egg_mass ~ breeding_exp_pair_days.z +
                   breeding_exp_Mate_days.z+
                   Focal_MinAge.z+
                   Mate_MinAge.z+
                   Col_Size.z+
                   tarsus_Focal.z+
                   Final_clutch_size.z+
                   (1|Season)  + (1|ColNestLaying) + (1|Colony)+
                   (1|ring)+ (breeding_exp_pair_days.z|Pair) +(breeding_exp_Mate_days.z|Mate_ID),
                 control = list(adapt_delta = 0.9999, max_treedepth = 12),
                 iter = 8000,
                 cores = 4,
                 #prior = set_prior('normal(0, 3)'),
                 data = dat3_br_attempt_f,
                 file="eggMass_3yrs_nat1")
#There were 27 divergent transitions after warmup.


#simplify by not modelling the correlation between slope and intercept
mod2 = brms::brm(egg_mass ~ breeding_exp_pair_days.z +
                   breeding_exp_Mate_days.z+
                   Focal_MinAge.z+
                   Mate_MinAge.z+
                   Col_Size.z+
                   tarsus_Focal.z+
                   Final_clutch_size.z+
                   (1|Season) + (1|Colony) + (1|ColNestLaying)+
                   (1|ring)+(breeding_exp_pair_days.z||Pair) +(breeding_exp_Mate_days.z||Mate_ID),
                 control = list(adapt_delta = 0.9999, max_treedepth = 13),
                 iter = 8000,
                 cores = 4,
                 #prior = set_prior('normal(0, 3)'),
                 data = dat3_br_attempt_f,
                 file="eggMass_3yrs_nat2")
#There were 10 divergent transitions after warmup.

#+ (1|Colony) It explains very little variance
#### FINAL MODEL NATURAL - CROSS. ####
mod3 = brms::brm(egg_mass ~ breeding_exp_pair_days.z +
                   breeding_exp_Mate_days.z+
                   Focal_MinAge.z+
                   Mate_MinAge.z+
                   Col_Size.z+
                   tarsus_Focal.z+
                   Final_clutch_size.z+
                   (1|Season)  + (1|ColNestLaying)+
                   (1|ring)+(breeding_exp_pair_days.z||Pair) +
                   (breeding_exp_Mate_days.z||Mate_ID),
                 control = list(adapt_delta = 0.9999, max_treedepth = 13),
                 iter = 8000,
                 cores = 4,
                 #prior = set_prior('normal(0, 3)'),
                 data = dat3_br_attempt_f,
                 file="eggMass_3yrs_nat3")
#There were 8 divergent transitions after warmup.

#The few divergent transitions are not a problem as results are largely consistent 
#between the 3 models

plot(mod3)

pp_check(mod3)
#good

bayes_R2(mod3)
#     Estimate  Est.Error      Q2.5     Q97.5
# R2 0.6151594 0.02427528 0.5635332 0.6576825

#### main variable plot - NAT - CROSS. ####

#extract Plotting values
marg<-marginal_effects(mod3, resolution=1000)
#select the variable of interest
Egg_mass<-marg$breeding_exp_pair_days.z

#plot

ggplot()+
  geom_line(data = Egg_mass,
            aes(x=sd(dat3_br_attempt_f$breeding_exp_pair_days)*
                      Egg_mass$breeding_exp_pair_days.z+
                      mean(dat3_br_attempt_f$breeding_exp_pair_days),
                y=estimate__, ymax=upper__ , ymin=lower__),
            size=1.3, color="#007373")+
  geom_ribbon(data = Egg_mass,
              aes(x=sd(dat3_br_attempt_f$breeding_exp_pair_days)*
                        Egg_mass$breeding_exp_pair_days.z+
                        mean(dat3_br_attempt_f$breeding_exp_pair_days),
                  y=estimate__, ymax=upper__, ymin=lower__),
              fill="#007373",alpha=0.3)+
  geom_point(data = dat3_br_attempt_f,
             aes(x=sd(dat3_br_attempt_f$breeding_exp_pair_days)*
                       dat3_br_attempt_f$breeding_exp_pair_days.z+
                       mean(dat3_br_attempt_f$breeding_exp_pair_days),
                 y=egg_mass),color="#007373",alpha=0.1) +
  ylim(1.5,3.4)+
  #xlim(0,3)+
  xlab(element_blank())+
  ylab(element_blank())+
  theme(text = element_text(size=20))+
  #theme(legend.position = c(0.9, 0.9))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))+
  scale_x_continuous(limits= c(0,1100),breaks = c(0,350,700,1050),
                     labels=c("0","350", "700", "1050"))



ggsave("Egg_mass_exp_3y_nat3_v2.eps", device=cairo_ps, width = 8, height = 8,
       units ="cm", fallback_resolution = 600)






####Forest Plot - NAT - CROSS. ####


plot_model(mod3,bpe = "mean",type="std2", vline.color = "#007373", line.size = .35, 
           sort.est = TRUE, transform = NULL, show.values = TRUE,
           ci.style="whisker", value.size=1.9,
           prob.inner=0,prob.outer=.95, width=0.2, 
           bpe.style = "dot", bpe.color = "black")+  
  #scale_color_sjplot("simply")+ 
  ylim(-.12, .12)+ 
  theme(text=element_blank())+
  theme(panel.grid = element_blank())+
  theme(panel.background = element_blank())+
  theme(panel.background = element_rect(color = NA)) + #basic theme
  theme( plot.title = element_blank())+
  theme(axis.text.y=element_blank(),axis.ticks.y = element_blank())+    #remove the label
  theme(axis.text.x=element_blank()) +
  theme(axis.line.x = element_line())

#size 130-180


#Protected - Cross-sectional

## protected conditions
# full model did not converge, model simplified:
# 2 models ran:
# 1- full
# 2- exclude the correlation between slope and intercept
# 

mod0 = brms::brm(egg_mass ~ breeding_exp_pair_days.z +
                   breeding_exp_Mate_days.z+
                   Focal_MinAge.z+
                   Mate_MinAge.z+
                   Col_Size.z+
                   tarsus_Focal.z+
                   Final_clutch_size.z+
                   mo(season_of_protection)+
                   (1|Colony) + (1|ColNestLaying) +
                   (1|ring) + (1|Season)+
                   (breeding_exp_pair_days.z||Pair) + 
                   (breeding_exp_Mate_days.z||Mate_ID),
                 control = list(adapt_delta = 0.9999, max_treedepth = 13),
                 iter = 8000,
                 cores = 4,
                 #prior = set_prior('normal(0, 3)'),
                 data = dat3_br_attempt_f,
                 file="eggMass_prot1_3yrs0")
#There were XX divergent transitions after warmup.


#### FINAL MODEL PROTECTED - CROSS. ####
mod1 = brms::brm(egg_mass ~ breeding_exp_pair_days.z +
                    breeding_exp_Mate_days.z+
                    Focal_MinAge.z+
                    Mate_MinAge.z+
                    Col_Size.z+
                    tarsus_Focal.z+
                    Final_clutch_size.z+
                    mo(season_of_protection)+
                    (1|Colony) + (1|ColNestLaying) +
                    (1|ring) + (1|Season)+
                   (breeding_exp_pair_days.z||Pair) + 
                   (breeding_exp_Mate_days.z||Mate_ID),
                  control = list(adapt_delta = 0.9999, max_treedepth = 13),
                  iter = 8000,
                  cores = 4,
                  #prior = set_prior('normal(0, 3)'),
                  data = dat3_br_attempt_f,
                  file="eggMass_prot1_3yrs")
#There were 4 divergent transitions after warmup.

#The few divergent transitions are not a problem as results are largely consistent 
#between models

pp_check(mod1)
#good

bayes_R2(mod1)
#     Estimate Est.Error      Q2.5     Q97.5
# R2 0.6898597 0.0223561 0.6425032 0.7292921

#### main variable plot - PROT. - CROSS.####
#extract Plotting values
marg<-marginal_effects(mod1, resolution=1000)
#select the variable of interest
Egg_mass<-marg$breeding_exp_pair_days.z



ggplot()+
  geom_line(data = Egg_mass,
            aes(x=sd(dat3_br_attempt_f$breeding_exp_pair_days)*
                      Egg_mass$breeding_exp_pair_days.z+
                      mean(dat3_br_attempt_f$breeding_exp_pair_days),
                y=estimate__, ymax=upper__ , ymin=lower__),
            size=1.3, color="#D55E00")+
  geom_ribbon(data = Egg_mass,
              aes(x=sd(dat3_br_attempt_f$breeding_exp_pair_days)*
                        Egg_mass$breeding_exp_pair_days.z+
                        mean(dat3_br_attempt_f$breeding_exp_pair_days),
                  y=estimate__, ymax=upper__, ymin=lower__),
              fill="#D55E00",alpha=0.3)+
  geom_point(data = dat3_br_attempt_f,
             aes(x=sd(dat3_br_attempt_f$breeding_exp_pair_days)*
                       dat3_br_attempt_f$breeding_exp_pair_days.z+
                       mean(dat3_br_attempt_f$breeding_exp_pair_days),
                 y=egg_mass),color="#D55E00",alpha=0.1) +
  ylim(1.5,3.4)+
  #xlim(0,3)+
  xlab(element_blank())+
  ylab(element_blank())+
  theme(text = element_text(size=20))+
  #theme(legend.position = c(0.9, 0.9))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))+
  scale_x_continuous(limits= c(0,1100),breaks = c(0,350,700,1050),
                     labels=c("0","350", "700", "1050"))



ggsave("Egg_mass_exp_3y_prot1_v2.eps", device=cairo_ps, width = 8, height = 8,
       units ="cm", fallback_resolution = 600)





####Forest Plot - PROT - CROSS. ####


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
  #scale_color_sjplot("simply")+ 
  ylim(-.29, .23)+ 
  theme(text=element_blank())+
  theme(panel.grid = element_blank())+
  theme(panel.background = element_blank())+
  theme(panel.background = element_rect(color = NA)) + #basic theme
  theme( plot.title = element_blank())+
  theme(axis.text.y=element_blank(),axis.ticks.y = element_blank())+    #remove the label
  theme(axis.text.x=element_blank()) +
  theme(axis.line.x = element_line())

## size: 130 - 180

