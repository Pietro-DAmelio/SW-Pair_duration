#Aim: Describe influence of pair experience on the mass of the fledglings 

#Author: P. D'Amelio


### libraries  ####
library(brms)
library(loo)
library(sjPlot)
library(ggplot2)


##LONGITUDINAL database  #####


#load database
dat<- read.csv("YOUR_DIRECTORY/Longitudinal_dataset.csv")



#I run 2 analysis, one for protected and one for natural colonies.
# The 2 databases do not include the same data, subset them before scaling.


#CHOOSE natural OR protected colonies


#for natural colonies you MUST use this (otherwise all NAs in season of protection)
dat_subset<-dat[,c( "fledging_mass","breeding_exp_pair_days" , "season_length_population",
                    "Pair", "Season", "predation", "breeding_exp_Mom_days", "breeding_exp_Dad_days",
                    "Col_Size", "Mom_MinAge", "Dad_MinAge", "Colony", "tarsus_mom", "tarsus_dad",
                    "N_hatched",
                    "BreederMom", "BreederDad", "season_length","ColNestLaying","unique_identifier")] 
dat_subset<-subset(dat_subset, predation=="natural")




#use this for protected colonies
dat_subset<-dat[,c( "fledging_mass","breeding_exp_pair_days" , "season_length_population",
                    "Pair", "Season", "predation", "breeding_exp_Mom_days", "breeding_exp_Dad_days",
                    "Col_Size", "Mom_MinAge", "Dad_MinAge", "Colony", "tarsus_mom", "tarsus_dad",
                    "N_hatched",
                    "BreederMom", "BreederDad", "season_length", "season_of_protection","ColNestLaying","unique_identifier")] 
dat_subset<-subset(dat_subset, predation=="protected")




##Now the databases follow the same path
dat_subset<- unique(dat_subset)

#select only complete cases
dat_subset<-dat_subset[complete.cases(dat_subset), ] 


# variables that are factors
dat_subset$Colony<- as.factor(dat_subset$Colony)



#erase impossible values (below 19 and over 36)
dat_subset <- subset(dat_subset, fledging_mass>19 & fledging_mass<36)


# erase impossible tarsus values
# hist(dat_subset$tarsus_dad, breaks = 20)
# tail(sort(dat_subset$tarsus_dad),5)
# hist(dat_subset$tarsus_mom, breaks = 20)
# tail(sort(dat_subset$tarsus_mom),5)

dat_subset <- subset(dat_subset, tarsus_dad>19 & tarsus_dad<25.5)
dat_subset <- subset(dat_subset, tarsus_mom>19 & tarsus_mom<25.5)




#Scale continuos variables
dat_subset$breeding_exp_pair_days.z <-scale(dat_subset$breeding_exp_pair_days)
dat_subset$breeding_exp_Dad_days.z <-scale(dat_subset$breeding_exp_Dad_days)
dat_subset$breeding_exp_Mom_days.z <-scale(dat_subset$breeding_exp_Mom_days)
dat_subset$Mom_MinAge.z <-scale(dat_subset$Mom_MinAge)
dat_subset$Dad_MinAge.z <- scale(dat_subset$Dad_MinAge)

dat_subset$Col_Size.z<- scale(dat_subset$Col_Size)
dat_subset$season_length_population.z<- scale(dat_subset$season_length_population)

dat_subset$tarsus_mom.z<- scale(dat_subset$tarsus_mom)
dat_subset$tarsus_dad.z<- scale(dat_subset$tarsus_dad)

#dat_subset$Final_clutch_size.z<-scale(dat_subset$Final_clutch_size)
dat_subset$N_hatched.z<-scale(dat_subset$N_hatched)




## NATURAL conditions
## all variables scaled
## group slopes
## no interactions



#### FINAL MODEL NATURAL - LONG. ####

mod6 = brms::brm(fledging_mass ~ breeding_exp_pair_days.z +
                   breeding_exp_Dad_days.z+
                   breeding_exp_Mom_days.z+
                   Mom_MinAge.z+
                   Dad_MinAge.z+
                   tarsus_mom.z+
                   tarsus_dad.z+
                   Col_Size.z+
                   N_hatched.z+
                   (1|Season)  + (1|ColNestLaying) + (1|Colony)+
                   (breeding_exp_pair_days.z|Pair) +
                   (breeding_exp_Dad_days.z|BreederDad) + 
                   (breeding_exp_Mom_days.z|BreederMom),
                 control = list(adapt_delta = 0.99, max_treedepth = 10),
                 iter = 8000,
                 cores = 4,
                 #prior = set_prior('normal(0, 3)'),
                 data = dat_subset,
                 file="fledging_mass_nat6a")


bayes_R2(mod6)
#      Estimate  Est.Error      Q2.5     Q97.5
# R2  0.6794055 0.02074422 0.6350216 0.7169415

pp_check(mod6)
#good but clearly, despite erasing the extremes, there are some wrong values...



#### test - how dependent is the result from old pairs####
#
dat_subset <- subset(dat_subset, breeding_exp_pair_days.z<3) #only 8 observations
mod7 = brms::brm(fledging_mass ~ breeding_exp_pair_days.z +
                   breeding_exp_Dad_days.z+
                   breeding_exp_Mom_days.z+
                   Mom_MinAge.z+
                   Dad_MinAge.z+
                   tarsus_mom.z+
                   tarsus_dad.z+
                   Col_Size.z+
                   N_hatched.z+
                   (1|Season)  + (1|ColNestLaying) + (1|Colony)+
                   (breeding_exp_pair_days.z|Pair) +
                   (breeding_exp_Dad_days.z|BreederDad) + 
                   (breeding_exp_Mom_days.z|BreederMom),
                 control = list(adapt_delta = 0.99, max_treedepth = 10),
                 iter = 8000,
                 cores = 4,
                 #prior = set_prior('normal(0, 3)'),
                 data = dat_subset,
                 file="fledging_mass_nat7a")


#major influence of 8 datapoint, 2 pairs.




#### main variable plot - NAT. - LONG. ####

#extract Plotting values
marg<-marginal_effects(mod6, resolution=1000)
#select the variable of interest
Fledging_mass<-marg$breeding_exp_pair_days.z

#plot

ggplot()+
  geom_line(data = Fledging_mass,
            aes(x=sd(dat_subset$breeding_exp_pair_days)*
                      Fledging_mass$breeding_exp_pair_days.z+
                      mean(dat_subset$breeding_exp_pair_days),
                y=estimate__, ymax=upper__ , ymin=lower__),
            size=1.3, color="#007373")+
  geom_ribbon(data = Fledging_mass,
              aes(x=sd(dat_subset$breeding_exp_pair_days)*
                        Fledging_mass$breeding_exp_pair_days.z+
                        mean(dat_subset$breeding_exp_pair_days),
                  y=estimate__, ymax=upper__, ymin=lower__),
              fill="#007373",alpha=0.3)+
  geom_point(data = dat_subset, 
             aes(x=sd(dat_subset$breeding_exp_pair_days)*
                       dat_subset$breeding_exp_pair_days.z+
                       mean(dat_subset$breeding_exp_pair_days),
                 y=fledging_mass),color="#007373",alpha=0.1) + 
  ylim(19,36)+
  #xlim(0,4)+
  xlab(element_blank())+
  ylab(element_blank())+
  theme(text = element_text(size=20))+
  #theme(legend.position = c(0.9, 0.9))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))

ggsave("fledging_mass_exp_nat6_v3.eps", device=cairo_ps, width = 9, height = 9,
       units ="cm", fallback_resolution = 600)




####Forest Plot - NAT. - LONG. ####

plot_model(mod6,bpe = "mean",type="std2", vline.color = "#007373", line.size = .35, 
           sort.est = TRUE, transform = NULL, show.values = TRUE,
           ci.style="whisker", value.size=1.9,
           prob.inner=0,prob.outer=.95, width=0.2, 
           bpe.style = "dot", bpe.color = "black")+  
  #scale_color_sjplot("simply")+ 
  ylim(-.7, .7)+ 
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

#### FINAL MODEL PROTECTED LONG. ####
mod5 = brms::brm(fledging_mass ~ breeding_exp_pair_days.z +
                   breeding_exp_Dad_days.z+
                   breeding_exp_Mom_days.z+
                   Mom_MinAge.z+
                   Dad_MinAge.z+
                   Col_Size.z+
                   tarsus_mom.z+
                   tarsus_dad.z+
                   N_hatched.z+
                   mo(season_of_protection)+
                   (1|Season)  + (1|ColNestLaying) + (1|Colony)+
                   (breeding_exp_pair_days.z|Pair) +
                   (breeding_exp_Dad_days.z|BreederDad) + 
                   (breeding_exp_Mom_days.z|BreederMom),
                 control = list(adapt_delta = 0.99, max_treedepth = 10),
                 iter = 8000,
                 cores = 4,
                 #prior = set_prior('normal(0, 3)'),
                 data = dat_subset,
                 file="fledging_mass_prot5a")


#Model Check
#has the model converged?
summary(mod5)

plot(mod5)

pp = brms::pp_check(mod5)
pp + theme_bw()


bayes_R2(mod5)
#     Estimate Est.Error      Q2.5     Q97.5
# R2 0.587992 0.0216755 0.5436095 0.6284494  


#### main variable plot - PROT. - LONG.####



#extract Plotting values
marg<-marginal_effects(mod5, resolution=1000)
#select the variable of interest
Fledging_mass<-marg$breeding_exp_pair_days.z

#plot

ggplot()+
  geom_line(data = Fledging_mass, 
            aes(x=sd(dat_subset$breeding_exp_pair_days)*
                      Fledging_mass$breeding_exp_pair_days.z+
                      mean(dat_subset$breeding_exp_pair_days),
                y=estimate__, ymax=upper__ , ymin=lower__),
            size=1.3, color="#D55E00")+
  geom_ribbon(data = Fledging_mass, 
              aes(x=sd(dat_subset$breeding_exp_pair_days)*
                        Fledging_mass$breeding_exp_pair_days.z+
                        mean(dat_subset$breeding_exp_pair_days),
                  y=estimate__, ymax=upper__, ymin=lower__),
              fill="#D55E00",alpha=0.3)+
  geom_point(data = dat_subset, 
             aes(x=sd(dat_subset$breeding_exp_pair_days)*
                       dat_subset$breeding_exp_pair_days.z+
                       mean(dat_subset$breeding_exp_pair_days),
                 y=fledging_mass),color="#D55E00",alpha=0.1) + 
  ylim(19,36)+
  #xlim(0,4)+
  xlab(element_blank())+
  ylab(element_blank())+
  theme(text = element_text(size=20))+
  #theme(legend.position = c(0.9, 0.9))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))

ggsave("fledging_mass_exp_prot5_v3.eps", device=cairo_ps, width = 9, height = 9,
       units ="cm", fallback_resolution = 600)



####Forest Plot - PROT. - LONG. ####
plot_model(mod5,bpe = "mean",type="std2", vline.color = "#D55E00", line.size = .35, 
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
  ylim(-.9, 0.7)+ 
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

dat3<- read.csv("YOUR_DIRECTORY/CrossSectional_dataset_breeding_attempt.csv")


##choose if you want to select natual or protected conditions

#natural codition
dat3_br_attempt<-dat3[,c("fledging_mass", "breeding_exp_pair_days", "predation",
                         "tarsus_Focal", "tarsus_Mate","N_hatched",
                         "sex", "Season", "breeding_exp_Mate_days", 
                         "Colony", "Col_Size","Focal_MinAge", "Mate_MinAge", 
                         "Pair", "ring", "Mate_ID", "season_length_population",
                         "season_length","ColNestLaying", "unique_identifier")]
dat3_br_attempt<- subset(dat3_br_attempt, predation=="natural")

#protected colonies
dat3_br_attempt<-dat3[,c("fledging_mass", "breeding_exp_pair_days", "predation", 
                         "tarsus_Focal", "tarsus_Mate", "N_hatched",
                         "sex", "Season", "breeding_exp_Mate_days", "Colony", "Col_Size",
                         "Focal_MinAge", "Mate_MinAge", "season_of_protection",
                         "Pair", "ring", "Mate_ID", "season_length_population",
                         "season_length","ColNestLaying", "unique_identifier")]
dat3_br_attempt<- subset(dat3_br_attempt, predation=="protected")



#erase the incomplete cases
dat3_br_attempt<-dat3_br_attempt[complete.cases(dat3_br_attempt), ] #select only complete cases

#erase duplicates
dat3_br_attempt<-unique(dat3_br_attempt)


#variable tranformation
dat3_br_attempt$Colony<- as.factor(dat3_br_attempt$Colony)
dat3_br_attempt$sex<- as.factor(dat3_br_attempt$sex)




#erase impossible values fledgling mass (below 19 and over 36)
dat3_br_attempt <- subset(dat3_br_attempt, fledging_mass>19 & fledging_mass<36)

#erase impossible values of tarsus (below 19 and over 36)
dat3_br_attempt <- subset(dat3_br_attempt, tarsus_Focal>19 & tarsus_Focal<25.5)
dat3_br_attempt <- subset(dat3_br_attempt, tarsus_Mate>19 & tarsus_Mate<25.5)


##Scaling
dat3_br_attempt$breeding_exp_Mate_days.z<- scale(dat3_br_attempt$breeding_exp_Mate_days)
dat3_br_attempt$breeding_exp_pair_days.z<- scale(dat3_br_attempt$breeding_exp_pair_days)
dat3_br_attempt$Focal_MinAge.z<- scale(dat3_br_attempt$Focal_MinAge)
dat3_br_attempt$Mate_MinAge.z<- scale(dat3_br_attempt$Mate_MinAge)
dat3_br_attempt$Col_Size.z<- scale(dat3_br_attempt$Col_Size)
dat3_br_attempt$season_length_population.z<-scale(dat3_br_attempt$season_length_population)
dat3_br_attempt$season_length.z <- scale(dat3_br_attempt$season_length)
dat3_br_attempt$tarsus_Focal.z <- scale(dat3_br_attempt$tarsus_Focal)
dat3_br_attempt$tarsus_Mate.z <- scale(dat3_br_attempt$tarsus_Mate)

dat3_br_attempt$N_hatched.z <- scale(dat3_br_attempt$N_hatched)


##NATURAL




#### FINAL MODEL NATURAL CROSS. ####

mod5 = brms::brm(fledging_mass ~ breeding_exp_pair_days.z +
                   breeding_exp_Mate_days.z+
                   Focal_MinAge.z+
                   Mate_MinAge.z+
                   Col_Size.z+
                   tarsus_Focal.z+
                   tarsus_Mate.z+
                   sex+
                   N_hatched.z+
                   (1|Colony) + (1|ColNestLaying) + (1|Season)+
                   (1|ring) + (1|Pair) +(1|Mate_ID),
                 control = list(adapt_delta = 0.9999, max_treedepth = 12),
                 iter = 8000,
                 cores = 4,
                 #prior = set_prior('normal(0, 3)'),
                 data = dat3_br_attempt,
                 file="fledging_mass_3yrs_nat5a")

plot(mod5)
pp_check(mod5)
#good but clearly some mistakes at 22 gr....

bayes_R2(mod5)
#     Estimate  Est.Error      Q2.5     Q97.5
# R2 0.7892223 0.02318008 0.7370873 0.8272768


#### main variable plot - NAT. - CROSS. ####

#extract Plotting values
marg<-marginal_effects(mod5, resolution=1000)

#select the variable of interest
Fledging_mass<-marg$breeding_exp_pair_days.z


ggplot()+
  geom_line(data = Fledging_mass, 
            aes(x=sd(dat3_br_attempt$breeding_exp_pair_days)*
                  Fledging_mass$breeding_exp_pair_days.z+
                  mean(dat3_br_attempt$breeding_exp_pair_days),
                y=estimate__, ymax=upper__ , ymin=lower__),
            size=1.3, color="#007373")+
  geom_ribbon(data = Fledging_mass, 
              aes(x=sd(dat3_br_attempt$breeding_exp_pair_days)*
                    Fledging_mass$breeding_exp_pair_days.z+
                    mean(dat3_br_attempt$breeding_exp_pair_days),
                  y=estimate__, ymax=upper__, ymin=lower__),
              fill="#007373",alpha=0.3)+
  geom_point(data = dat3_br_attempt,
             aes(x=sd(dat3_br_attempt$breeding_exp_pair_days)*
                   dat3_br_attempt$breeding_exp_pair_days.z+
                   mean(dat3_br_attempt$breeding_exp_pair_days),
                 y=fledging_mass), color="#007373",alpha=0.15) +
  ylim(19,36)+
  #xlim(1,3)+
  xlab(element_blank())+
  ylab(element_blank())+
  theme(text = element_text(size=20))+
  #theme(legend.position = c(0.9, 0.9))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))


ggsave("fledging_mass_exp_3yrs_nat5_v1.eps", device=cairo_ps, width = 9, height = 9,
       units ="cm", fallback_resolution = 600)





####Forest Plot - NAT. - CROSS. ####
plot_model(mod5,bpe = "mean",type="std2", vline.color = "#007373", line.size = .35, 
           sort.est = TRUE, transform = NULL, show.values = TRUE,
           ci.style="whisker", value.size=1.9,
           prob.inner=0,prob.outer=.95, width=0.2, 
           bpe.style = "dot", bpe.color = "black")+  
  ylim(-1.5, 1.65)+ 
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


#### FINAL MODEL PROTECTED CROSS. ####

mod6 = brms::brm(fledging_mass ~ breeding_exp_pair_days.z +
                   breeding_exp_Mate_days.z+
                   Focal_MinAge.z+
                   Mate_MinAge.z+
                   Col_Size.z+
                   tarsus_Focal.z+
                   tarsus_Mate.z+
                   sex+
                   N_hatched.z+
                   mo(season_of_protection)+
                   (1|Colony) + (1|ColNestLaying) + (1|Season)+
                   (1|ring) + (1|Pair) +(1|Mate_ID),
                 control = list(adapt_delta = 0.9999, max_treedepth = 13),
                 iter = 8000,
                 cores = 4,
                 #prior = set_prior('normal(0, 3)'),
                 data = dat3_br_attempt,
                 file="fledging_mass_3yrs_prot6a")

plot(mod6)

pp_check(mod6)
#good

bayes_R2(mod6)
#     Estimate  Est.Error      Q2.5     Q97.5
# R2 0.7587881 0.02040656 0.7144247 0.7938451



#### main variable plot - PROT. CROSS. ####
#extract Plotting values
marg<-marginal_effects(mod6, resolution=1000)

#select the variable of interest
Fledging_mass<-marg$breeding_exp_pair_days.z


ggplot()+
  geom_line(data = Fledging_mass, 
            aes(x=sd(dat3_br_attempt$breeding_exp_pair_days)*
                  Fledging_mass$breeding_exp_pair_days.z+
                  mean(dat3_br_attempt$breeding_exp_pair_days),
                y=estimate__, ymax=upper__ , ymin=lower__),
            size=1.3, color="#D55E00")+
  geom_ribbon(data = Fledging_mass, 
              aes(x=sd(dat3_br_attempt$breeding_exp_pair_days)*
                    Fledging_mass$breeding_exp_pair_days.z+
                    mean(dat3_br_attempt$breeding_exp_pair_days),
                  y=estimate__, ymax=upper__, ymin=lower__),
              fill="#D55E00",alpha=0.3)+
  geom_point(data = dat3_br_attempt,
             aes(x=sd(dat3_br_attempt$breeding_exp_pair_days)*
                   dat3_br_attempt$breeding_exp_pair_days.z+
                   mean(dat3_br_attempt$breeding_exp_pair_days),
                 y=fledging_mass), color="#D55E00",alpha=0.15) +
  ylim(19,36)+
  #xlim(1,3)+
  xlab(element_blank())+
  ylab(element_blank())+
  theme(text = element_text(size=20))+
  #theme(legend.position = c(0.9, 0.9))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))

ggsave("fledging_mass_dur_3yrs_prot5_v1.eps", device=cairo_ps, width = 9, height = 9,
       units ="cm", fallback_resolution = 600)





####Forest Plot - PROT. - CROSS. ####
plot_model(mod6,bpe = "mean",type="std2", vline.color = "#D55E00", line.size = .35, 
           sort.est = TRUE, transform = NULL, show.values = TRUE,
           ci.style="whisker", value.size=1.9,
           prob.inner=0,prob.outer=.95, width=0.2, 
           bpe.style = "dot", bpe.color = "black",
           rm.terms = c("simo_moseason_of_protection1.1.",
                        "simo_moseason_of_protection1.2.",
                        "simo_moseason_of_protection1.3.",
                        "simo_moseason_of_protection1.4.",
                        "simo_moseason_of_protection1.5."))+  
  ylim(-1.5, 1.5)+ 
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

