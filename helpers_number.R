#Aim: Potential reasons for increased breeding success in case of long term monogamy: do the number of helpers increase in more experienced pairs?

#Author: P. D'Amelio


# One value per pair per breeding attempt
# Data available only for less seasons (video analysis started later than data collection)

### libraries  ####
library(brms)
library(loo)
library(sjPlot)
library(ggplot2)



##LONGITUDINAL database  #####


#load database
dat<- read.csv("YOUR_DIRECTORY/Longitudinal_dataset.csv")


#I called it group size but is actually number of helpers

#for natural colonies you MUST use this (otherwise all NAs in season of protection)
dat_subset<-dat[,c( "Group_Size","breeding_exp_pair_days",
                    "Pair", "Season", "predation", "breeding_exp_Mom_days", "breeding_exp_Dad_days",
                    "Col_Size", "Mom_MinAge", "Dad_MinAge", "Colony",
                    "BreederMom", "BreederDad")] 
dat_subset<-subset(dat_subset, predation=="natural")




#use this for protected colonies
dat_subset<-dat[,c( "Group_Size","breeding_exp_pair_days",
                    "Pair", "Season", "predation", "breeding_exp_Mom_days", "breeding_exp_Dad_days",
                    "Col_Size", "Mom_MinAge", "Dad_MinAge", "Colony",
                    "BreederMom", "BreederDad", "season_of_protection")] 
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

dat_subset$Group_Size.r<-round(dat_subset$Group_Size)




## Natural conditions
# I compared 7 models
# 4 model families  compared (skew_normal, normal, negative binomial, poisson)
# to run poisson and negative binomial I rounded number of helpers and ...
#...to compare with normal and skew normal I rounded for these functions too...
#..but then I kept the original helper number distribution to have more precise estimates
# 1- skew normal with truncation 
# 2- skew normal
# 3- normal
# 4- poisson (rounded group size)
# 5- negative binomial (rounded group size)
# 6- normal (rounded group size)
# 7- skew normal (rounded group size)


mod1 = brms::brm(Group_Size| trunc(lb = 0) ~ breeding_exp_pair_days.z+
                   breeding_exp_Mom_days.z+ breeding_exp_Dad_days.z+
                  Mom_MinAge.z + Dad_MinAge.z + Col_Size.z + 
                  (1|Colony)+ (1| Season)+ (breeding_exp_pair_days.z|Pair) +
                  (breeding_exp_Dad_days.z|BreederDad) + 
                   (breeding_exp_Mom_days.z|BreederMom),
                control = list(adapt_delta = 0.9, max_treedepth = 15),
                iter = 6000,
                cores = 8,
                #prior = set_prior('normal(0, 3)'),
                data = dat_subset,
                family="skew_normal",
                file="Nr_helpers_nat1")
#The model has not converged (some Rhats are > 1.1)

#part of the problem is the truncation, erase it and run again


#no trunc, 8000 iteractions
#### FINAL MODEL NATURAL - LONG. ####
mod2 = brms::brm(Group_Size ~ breeding_exp_pair_days.z+
                   breeding_exp_Mom_days.z+ breeding_exp_Dad_days.z+
                   Mom_MinAge.z + Dad_MinAge.z + Col_Size.z + 
                   (1|Colony)+ (1| Season)+ (breeding_exp_pair_days.z|Pair) +
                   (breeding_exp_Dad_days.z|BreederDad) + 
                   (breeding_exp_Mom_days.z|BreederMom),
                 control = list(adapt_delta = 0.99, max_treedepth = 12),
                 iter = 8000,
                 cores = 8,
                 #prior = set_prior('normal(0, 3)'),
                 data = dat_subset,
                 family="skew_normal",
                 file="Nr_helpers_nat2")

mod2<-add_criterion(mod2, "loo", cores=4)#,reloo=TRUE
#Found 93 observations with a pareto_k > 0.7 in model 'mod2'


pp_check(mod2)
#okish

plot(mod2)

bayes_R2(mod2)
#     Estimate  Est.Error      Q2.5     Q97.5
# R2 0.5180729 0.04039343 0.4323514 0.5916988



#normal
mod3 = brms::brm(Group_Size ~ breeding_exp_pair_days.z+
                   breeding_exp_Mom_days.z+ breeding_exp_Dad_days.z+
                   Mom_MinAge.z + Dad_MinAge.z + Col_Size.z + 
                   (1|Colony)+ (1| Season)+ (breeding_exp_pair_days.z|Pair) +
                   (breeding_exp_Dad_days.z|BreederDad) + 
                   (breeding_exp_Mom_days.z|BreederMom),
                 control = list(adapt_delta = 0.99, max_treedepth = 12),
                 iter = 6000,
                 cores = 8,
                 #prior = set_prior('normal(0, 3)'),
                 data = dat_subset,
                 #family="skew_normal",
                 file="Nr_helpers_nat3")

mod3<-add_criterion(mod3, "loo", cores=4)#,reloo=TRUE
#Found 17 observations with a pareto_k > 0.7 



#round group size and use poisson
dat_subset$Group_Size.r<-round(dat_subset$Group_Size)
mod4 = brms::brm(Group_Size.r ~ breeding_exp_pair_days.z+
                   breeding_exp_Mom_days.z+ breeding_exp_Dad_days.z+
                   Mom_MinAge.z + Dad_MinAge.z + Col_Size.z + 
                   (1|Colony)+ (1| Season)+ (breeding_exp_pair_days.z|Pair) +
                   (breeding_exp_Dad_days.z|BreederDad) + 
                   (breeding_exp_Mom_days.z|BreederMom),
                 control = list(adapt_delta = 0.999, max_treedepth = 10),
                 iter = 8000,
                 cores = 8,
                 #prior = set_prior('normal(0, 3)'),
                 data = dat_subset,
                 family="poisson",
                 file="Nr_helpers_nat4")
mod4<-add_criterion(mod4, "loo",reloo=TRUE, cores=4)

plot_model(mod4,type="std2", vline.color = "black", show.values = TRUE,sort.est = TRUE,
           digits = 3, transform = NULL)+ ylim(-.4, .4) + theme_bw()

bayes_R2(mod4)
#    Estimate  Est.Error      Q2.5     Q97.5
# R2 0.366737 0.05442002 0.2584181 0.4686612


pp_check(mod4)





# negbinomial
mod5 = brms::brm(Group_Size.r ~ breeding_exp_pair_days.z+
                   breeding_exp_Mom_days.z+ breeding_exp_Dad_days.z+
                   Mom_MinAge.z + Dad_MinAge.z + Col_Size.z + 
                   (1|Colony)+ (1| Season)+ (breeding_exp_pair_days.z|Pair) +
                   (breeding_exp_Dad_days.z|BreederDad) + 
                   (breeding_exp_Mom_days.z|BreederMom),
                 control = list(adapt_delta = 0.999, max_treedepth = 10),
                 iter = 8000,
                 cores = 8,
                 #prior = set_prior('normal(0, 3)'),
                 data = dat_subset,
                 family="negbinomial",
                 file="Nr_helpers_nat5")

mod5<-add_criterion(mod5, "loo",reloo=TRUE, cores=4)


pp_check(mod5)

##run normal and skew normal again with rounded group size 
#to compare information criteria
#normal
mod6 = brms::brm(Group_Size.r ~ breeding_exp_pair_days.z+
                   breeding_exp_Mom_days.z+ breeding_exp_Dad_days.z+
                   Mom_MinAge.z + Dad_MinAge.z + Col_Size.z + 
                   (1|Colony)+ (1| Season)+ (breeding_exp_pair_days.z|Pair) +
                   (breeding_exp_Dad_days.z|BreederDad) + 
                   (breeding_exp_Mom_days.z|BreederMom),
                 control = list(adapt_delta = 0.999, max_treedepth = 10),
                 iter = 8000,
                 cores = 4,
                 #prior = set_prior('normal(0, 3)'),
                 data = dat_subset,
                 #family="poisson",
                 file="Nr_helpers_nat6")

mod6<-add_criterion(mod6, "loo", cores=4)#,reloo=TRUE
#Found 16 observations with a pareto_k > 0.7 in model 'mod6'

pp_check(mod6)



#skew normal
mod7 = brms::brm(Group_Size.r ~ breeding_exp_pair_days.z+
                   breeding_exp_Mom_days.z+ breeding_exp_Dad_days.z+
                   Mom_MinAge.z + Dad_MinAge.z + Col_Size.z + 
                   (1|Colony)+ (1| Season)+ (breeding_exp_pair_days.z|Pair) +
                   (breeding_exp_Dad_days.z|BreederDad) + 
                   (breeding_exp_Mom_days.z|BreederMom),
                 control = list(adapt_delta = 0.999, max_treedepth = 10),
                 iter = 8000,
                 cores = 4,
                 #prior = set_prior('normal(0, 3)'),
                 data = dat_subset,
                 family="skew_normal",
                 file="Nr_helpers_nat7")


mod7<-add_criterion(mod7, "loo", cores=4)#,reloo=TRUE
#Found 76 observations with a pareto_k > 0.7 in model 'mod7'

pp_check(mod7)


loo_compare(mod2, mod3, mod4, mod2a, mod5, mod6, mod7)
#skew normal models much better
#I choose the one without rounding because estimates are probably more precise


bayes_R2(mod7)
#     Estimate  Est.Error      Q2.5     Q97.5
# R2 0.4855108 0.04447261 0.3918054 0.5661564






#### main variable plot - NAT. - LONG.  ####

#extract Plotting values
marg<-marginal_effects(mod2, resolution=1000)

#select the variable of interest
Nr_helpers<-marg$breeding_exp_pair_days.z



ggplot()+
 geom_line(data=Nr_helpers,aes(x=sd(dat_subset$breeding_exp_pair_days)*
                                   Nr_helpers$breeding_exp_pair_days.z+
                                   mean(dat_subset$breeding_exp_pair_days),
                               y=estimate__, ymax=upper__ , ymin=lower__),
           size=1.3, color="#007373")+
 geom_ribbon(data=Nr_helpers,aes(x=sd(dat_subset$breeding_exp_pair_days)*
                                     Nr_helpers$breeding_exp_pair_days.z+
                                       mean(dat_subset$breeding_exp_pair_days),
                                 y=estimate__, ymax=upper__, ymin=lower__),
             fill="#007373",alpha=0.3)+
 geom_point(data=dat_subset,aes(x=sd(dat_subset$breeding_exp_pair_days)*
                                      dat_subset$breeding_exp_pair_days.z+
                                      mean(dat_subset$breeding_exp_pair_days),
                                y=Group_Size), 
             color="#007373",alpha=0.1) + 
 ylim(-0.2,7.2)+
 #xlim(0,1400)+
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


ggsave("Nr_helpers_exp_nat2_v5.eps", device=cairo_ps, width = 9, height = 9,
       units ="cm", fallback_resolution = 600)




####Forest Plot - NAT. -  LONG. ####

plot_model(mod2,type="std2", vline.color = "#007373", line.size = .35, 
           sort.est = TRUE, transform = NULL, show.values = TRUE,
           ci.style="whisker", value.size=1.9,
           prob.inner=0,prob.outer=.95, width=0.2, 
           bpe.style = "dot", bpe.color = "black")+  
  ylim(-.5, .51)+ 
  theme(text=element_blank())+
  theme(panel.grid = element_blank())+
  theme(panel.background = element_blank())+
  theme(panel.background = element_rect(color = NA)) + #basic theme
  theme( plot.title = element_blank())+
  theme(axis.text.y=element_blank(),axis.ticks.y = element_blank())+    #remove the label
  theme(axis.text.x=element_blank()) +
  theme(axis.line.x = element_line())

## dimensions: 130 - 180




##PROTECTED

## Natural conditions
# I compared 4 models
# 3 model families  compared (skew_normal, normal, poisson)
# to run poisson I rounded number of helpers and ...
#...to compare with normal and skew normal I rounded for these functions too...
#

# 1- poisson (rounded group size)
# 2- normal (rounded group size)
# 3- skew normal (rounded group size)
# 4- skew normal 







#### FINAL MODEL PROT. - LONG. ####
dat_subset$Group_Size.r<-round(dat_subset$Group_Size)
mod1 = brms::brm(Group_Size.r ~ breeding_exp_pair_days.z+
                   breeding_exp_Mom_days.z+ breeding_exp_Dad_days.z+
                   Mom_MinAge.z + Dad_MinAge.z + Col_Size.z + 
                   mo(season_of_protection)+
                   (1|Colony)+ (1| Season)+ (breeding_exp_pair_days.z|Pair) +
                   (breeding_exp_Dad_days.z|BreederDad) + 
                   (breeding_exp_Mom_days.z|BreederMom),
                 control = list(adapt_delta = 0.999, max_treedepth = 10),
                 iter = 8000,
                 cores = 8,
                 #prior = set_prior('normal(0, 3)'),
                 data = dat_subset,
                 family="poisson",
                 file="Nr_helpers_prot1")
mod1<-add_criterion(mod1, "loo",reloo=TRUE, cores=4)

pp_check(mod1)



bayes_R2(mod1)
#     Estimate  Est.Error      Q2.5     Q97.5
# R2 0.4365113 0.04967667 0.3350999 0.5269586




#gaussian model 
mod2 = brms::brm(Group_Size.r ~ breeding_exp_pair_days.z+
                   breeding_exp_Mom_days.z+ breeding_exp_Dad_days.z+
                   Mom_MinAge.z + Dad_MinAge.z + Col_Size.z + 
                   mo(season_of_protection)+
                   (1|Colony)+ (1| Season)+ (breeding_exp_pair_days.z|Pair) +
                   (breeding_exp_Dad_days.z|BreederDad) + 
                   (breeding_exp_Mom_days.z|BreederMom),
                 control = list(adapt_delta = 0.999, max_treedepth = 12),
                 iter = 8000,
                 cores = 8,
                 #prior = set_prior('normal(0, 3)'),
                 data = dat_subset,
                 #family="poisson",
                 file="Nr_helpers_prot2")
pp_check(mod2)


mod2<-add_criterion(mod2, "loo", cores=4)#,reloo=TRUE



#skew_normal
mod3 = brms::brm(Group_Size.r ~ breeding_exp_pair_days.z+
                   breeding_exp_Mom_days.z+ breeding_exp_Dad_days.z+
                   Mom_MinAge.z + Dad_MinAge.z + Col_Size.z + 
                   mo(season_of_protection)+
                   (1|Colony)+ (1| Season)+ (breeding_exp_pair_days.z|Pair) +
                   (breeding_exp_Dad_days.z|BreederDad) + 
                   (breeding_exp_Mom_days.z|BreederMom),
                 control = list(adapt_delta = 0.999, max_treedepth = 10),
                 iter = 8000,
                 cores = 4,
                 #prior = set_prior('normal(0, 3)'),
                 data = dat_subset,
                 family="skew_normal",
                 file="Nr_helpers_prot3")

pp_check(mod3)


mod3<-add_criterion(mod3, "loo", cores=4)#,reloo=TRUE
#Found 43 observations with a pareto_k > 0.7 in model 'mod3'


bayes_R2(mod3)
#     Estimate  Est.Error      Q2.5     Q97.5
# R2 0.4508488 0.04497697 0.3599118 0.5345982


loo_compare(mod1,mod2, mod3)
#skew normal better but lots of overlap with poisson

#skew normal but do not round group size
mod4 = brms::brm(Group_Size ~ breeding_exp_pair_days.z+
                   breeding_exp_Mom_days.z+ breeding_exp_Dad_days.z+
                   Mom_MinAge.z + Dad_MinAge.z + Col_Size.z + 
                   mo(season_of_protection)+
                   (1|Colony)+ (1| Season)+ (breeding_exp_pair_days.z|Pair) +
                   (breeding_exp_Dad_days.z|BreederDad) + 
                   (breeding_exp_Mom_days.z|BreederMom),
                 control = list(adapt_delta = 0.999, max_treedepth = 12),
                 iter = 8000,
                 cores = 4,
                 #prior = set_prior('normal(0, 3)'),
                 data = dat_subset,
                 family="skew_normal",
                 file="Nr_helpers_prot4")

pp_check(mod4)


mod4<-add_criterion(mod4, "loo", cores=4)#,reloo=TRUE
#Found 47 observations with a pareto_k > 0.7 in model 'mod4'


bayes_R2(mod4)
# Estimate  Est.Error      Q2.5    Q97.5
# R2 0.4326616 0.04422691 0.3423665 0.515768


loo_compare(mod1,mod2, mod3, mod4)


#R-squared of all models are quite similar,
#given the uncertainty about the best fitting link function,
#I choose the poisson model because it does not contain negative estimates




#### main variable plot - PROT. - LONG. ####

#extract Plotting values
marg<-marginal_effects(mod1, resolution=1000)

#select the variable of interest
Nr_helpers<-marg$breeding_exp_pair_days.z



ggplot()+
  geom_line(data=Nr_helpers,aes(x=sd(dat_subset$breeding_exp_pair_days)*
                                      Nr_helpers$breeding_exp_pair_days.z+
                                      mean(dat_subset$breeding_exp_pair_days),
                                y=estimate__, ymax=upper__ , ymin=lower__),
            size=1.3, color="#D55E00")+
  geom_ribbon(data=Nr_helpers,aes(x=sd(dat_subset$breeding_exp_pair_days)*
                                        Nr_helpers$breeding_exp_pair_days.z+
                                        mean(dat_subset$breeding_exp_pair_days),
                                  y=estimate__, ymax=upper__, ymin=lower__),
              fill="#D55E00",alpha=0.3)+
  geom_point(data=dat_subset,aes(x=sd(dat_subset$breeding_exp_pair_days)*
                                       dat_subset$breeding_exp_pair_days.z+
                                       mean(dat_subset$breeding_exp_pair_days),
                                 y=jitter(Group_Size.r)), 
             color="#D55E00",alpha=0.1) + 
  ylim(-0.2,7.2)+
  #xlim(0,4)+
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

ggsave("Nr_helpers_exp_prot1_v5.eps", device=cairo_ps, width = 9, height = 9,
       units ="cm", fallback_resolution = 600)




####Forest Plot - PROT. - LONG. ####


plot_model(mod1,type="std2", vline.color = "#D55E00", line.size = .35, 
           sort.est = TRUE, transform = NULL, show.values = TRUE,
           ci.style="whisker", value.size=1.9,
           prob.inner=0,prob.outer=.95, width=0.2, 
           bpe.style = "dot", bpe.color = "black",
           rm.terms = c("simo_moseason_of_protection1.1.",
                        "simo_moseason_of_protection1.2.",
                        "simo_moseason_of_protection1.3.",
                        "simo_moseason_of_protection1.4.",
                        "simo_moseason_of_protection1.5."))+  
  ylim(-.4, .4)+ 
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
dat3_br_attempt<-dat3[,c("Group_Size", "breeding_exp_pair_days", "predation",  
                                    "sex", "Season", "breeding_exp_Mate_days", "Colony", "Col_Size",
                                    "Focal_MinAge", "Mate_MinAge", 
                                    "Pair", "ring", "Mate_ID","ColNestLaying")]
dat3_br_attempt<- subset(dat3_br_attempt, predation=="natural")

#protected colonies
dat3_br_attempt<-dat3[,c("Group_Size", "breeding_exp_pair_days", "predation", 
                                    "sex", "Season", "breeding_exp_Mate_days", "Colony", "Col_Size",
                                    "Focal_MinAge", "Mate_MinAge", "season_of_protection",
                                    "Pair", "ring", "Mate_ID","ColNestLaying")]
dat3_br_attempt<- subset(dat3_br_attempt, predation=="protected")



#erase the incomplete cases
dat3_br_attempt<-dat3_br_attempt[complete.cases(dat3_br_attempt), ] 

#erase duplicates
dat3_br_attempt<-unique(dat3_br_attempt)


#variable tranformation
dat3_br_attempt$Colony<- as.factor(dat3_br_attempt$Colony)
dat3_br_attempt$sex<- as.factor(dat3_br_attempt$sex)
dat3_br_attempt$Season<-factor(dat3_br_attempt$Season)

##Scaling
dat3_br_attempt$breeding_exp_Mate_days.z<- scale(dat3_br_attempt$breeding_exp_Mate_days)
dat3_br_attempt$breeding_exp_pair_days.z<- scale(dat3_br_attempt$breeding_exp_pair_days)
dat3_br_attempt$Focal_MinAge.z<- scale(dat3_br_attempt$Focal_MinAge)
dat3_br_attempt$Mate_MinAge.z<- scale(dat3_br_attempt$Mate_MinAge)
dat3_br_attempt$Col_Size.z<- scale(dat3_br_attempt$Col_Size)

#round group size and use poisson
dat3_br_attempt$Group_Size.r<-round(dat3_br_attempt$Group_Size)





##NATURAL

#a lot of variations between season poorly explained by all models

## Natural conditions
# I compared 8 models
# 4 model families  compared (poisson, student, normal, skew normal)
# to run poisson I rounded number of helpers and ...
#...to compare with normal and skew normal I rounded for these functions too...
#..but then I kept the original helper number distribution to have more precise estimates.
# I also had to simplify the model. I tried to exclude sex and mate ID, 
# but the problematic variable is season.
# we are simply not able to include the variation between season 
# with our limited database. I excluded it.
# 1- full model, poisson with random slopes
# 2- full model, remove random slopes
# 3- family student remove mate ID and sex
# 4- family normal remove mate ID and sex
# 5- family normal remove mate ID
# 6- family skew normal remove Mate ID
# 7- family skew normal run on the non-rounded group size for better estimates
# 8- family skew normal remove season for convergence




mod1 = brms::brm(Group_Size.r ~ breeding_exp_pair_days.z+
                   breeding_exp_Mate_days.z+
                   Focal_MinAge.z + Mate_MinAge.z + Col_Size.z + sex+
                   (1|Colony)+ (1| Season)+ (breeding_exp_pair_days.z|Pair) +
                   (1|ring) + 
                   (breeding_exp_Mate_days.z|Mate_ID),
                 control = list(adapt_delta = 0.9999, max_treedepth = 12),
                 iter = 10000,
                 cores = 8,
                 #prior = set_prior('normal(0, 3)'),
                 data = dat3_br_attempt,
                 family="poisson",
                 file="Nr_helpers_3yrs_nat1")
#There were 46 divergent transitions after warmup

mod1<-add_criterion(mod1, "loo",reloo=TRUE, cores=4)

plot(mod1)

pp_check(mod1)




#remove random slopes
mod2 = brms::brm(Group_Size.r ~ breeding_exp_pair_days.z+
                   breeding_exp_Mate_days.z+
                   Focal_MinAge.z + Mate_MinAge.z + Col_Size.z + sex+
                   (1|Colony)+ (1| Season)+ (1|Pair) +
                   (1|ring) + 
                   (1|Mate_ID),
                 control = list(adapt_delta = 0.9999, max_treedepth = 12),
                 iter = 10000,
                 cores = 8,
                 #prior = set_prior('normal(0, 3)'),
                 data = dat3_br_attempt,
                 family="poisson",
                 file="Nr_helpers_3yrs_nat2")
#There were 11 divergent transitions after warmup.

mod2<-add_criterion(mod2, "loo",reloo=TRUE, cores=4)


plot(mod2)

pp_check(mod2)


#family student remove mate ID and sex
mod3 = brms::brm(Group_Size.r ~ breeding_exp_pair_days.z+
                   breeding_exp_Mate_days.z+
                   Focal_MinAge.z + Mate_MinAge.z +
                   Col_Size.z + #sex+
                   (1|Colony)+ (1| Season)+ 
                   (1|Pair)+ (1|ring),  
                   #(1|Mate_ID),
                 control = list(adapt_delta = 0.9999, max_treedepth = 12),
                 iter = 8000,
                 cores = 4,
                 #prior = set_prior('normal(0, 3)'),
                 data = dat3_br_attempt,
                 family="student",
                 file="Nr_helpers_3yrs_nat7")
#There were XX divergent transitions after warmup.


mod3<-add_criterion(mod3, "loo", cores=4) #,reloo=TRUE
#Found 6 observations with a pareto_k > 0.7 in model 'mod7'


plot(mod3)

pp_check(mod3)



#family normal remove mate ID and sex
mod4 = brms::brm(Group_Size.r ~ breeding_exp_pair_days.z+
                   breeding_exp_Mate_days.z+
                   Focal_MinAge.z + Mate_MinAge.z +
                   Col_Size.z + #sex+
                   (1|Colony)+ (1| Season)+ 
                   (1|Pair)+ (1|ring),  
                 #(1|Mate_ID),
                 control = list(adapt_delta = 0.9999, max_treedepth = 12),
                 iter = 8000,
                 cores = 4,
                 #prior = set_prior('normal(0, 3)'),
                 data = dat3_br_attempt,
                 file="Nr_helpers_3yrs_nat8")
#There were 31 divergent transitions after warmup.

mod4<-add_criterion(mod4, "loo", cores=4) #,reloo=TRUE

plot(mod4)

pp_check(mod4)


loo_compare(mod2,mod3, mod4)
#normal better than poisson and student


#family normal remove mate ID
mod5 = brms::brm(Group_Size.r ~ breeding_exp_pair_days.z+
                   breeding_exp_Mate_days.z+
                   Focal_MinAge.z + Mate_MinAge.z +
                   Col_Size.z + sex+ 
                   (1|Colony)+  (1|Season)+
                   (1|Pair)+ (1|ring),#+  
                   #(1|Mate_ID),
                 control = list(adapt_delta = 0.9999, max_treedepth = 12),
                 iter = 8000,
                 #warmup = 8000, 
                 cores = 4,
                 #prior = set_prior('normal(0, 3)'),
                 data = dat3_br_attempt,
                 family="normal",
                 file="Nr_helpers_3yrs_nat9")
#There were 2 divergent transitions after warmup.


mod5<-add_criterion(mod5, "loo", cores=4) #,reloo=TRUE
#Found 10 observations with a pareto_k > 0.7 in model 'mod9'


plot(mod5)

pp_check(mod5)

bayes_R2(mod5)
#     Estimate  Est.Error      Q2.5     Q97.5
# R2 0.5798725 0.05176916 0.4621868 0.6655685


### family skew normal remove Mate ID
mod6 = brms::brm(Group_Size.r ~ breeding_exp_pair_days.z+
                   breeding_exp_Mate_days.z+
                   Focal_MinAge.z + Mate_MinAge.z +
                   Col_Size.z + sex+ 
                   (1|Colony)+  (1|Season)+
                   (1|Pair)+ (1|ring),#+  
                 #(1|Mate_ID),
                 control = list(adapt_delta = 0.9999, max_treedepth = 15),
                 iter = 8000,
                 #warmup = 8000, 
                 cores = 4,
                 #prior = set_prior('normal(0, 3)'),
                 data = dat3_br_attempt,
                 family="skew_normal",
                 file="Nr_helpers_3yrs_nat10")
#There were 70 divergent transitions after warmup.


mod6<-add_criterion(mod6, "loo", cores=4) #,reloo=TRUE
#Found 14 observations with a pareto_k > 0.7 in model 'mod10'

plot(mod6)

pp_check(mod6)

bayes_R2(mod6)
# Estimate  Est.Error      Q2.5     Q97.5
# R2 0.5987407 0.06455278 0.4592536 0.6906002

loo_compare(mod5, mod6)
#skew normal much better



#run on the non-rounded group size for better estimates
mod7 = brms::brm(Group_Size ~ breeding_exp_pair_days.z+
                    breeding_exp_Mate_days.z+
                    Focal_MinAge.z + Mate_MinAge.z +
                    Col_Size.z + sex+ 
                    (1|Colony)+  (1|Season)+
                    (1|Pair)+ (1|ring)+  
                   (1|Mate_ID),
                  control = list(adapt_delta = 0.9999, max_treedepth = 12.5),
                  iter = 8000, 
                  cores = 4,
                  #prior = set_prior('normal(0, 3)'),
                  data = dat3_br_attempt,
                  family="skew_normal",
                  file="Nr_helpers_3yrs_nat10a")
#There were 39 divergent transitions after warmup.

mod7<-add_criterion(mod7, "loo", cores=4) #,reloo=TRUE
#Found 22 observations with a pareto_k > 0.7 in model 'mod10a'

bayes_R2(mod7)
#     Estimate  Est.Error      Q2.5     Q97.5
# R2 0.6730763 0.03618932 0.5806322 0.7238625


#run without season to achieve convergence (and with the most complete model as possible)
#### FINAL MODEL NATURAL CROSS. ####
mod8 = brms::brm(Group_Size ~ breeding_exp_pair_days.z+
                     breeding_exp_Mate_days.z+
                     Focal_MinAge.z + Mate_MinAge.z +
                     Col_Size.z + sex+ 
                     (1|Colony)+  #(1|Season)+
                     (1|Pair)+ (1|ring)+  
                     (1|Mate_ID),
                   control = list(adapt_delta = 0.9999, max_treedepth = 12.5),
                   iter = 8000, 
                   cores = 4,
                   #prior = set_prior('normal(0, 3)'),
                   data = dat3_br_attempt,
                   family="skew_normal",
                   file="Nr_helpers_3yrs_nat10a1")

plot(mod8)
pp_check(mod8)
#not so horrible

mod8<-add_criterion(mod8, "loo", cores=4) #,reloo=TRUE
#Found 18 observations with a pareto_k > 0.7 in model 'mod10aa'

loo_compare(mod7,mod8)
#model with season better, but convergence more important.
#we are simply not able to include the variation between season 
#with our limited database.



bayes_R2(mod8)
#     Estimate  Est.Error      Q2.5     Q97.5
# R2 0.6670639 0.03896471 0.5651646 0.7220362
#very high and comparable to the model with season





#### main variable plot - NAT. - CROSS. ####

#extract Plotting values
marg<-marginal_effects(mod8, resolution=1000)
#select the variable of interest
Nr_helpers<-marg$breeding_exp_pair_days.z

#plot

ggplot()+
  geom_line(data=Nr_helpers,aes(x=sd(dat3_br_attempt$breeding_exp_pair_days)*
                                      Nr_helpers$breeding_exp_pair_days.z+
                                      mean(dat3_br_attempt$breeding_exp_pair_days),
                                y=estimate__, ymax=upper__ , ymin=lower__),
            size=1.3, color="#007373")+
  geom_ribbon(data=Nr_helpers,aes(x=sd(dat3_br_attempt$breeding_exp_pair_days)*
                                        Nr_helpers$breeding_exp_pair_days.z+
                                        mean(dat3_br_attempt$breeding_exp_pair_days),
                                  y=estimate__, ymax=upper__, ymin=lower__),
              fill="#007373",alpha=0.3)+
  geom_point(data=dat3_br_attempt,aes(x=sd(dat3_br_attempt$breeding_exp_pair_days)*
                                            dat3_br_attempt$breeding_exp_pair_days.z+
                                       mean(dat3_br_attempt$breeding_exp_pair_days),
                                 y=Group_Size), 
             color="#007373",alpha=0.1) + 
  ylim(-0.4,6.5)+
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




ggsave("Nr_helpers_exp_3y_nat10aa_v3.eps", device=cairo_ps, width = 9, height = 9,
       units ="cm", fallback_resolution = 600)




####Forest Plot - NAT. - CROSS. ####


plot_model(mod8,type="std2", vline.color = "#007373", line.size = .35, 
           sort.est = TRUE, transform = NULL, show.values = TRUE,
           ci.style="whisker", value.size=1.9,
           prob.inner=0,prob.outer=.95, width=0.2, 
           bpe.style = "dot", bpe.color = "black")+  
  ylim(-.4, .75)+ 
  theme(text=element_blank())+
  theme(panel.grid = element_blank())+
  theme(panel.background = element_blank())+
  theme(panel.background = element_rect(color = NA)) + #basic theme
  theme( plot.title = element_blank())+
  theme(axis.text.y=element_blank(),axis.ticks.y = element_blank())+    #remove the label
  theme(axis.text.x=element_blank()) +
  theme(axis.line.x = element_line())

## dimensions: 130 - 180






#PROTECTED


#### FINAL MODEL PROTECTED CROSS. ####
mod7 = brms::brm(Group_Size ~ breeding_exp_pair_days.z+
                   breeding_exp_Mate_days.z+
                   Focal_MinAge.z + Mate_MinAge.z +
                   Col_Size.z + sex+
                   mo(season_of_protection)+
                   (1|Colony)+ (1|Pair) +
                   (1|ring) + (1|Season)+
                   (1|Mate_ID),
                 control = list(adapt_delta = 0.9999, max_treedepth = 12),
                 iter = 8000,
                 cores = 4,
                 #prior = set_prior('normal(0, 3)'),
                 data = dat3_br_attempt,
                 family="skew_normal",
                 file="Nr_helpers_3yrs_prot7")


pp_check(mod7)
#good

bayes_R2(mod7)
# Estimate  Est.Error      Q2.5     Q97.5
# R2 0.6021897 0.06958942 0.4449609 0.7185562



#### main variable plot - PROT. - CROSS. ####

#extract Plotting values
marg<-marginal_effects(mod7, resolution=1000)

#select the variable of interest
Nr_helpers<-marg$breeding_exp_pair_days.z

#plot
ggplot()+
  geom_line(data=Nr_helpers,aes(x=sd(dat3_br_attempt$breeding_exp_pair_days)*
                                      Nr_helpers$breeding_exp_pair_days.z+
                                      mean(dat3_br_attempt$breeding_exp_pair_days),
                                y=estimate__, ymax=upper__ , ymin=lower__),
            size=1.3, color="#D55E00")+
  geom_ribbon(data=Nr_helpers,aes(x=sd(dat3_br_attempt$breeding_exp_pair_days)*
                                        Nr_helpers$breeding_exp_pair_days.z+
                                        mean(dat3_br_attempt$breeding_exp_pair_days),
                                  y=estimate__, ymax=upper__, ymin=lower__),
              fill="#D55E00",alpha=0.3)+
  geom_point(data=dat3_br_attempt,aes(x=sd(dat3_br_attempt$breeding_exp_pair_days)*
                                       dat3_br_attempt$breeding_exp_pair_days.z+
                                       mean(dat3_br_attempt$breeding_exp_pair_days),
                                 y=Group_Size), 
             color="#D55E00",alpha=0.1) + 
  ylim(-0.4,6.5)+
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




ggsave("Nr_helpers_exp_3y_prot7_v3.eps", device=cairo_ps, width = 9, height = 9,
       units ="cm", fallback_resolution = 600)





####Forest Plot - PROT - CROSS-. ####


plot_model(mod7,type="std2", vline.color = "#D55E00", line.size = .35, 
           sort.est = TRUE, transform = NULL, show.values = TRUE,
           ci.style="whisker", value.size=1.9,
           prob.inner=0,prob.outer=.95, width=0.2, 
           bpe.style = "dot", bpe.color = "black",
           rm.terms = c("simo_moseason_of_protection1.1.",
                        "simo_moseason_of_protection1.2.",
                        "simo_moseason_of_protection1.3.",
                        "simo_moseason_of_protection1.4.",
                        "simo_moseason_of_protection1.5."))+   
  ylim(-1, 1.2)+ 
  theme(text=element_blank())+
  theme(panel.grid = element_blank())+
  theme(panel.background = element_blank())+
  theme(panel.background = element_rect(color = NA)) + #basic theme
  theme( plot.title = element_blank())+
  theme(axis.text.y=element_blank(),axis.ticks.y = element_blank())+    #remove the label
  theme(axis.text.x=element_blank()) +
  theme(axis.line.x = element_line())


#dimensions 130-180




