#Aim: Potential reasons for increased breeding success in case of long term monogamy: is nestling survival higher?
#Author: P. D'Amelio

#Nestling survival is intended from hatching, not from the egg stage.

#### libraries  ####
library(brms)
library(loo)
library(sjPlot)
library(ggplot2)



##LONGITUDINAL database  #####


#load database
dat<- read.csv("YOUR_DIRECTORY/Longitudinal_dataset.csv")


#CHOOSE natural OR protected colonies

#I will run bernoulli models (binomials with only 0 and 1), each row an egg, 1 if it hatched, 0 if it failed.

#I run 2 analysis, one for protected and one for natural colonies.
# The 2 databases do not include the same data, subset them before scaling.

##Divide database between natural and protected

#for natural colonies you MUST use this (otherwise all NAs in season of protection)
dat_subset<-dat[,c( "succ_from_hatch","breeding_exp_pair_days",
                    "Pair", "Season", "predation", "breeding_exp_Mom_days", "breeding_exp_Dad_days",
                    "Col_Size", "Mom_MinAge", "Dad_MinAge", "Colony",
                    "BreederMom", "BreederDad", "season_length","ColNestLaying", "unique_identifier")] 
dat_subset<-subset(dat_subset, predation=="natural")




#use this for protected colonies
dat_subset<-dat[,c( "succ_from_hatch","breeding_exp_pair_days",
                    "Pair", "Season", "predation", "breeding_exp_Mom_days", "breeding_exp_Dad_days",
                    "Col_Size", "Mom_MinAge", "Dad_MinAge", "Colony",
                    "BreederMom", "BreederDad", "season_length", 
                    "season_of_protection","ColNestLaying", "unique_identifier")] 
dat_subset<-subset(dat_subset, predation=="protected")





##Now the databases follow the same path
dat_subset<- unique(dat_subset) 

dat_subset<-dat_subset[complete.cases(dat_subset), ] #select only complete cases


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

# FULL DATABASE
# NATURAL


#### FINAL MODEL NATURAL LONG. ####
#I fit a binomial (bernoulli) 
#bernoulli is just a special case of binomial -results are the same (I checked)
#vector with a y/n success for each hatched egg, 1 it fledged, 0 it didn t
mod1 = brms::brm(succ_from_hatch~ breeding_exp_pair_days.z+
                   breeding_exp_Dad_days.z + breeding_exp_Mom_days.z+ 
                   Mom_MinAge.z + Dad_MinAge.z +
                   Col_Size.z+
                   (1|Season) + (1|Colony)  + (1|ColNestLaying)+
                   (breeding_exp_pair_days.z|Pair)+ 
                   (breeding_exp_Dad_days.z|BreederDad)+ 
                   (breeding_exp_Mom_days.z|BreederMom),
                 control = list(adapt_delta = 0.999),
                 iter = 8000,
                 cores = 4,
                 #prior = set_prior('normal(0, 3)'),
                 data = dat_subset,
                 family="bernoulli",
                 file="fledge_succ_nat1")

mod1<-add_criterion(mod1, "loo", cores=4) #add loo , reloo = TRUE
#good fit

plot(mod1)

pp_check(mod1)
#good fit

#measure R squared
bayes_R2(mod1)
#      Estimate Est.Error       Q2.5    Q97.5
# R2  0.7306439 0.01189742 0.7059231 0.7526819




#### main variable plot - NAT - LONG. ####

#extract Plotting values
marg<-marginal_effects(mod1, resolution=1000)

#select the variable of interest
Succ_from_hatch<-marg$breeding_exp_pair_days.z


ggplot()+
  geom_line(data = Succ_from_hatch, 
            aes(x=sd(dat_subset$breeding_exp_pair_days)*
                      Succ_from_hatch$breeding_exp_pair_days.z+
                      mean(dat_subset$breeding_exp_pair_days),
                y=estimate__, ymax=upper__ , ymin=lower__),
            size=1.3, color="#007373")+
  geom_ribbon(data = Succ_from_hatch,
              aes(x=sd(dat_subset$breeding_exp_pair_days)*
                        Succ_from_hatch$breeding_exp_pair_days.z+
                        mean(dat_subset$breeding_exp_pair_days),
                  y=estimate__, ymax=upper__, ymin=lower__),
              fill="#007373",alpha=0.3)+
  geom_point(data = dat_subset,
             aes(x=sd(dat_subset$breeding_exp_pair_days)*
                       dat_subset$breeding_exp_pair_days.z+
                       mean(dat_subset$breeding_exp_pair_days),
                 y=jitter(succ_from_hatch,0.12)),color="#007373",alpha=0.1) + 
  #xlim(0,4)+
  xlab(element_blank())+
  ylab(element_blank())+
  theme(text = element_text(size=20))+
  #theme(legend.position = c(0.9, 0.9))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))+
 scale_x_continuous(breaks=c(0,400,800,1200),
                   labels=c("0","400","800","1200"),limits = c(0,1500))

ggsave("fledge_succ_durat_nat1_v2.eps", device=cairo_ps, width = 9, height = 9,
       units ="cm", fallback_resolution = 600)




####Forest Plot - NAT - LONG. ####


plot_model(mod1,bpe = "mean",type="std2", vline.color = "#007373", line.size = .35, 
           sort.est = TRUE, transform = NULL, show.values = TRUE,
           ci.style="whisker", value.size=1.9,
           prob.inner=0,prob.outer=.95, width=0.2, 
           bpe.style = "dot", bpe.color = "black")+  
  #scale_color_sjplot("simply")+ 
  ylim(-1.9, 2.8)+ 
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








# PROTECTED


#### FINAL MODEL PROTECTED - LONG. ####
mod1 = brms::brm(succ_from_hatch~ breeding_exp_pair_days.z+
                    breeding_exp_Dad_days.z + breeding_exp_Mom_days.z+ 
                    Mom_MinAge.z + Dad_MinAge.z +
                    Col_Size.z+
                    mo(season_of_protection)+
                    (1|Season) + (1|Colony)  + (1|ColNestLaying)+
                    (breeding_exp_pair_days.z|Pair)+ 
                    (breeding_exp_Dad_days.z|BreederDad)+ 
                    (breeding_exp_Mom_days.z|BreederMom),
                  control = list(adapt_delta = 0.999),
                  iter = 8000,
                  cores = 4,
                  #prior = set_prior('normal(0, 3)'),
                  data = dat_subset,
                  family="bernoulli",
                  file="fledge_succ_prot1")



plot(mod1)

pp_check(mod1)
#pretty good fitting



#measure R squared
bayes_R2(mod1)
#    Estimate  Est.Error     Q2.5     Q97.5
# R2 0.568364 0.01604425 0.535617 0.5982389




#### main variable plot - PROT  - LONG. ####

#extract Plotting values
marg<-marginal_effects(mod1, resolution=1000)

#select the variable of interest
Succ_from_hatch<-marg$breeding_exp_pair_days.z



ggplot()+
  geom_line(data = Succ_from_hatch, 
            aes(x=sd(dat_subset$breeding_exp_pair_days)*
                      Succ_from_hatch$breeding_exp_pair_days.z+
                      mean(dat_subset$breeding_exp_pair_days),
                y=estimate__, ymax=upper__ , ymin=lower__),
            size=1.3, color="#D55E00")+
  geom_ribbon(data = Succ_from_hatch,
              aes(x=sd(dat_subset$breeding_exp_pair_days)*
                        Succ_from_hatch$breeding_exp_pair_days.z+
                        mean(dat_subset$breeding_exp_pair_days),
                  y=estimate__, ymax=upper__, ymin=lower__),
              fill="#D55E00",alpha=0.3)+
  geom_point(data = dat_subset,
             aes(x=sd(dat_subset$breeding_exp_pair_days)*
                       dat_subset$breeding_exp_pair_days.z+
                       mean(dat_subset$breeding_exp_pair_days),
                 y=jitter(succ_from_hatch,0.12)),color="#D55E00",alpha=0.1) + 
  #xlim(0,4)+
  xlab(element_blank())+
  ylab(element_blank())+
  theme(text = element_text(size=20))+
  #theme(legend.position = c(0.9, 0.9))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))


ggsave("fledge_succ_durat_prot1_v2.eps", device=cairo_ps, width = 9, height = 9,
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
  #scale_color_sjplot("simply")+ 
  ylim(-0.7, .85)+ 
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
dat3_br_attempt<-dat3[,c("succ_from_hatch", "breeding_exp_pair_days", "predation",  
                                    "sex", "Season", "breeding_exp_Mate_days", "Colony", "Col_Size",
                                    "Focal_MinAge", "Mate_MinAge", 
                                    "Pair", "ring", "Mate_ID", "season_length","ColNestLaying","unique_identifier")]
dat3_br_attempt<- subset(dat3_br_attempt, predation=="natural")

#protected colonies
dat3_br_attempt<-dat3[,c("succ_from_hatch", "breeding_exp_pair_days", "predation", 
                                    "sex", "Season", "breeding_exp_Mate_days", "Colony", "Col_Size",
                                    "Focal_MinAge", "Mate_MinAge", "season_of_protection",
                                    "Pair", "ring", "Mate_ID", "season_length","ColNestLaying","unique_identifier")]
dat3_br_attempt<- subset(dat3_br_attempt, predation=="protected")



#erase the incomplete cases
dat3_br_attempt<-dat3_br_attempt[complete.cases(dat3_br_attempt), ] #select only complete cases

#erase duplicates
dat3_br_attempt<-unique(dat3_br_attempt)


#variable tranformation
dat3_br_attempt$Colony<- as.factor(dat3_br_attempt$Colony)
dat3_br_attempt$sex<- as.factor(dat3_br_attempt$sex)


##Scaling
dat3_br_attempt$breeding_exp_Mate_days.z<- scale(dat3_br_attempt$breeding_exp_Mate_days)
dat3_br_attempt$breeding_exp_pair_days.z<- scale(dat3_br_attempt$breeding_exp_pair_days)
dat3_br_attempt$Focal_MinAge.z<- scale(dat3_br_attempt$Focal_MinAge)
dat3_br_attempt$Mate_MinAge.z<- scale(dat3_br_attempt$Mate_MinAge)
dat3_br_attempt$Col_Size.z<- scale(dat3_br_attempt$Col_Size)
dat3_br_attempt$season_length.z <- scale(dat3_br_attempt$season_length)



##NATURAL

## Natural conditions
#
# 2 models ran:
# 1- full
# 2- addition of group level slopes to see if fitting improves


#### FINAL MODEL NATURAL - CROSS-.####
mod1 = brms::brm(succ_from_hatch~ breeding_exp_pair_days.z +
                   breeding_exp_Mate_days.z+
                   Focal_MinAge.z+
                   Mate_MinAge.z+
                   Col_Size.z+
                   sex+
                   (1|Season)  +(1|Pair) + (1|ColNestLaying)+
                   (1|Colony)+ (1|ring) + (1|Mate_ID),
                 control = list(adapt_delta = 0.999, max_treedepth = 12),
                 iter = 8000,
                 cores = 4,
                 #prior = set_prior('normal(0, 3)'),
                 data = dat3_br_attempt,
                 family="bernoulli",
                 file="fledg_succ_3yrs_nat1")

plot(mod1)

pp_check(mod1)
#very good



mod1<-add_criterion(mod1, "loo", cores=4)
#Found 34 observations with a pareto_k > 0.7 in model 'mod1'


#measure R squared
bayes_R2(mod1)
#     Estimate  Est.Error      Q2.5     Q97.5
# R2 0.7529846 0.01525321 0.7192802 0.778397 




#is putting the slope for the group factors improving the model? 

mod2 = brms::brm(succ_from_hatch~ breeding_exp_pair_days.z +
                   breeding_exp_Mate_days.z+
                   Focal_MinAge.z+
                   Mate_MinAge.z+
                   Col_Size.z+
                   sex+
                   (1|Season)  +(breeding_exp_pair_days.z|Pair) + (1|ColNestLaying)+
                   (1|Colony)+ (1|ring) + (breeding_exp_Mate_days.z|Mate_ID),
                 control = list(adapt_delta = 0.999, max_treedepth = 12),
                 iter = 8000,
                 cores = 4,
                 #prior = set_prior('normal(0, 3)'),
                 data = dat3_br_attempt,
                 family="bernoulli",
                 file="fledg_succ_3yrs_nat2")

mod2<-add_criterion(mod2, "loo", cores=4) #add loo , reloo = TRUE
#Found 39 observations with a pareto_k > 0.7 in model 'mod2'


loo_compare(mod1,mod2)
#no ot does not



#### main variable plot - NAT - CROSS. ####

#extract Plotting values
marg<-marginal_effects(mod1, resolution=1000)

#select the variable of interest
Succ_from_hatch<-marg$breeding_exp_pair_days.z

ggplot()+
  geom_line(data = Succ_from_hatch, 
            aes(x=sd(dat3_br_attempt$breeding_exp_pair_days)*
                  Succ_from_hatch$breeding_exp_pair_days.z+
                  mean(dat3_br_attempt$breeding_exp_pair_days),
                y=estimate__, ymax=upper__ , ymin=lower__),
            size=1.3, color="#007373")+
  geom_ribbon(data = Succ_from_hatch, 
              aes(x=sd(dat3_br_attempt$breeding_exp_pair_days)*
                    Succ_from_hatch$breeding_exp_pair_days.z+
                    mean(dat3_br_attempt$breeding_exp_pair_days),
                  y=estimate__, ymax=upper__, ymin=lower__),
              fill="#007373",alpha=0.3)+
  geom_point(data = dat3_br_attempt,
             aes(x=sd(dat3_br_attempt$breeding_exp_pair_days)*
                   dat3_br_attempt$breeding_exp_pair_days.z+
                   mean(dat3_br_attempt$breeding_exp_pair_days),
                 y=jitter(succ_from_hatch,0.12)), color="#007373",alpha=0.15) +
  # ylim(0, 1)+
  # xlim(1,3)+
  xlab(element_blank())+
  ylab(element_blank())+
  theme(text = element_text(size=20))+
  #theme(legend.position = c(0.9, 0.9))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))#+
 # scale_x_continuous(breaks=1:3,
  #                   labels=c("1","2","3"),limits = c(1,3))



ggsave("fledge_succ_durat_nat1_v1.eps", device=cairo_ps, width = 9, height = 9,
       units ="cm", fallback_resolution = 600)





####Forest Plot - NAT - CROSS. ####


plot_model(mod1,bpe = "mean",type="std2", vline.color = "#007373", line.size = .35, 
           sort.est = TRUE, transform = NULL, show.values = TRUE,
           ci.style="whisker", value.size=1.9,
           prob.inner=0,prob.outer=.95, width=0.2, 
           bpe.style = "dot", bpe.color = "black")+  
  ylim(-4.2, 4.5)+ 
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

#### FINAL MODEL PROTECTED - CROSS. ####
mod1 = brms::brm(succ_from_hatch~ breeding_exp_pair_days.z +
                   breeding_exp_Mate_days.z+
                   Focal_MinAge.z+
                   Mate_MinAge.z+
                   Col_Size.z+
                   sex+
                   mo(season_of_protection)+
                   (1|Season)  +(1|Pair) + (1|ColNestLaying)+
                   (1|Colony)+ (1|ring) + (1|Mate_ID),
                 control = list(adapt_delta = 0.999, max_treedepth = 12),
                 iter = 8000,
                 cores = 4,
                 #prior = set_prior('normal(0, 3)'),
                 data = dat3_br_attempt,
                 family="bernoulli",
                 file="fledg_succ_3yrs_prot1")

mod1<-add_criterion(mod1, "loo", cores=4) #add loo 
#Found 28 observations with a pareto_k > 0.7 in model 'mod1'.

#measure R squared
bayes_R2(mod1)
#     Estimate Est.Error       Q2.5     Q97.5
# R2 0.6540639 0.01862646 0.6136454 0.6859955


plot(mod1)

pp_check(mod1)
#very good





#### main variable plot - PROT - CROSS. ####

#extract Plotting values
marg<-marginal_effects(mod1, resolution=1000)

#select the variable of interest
Succ_from_hatch<-marg$breeding_exp_pair_days.z

ggplot()+
  geom_line(data = Succ_from_hatch, 
            aes(x=sd(dat3_br_attempt$breeding_exp_pair_days)*
                  Succ_from_hatch$breeding_exp_pair_days.z+
                  mean(dat3_br_attempt$breeding_exp_pair_days),
                y=estimate__, ymax=upper__ , ymin=lower__),
            size=1.3, color="#D55E00")+
  geom_ribbon(data = Succ_from_hatch, 
              aes(x=sd(dat3_br_attempt$breeding_exp_pair_days)*
                    Succ_from_hatch$breeding_exp_pair_days.z+
                    mean(dat3_br_attempt$breeding_exp_pair_days),
                  y=estimate__, ymax=upper__, ymin=lower__),
              fill="#D55E00",alpha=0.3)+
  geom_point(data = dat3_br_attempt,
             aes(x=sd(dat3_br_attempt$breeding_exp_pair_days)*
                   dat3_br_attempt$breeding_exp_pair_days.z+
                   mean(dat3_br_attempt$breeding_exp_pair_days),
                 y=jitter(succ_from_hatch,0.12)), color="#D55E00",alpha=0.15) +
  # ylim(0, 1)+
  # xlim(1,3)+
  xlab(element_blank())+
  ylab(element_blank())+
  theme(text = element_text(size=20))+
  #theme(legend.position = c(0.9, 0.9))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))#+
# scale_x_continuous(breaks=1:3,
#                   labels=c("1","2","3"),limits = c(1,3))




ggsave("fledge_succ_durat_prot1_v1.eps", device=cairo_ps, width = 9, height = 9,
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
  ylim(-2.5, 5)+ 
  theme(text=element_blank())+
  theme(panel.grid = element_blank())+
  theme(panel.background = element_blank())+
  theme(panel.background = element_rect(color = NA)) + #basic theme
  theme( plot.title = element_blank())+
  theme(axis.text.y=element_blank(),axis.ticks.y = element_blank())+    #remove the label
  theme(axis.text.x=element_blank()) +
  theme(axis.line.x = element_line())
#size 130 -180


