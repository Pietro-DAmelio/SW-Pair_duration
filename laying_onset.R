#Aim: Potential reasons for increased breeding success in case of long term monogamy: do already established pair breed earlier?
#Author: P. D'Amelio



# x= pair duration indays at the time of the first breeding in the season
# y= days from to the first pair breeding for each season



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

# "julian_date_first" Puts 1 to the first pair breeding for each season

#subset for natural colonies
dat_subset<-dat[,c( "julian_date_first","breeding_exp_pair_days_season_earliest" , 
                    "Pair", "Season", "predation", "breeding_exp_Mom_days_season_earliest", "breeding_exp_Dad_days_season_earliest",
                    "Col_Size", "Min_Age_Mom_season_days_earliest", "Min_Age_Dad_season_days_earliest", "Colony",
                    "BreederMom", "BreederDad", "season_length")] 
dat_subset<-subset(dat_subset, predation=="natural")



#subset protected colonies
dat_subset<-dat[,c( "julian_date_first","breeding_exp_pair_days_season_earliest" , 
                    "Pair", "Season", "predation", "breeding_exp_Mom_days_season_earliest", "breeding_exp_Dad_days_season_earliest",
                    "Col_Size", "Min_Age_Mom_season_days_earliest", "Min_Age_Dad_season_days_earliest", "Colony",
                    "BreederMom", "BreederDad", "season_length", "season_of_protection")] 
dat_subset<-subset(dat_subset, predation=="protected")


#remove NAs, duplicates
dat_subset<- unique(dat_subset)

#select only complete cases
dat_subset<-dat_subset[complete.cases(dat_subset), ]


# variables that are factors
dat_subset$Colony<- as.factor(dat_subset$Colony)



#Scale continuos variables
dat_subset$breeding_exp_pair_days_season_earliest.z <-scale(dat_subset$breeding_exp_pair_days_season_earliest)
dat_subset$breeding_exp_Dad_days_season_earliest.z <-scale(dat_subset$breeding_exp_Dad_days_season_earliest)
dat_subset$breeding_exp_Mom_days_season_earliest.z <-scale(dat_subset$breeding_exp_Mom_days_season_earliest)
dat_subset$Min_Age_Mom_season_days_earliest.z <-scale(dat_subset$Min_Age_Mom_season_days_earliest)
dat_subset$Min_Age_Dad_season_days_earliest.z <- scale(dat_subset$Min_Age_Dad_season_days_earliest)
dat_subset$Col_Size.z<- scale(dat_subset$Col_Size)



## Natural conditions


#### FINAL MODEL NATURAL - LONG. ####
mod1 = brms::brm(julian_date_first ~ breeding_exp_pair_days_season_earliest.z+
                     breeding_exp_Dad_days_season_earliest.z+
                     breeding_exp_Mom_days_season_earliest.z+
                     Min_Age_Mom_season_days_earliest.z+
                     Min_Age_Dad_season_days_earliest.z+
                     Col_Size.z+
                     (1|Season)  +(breeding_exp_pair_days_season_earliest.z|Pair) +
                     (1|Colony)+ (breeding_exp_Dad_days_season_earliest.z|BreederDad) + 
                     (breeding_exp_Mom_days_season_earliest.z|BreederMom),
                   control = list(adapt_delta = 0.9999, max_treedepth = 14.5),
                   iter = 8000,
                   cores = 4,
                   #prior = set_prior('normal(0, 3)'),
                   data = dat_subset,
                   family = "poisson",
                   file="breeding_onset_nat1")

plot(mod1)
#chains of main population factors mixed well
pp_check(mod1)
# good fit 

bayes_R2(mod1)
#     Estimate   Est.Error     Q2.5    Q97.5
# R2 0.9807694 0.001685511 0.977282 0.983894



#plot random slopes
#pair duration slopes explain a lot of variance,  plot them

ggplot(data=dat_subset, aes(x=breeding_exp_pair_days_season_earliest.z, y=julian_date_first, col=as.factor(Pair)))+
  #scale_color_manual(values=palette)+
  viridis::scale_color_viridis(discrete = TRUE)+
  geom_point(size=1, alpha=.8)+
  geom_line()+theme_bw()+ 
  #theme(text = element_text(size=15)) +
  theme(axis.text.x = element_text(size = 15),
        axis.text.y = element_text( size = 15),  
        axis.title.x = element_text( size = 14),
        axis.title.y = element_text( size = 14))+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"),
        legend.position = "none")+
        ggtitle("Laying onset - Natural")+
        theme(plot.title = element_text(hjust = 0.5,size=18))+
        ylab(bquote('Days (since season '* ~ 1^st*' breeding)'))+
        xlab("Pair duration (years)") 
        # ylab(bquote("Days (since season's "1^st~" breeding)"))


ggsave("raw_data_natural_v1.eps", device=cairo_ps, width = 10, height = 8,
       units ="cm", fallback_resolution = 600)








#### main variable plot - NAT - LONG. ####

#extract Plotting values
marg<-marginal_effects(mod1, resolution=1000)
#select the variable of interest
breeding_onset<-marg$breeding_exp_pair_days_season_earliest.z

#plot

ggplot()+  
 geom_line(data = breeding_onset,
           aes(x=((sd(dat_subset$breeding_exp_pair_days_season_earliest)*
                     breeding_onset$breeding_exp_pair_days_season_earliest.z+
                     mean(dat_subset$breeding_exp_pair_days_season_earliest)+1)/365),
               y=estimate__, ymax=upper__ , ymin=lower__),size=1.3, color="#007373")+
 geom_ribbon(data = breeding_onset,
             aes(x=((sd(dat_subset$breeding_exp_pair_days_season_earliest)*
                       breeding_onset$breeding_exp_pair_days_season_earliest.z+
                       mean(dat_subset$breeding_exp_pair_days_season_earliest)+1)/365),
                 y=estimate__, ymax=upper__, ymin=lower__),fill="#007373",alpha=0.3)+
 geom_point(data = dat_subset,
            aes(x=((sd(dat_subset$breeding_exp_pair_days_season_earliest)*
                      dat_subset$breeding_exp_pair_days_season_earliest.z+
                      mean(dat_subset$breeding_exp_pair_days_season_earliest)+1)/365),
                y=julian_date_first), 
             color="#007373",alpha=0.1)+
  ylim(0, 310)+
  xlim(0,4)+
  xlab(element_blank())+
  ylab(element_blank())+
  theme(text = element_text(size=15)) +
  #theme(legend.position = c(0.9, 0.9))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))

#color="#007373"

ggsave("breeding_onset_exp_nat1_v3.eps", device=cairo_ps, width = 8, height = 8,
       units ="cm", fallback_resolution = 600)




####Forest Plot - NAT LONG. ####

plot_model(mod1,type="std2", vline.color = "#007373", line.size = .35, 
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



#PROTECTED


# 2 models compared:
# 1- with individual and pair group slopes
# 2- without slopes




#### FINAL MODEL PROTECTED LONG. ####

mod1 = brms::brm(julian_date_first ~ breeding_exp_pair_days_season_earliest.z+
                   breeding_exp_Dad_days_season_earliest.z+
                   breeding_exp_Mom_days_season_earliest.z+
                   Min_Age_Mom_season_days_earliest.z+
                   Min_Age_Dad_season_days_earliest.z+
                   Col_Size.z+
                   mo(season_of_protection)+
                   (1|Season)  +(breeding_exp_pair_days_season_earliest.z|Pair) +
                   (1|Colony)+ (breeding_exp_Dad_days_season_earliest.z|BreederDad) + 
                   (breeding_exp_Mom_days_season_earliest.z|BreederMom),
                 control = list(adapt_delta = 0.999, max_treedepth = 15),
                 iter = 6000,
                 cores = 4,
                 #prior = set_prior('normal(0, 3)'),
                 data = dat_subset,
                 family = "poisson",
                 file="breeding_onset_prot1") 
#mod1<-add_criterion(mod1,reloo=TRUE, "loo", cores=4) 
#296 problematic observations. Use keyfold?

plot_model(mod1, type="std2", vline.color = "black", show.values = TRUE,sort.est = TRUE,
           digits = 3, pred.type = "re", transform = NULL)+ ylim(-1, .5) +theme_bw()


plot(mod1)

pp_check(mod1)
#good

bayes_R2(mod1)
# Estimate   Est.Error      Q2.5     Q97.5
# R2 0.9756618 0.002012771 0.9714516 0.9793542







#without slopes
mod2 = brms::brm(julian_date_first ~ breeding_exp_pair_days_season_earliest.z+
                   breeding_exp_Dad_days_season_earliest.z+
                   breeding_exp_Mom_days_season_earliest.z+
                   Min_Age_Mom_season_days_earliest.z+
                   Min_Age_Dad_season_days_earliest.z+
                   Col_Size.z+
                   mo(season_of_protection)+
                   (1|Season)  +(1|Pair) +
                   (1|Colony)+ (1|BreederDad) + 
                   (1|BreederMom),
                 control = list(adapt_delta = 0.999, max_treedepth = 12),
                 iter = 8000,
                 cores = 4,
                 #prior = set_prior('normal(0, 3)'),
                 data = dat_subset,
                 family = "poisson",
                 file="breeding_onset_prot2") 
#it did not converge.





#plot raw data
ggplot(data=dat_subset, aes(x=breeding_exp_pair_days_season_earliest.z, y=julian_date_first, col=as.factor(Pair)))+
  viridis::scale_color_viridis(discrete = TRUE)+
  geom_point(size=1, alpha=.8)+
  geom_line()+theme_bw()+ 
  theme(axis.text.x = element_text(size = 15),
        axis.text.y = element_text( size = 15),  
        axis.title.x = element_text( size = 14),
        axis.title.y = element_text( size = 14))+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"),
        legend.position = "none")+
  ggtitle("Laying onset - Protected")+
  theme(plot.title = element_text(hjust = 0.5,size=18))+
  ylab(bquote("Days (since season's "* ~ 1^st*" breeding)"))+
 xlab("Pair duration (years)") 
  # ylab(bquote("Days (since season's "1^st~" breeding)"))
  
  
  ggsave("raw_data_protected_v1.eps", device=cairo_ps, width = 10, height = 8,
         units ="cm", fallback_resolution = 600)






#### main variable plot - PROT - LONG. ####

#extract Plotting values
marg<-marginal_effects(mod1, resolution=1000)
#select the variable of interest
breeding_onset<-marg$breeding_exp_pair_days_season_earliest.z

#plot

ggplot()+  
 geom_line(data = breeding_onset, aes(x=((sd(dat_subset$breeding_exp_pair_days_season_earliest)*
                                            breeding_onset$breeding_exp_pair_days_season_earliest.z+
                                            mean(dat_subset$breeding_exp_pair_days_season_earliest)+1)/365),
                                      y=estimate__, ymax=upper__ , ymin=lower__),
           size=1.3, color="#D55E00")+
 geom_ribbon(data = breeding_onset, aes(x=((sd(dat_subset$breeding_exp_pair_days_season_earliest)*
                                              breeding_onset$breeding_exp_pair_days_season_earliest.z+
                                              mean(dat_subset$breeding_exp_pair_days_season_earliest)+1)/365),
                                        y=estimate__, ymax=upper__, ymin=lower__),
             fill="#D55E00",alpha=0.3)+
 geom_point(data = dat_subset, aes(x=((sd(dat_subset$breeding_exp_pair_days_season_earliest)*
                                         dat_subset$breeding_exp_pair_days_season_earliest.z+
                                         mean(dat_subset$breeding_exp_pair_days_season_earliest)+1)/365),
                                   y=julian_date_first), 
             color="#D55E00",alpha=0.1) +
 ylim(0,310)+
 xlim(0,4)+
 xlab(element_blank())+
 ylab(element_blank())+
 theme(text = element_text(size=15)) +
 #theme(legend.position = c(0.9, 0.9))+
 theme(panel.grid.major = element_blank(), 
       panel.grid.minor = element_blank(),
       panel.background = element_blank(), 
       axis.line = element_line(colour = "black"))

ggsave("breeding_onset_exp_prot1_v3.eps", device=cairo_ps, width = 8, height = 8,
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



#natural
dat3_season<-dat3[,c("julian_date_first", "breeding_exp_pair_days_season_earliest", "predation",
                            "sex", "Season", "breeding_exp_Mate_days_season_earliest", "Colony", "Col_Size",
                            "Min_Age_Mate_season_days_earliest", "Min_Age_Focal_season_days_earliest", 
                            "Pair", "ring", "Mate_ID",  "season_length")]
dat3_season<-subset(dat3_season, predation=="natural")



#protected

dat3_season<-dat3[,c("julian_date_first", "breeding_exp_pair_days_season_earliest", "predation",
                            "sex", "Season", "breeding_exp_Mate_days_season_earliest", "Colony", "Col_Size",
                            "Min_Age_Mate_season_days_earliest", "Min_Age_Focal_season_days_earliest", "season_of_protection",
                            "Pair", "ring", "Mate_ID",  "season_length")]
dat3_season<-subset(dat3_season, predation=="protected")


#remove NAs, duplicates
dat3_season<-unique(dat3_season)
#select only complete cases
dat3_season<-dat3_season[complete.cases(dat3_season), ] 



#variable tranformation
dat3_season$Colony<- as.factor(dat3_season$Colony)
dat3_season$sex<- as.factor(dat3_season$sex)


#scaling
dat3_season$breeding_exp_Mate_days_season_earliest.z<- scale(dat3_season$breeding_exp_Mate_days_season_earliest)
dat3_season$breeding_exp_pair_days_season_earliest.z<- scale(dat3_season$breeding_exp_pair_days_season_earliest)
dat3_season$Min_Age_Mate_season_days_earliest.z<- scale(dat3_season$Min_Age_Mate_season_days_earliest)
dat3_season$Min_Age_Focal_season_days_earliest.z<- scale(dat3_season$Min_Age_Focal_season_days_earliest)
dat3_season$Col_Size.z<- scale(dat3_season$Col_Size)




#NATURAL - 3yrs of exp


## Natural conditions
# full model did not converge, model simplified:
# 2 models ran:
# 1- full
# 2- group factor Mate ID removed



mod1 = brms::brm(julian_date_first ~ breeding_exp_pair_days_season_earliest.z+
                   breeding_exp_Mate_days_season_earliest.z+
                   Min_Age_Mate_season_days_earliest.z+
                   Min_Age_Focal_season_days_earliest.z+
                   Col_Size.z+
                   sex+
                   (1|Season)  +(1|Pair) +
                   (1|Colony)+ (1|Mate_ID),
                 control = list(adapt_delta = 0.9999, max_treedepth = 11.5),
                 iter = 8000,
                 cores = 4,
                 #prior = set_prior('normal(0, 3)'),
                 data = dat3_season,
                 family = "poisson",
                 file="breeding_onset_3yrs_nat1a")
#There were xx divergent transitions after warmup.




#remove mate ID
#### FINAL MODEL NATURAL CROSS####
mod3 = brms::brm(julian_date_first ~ breeding_exp_pair_days_season_earliest.z+
                   breeding_exp_Mate_days_season_earliest.z+
                   Min_Age_Mate_season_days_earliest.z+
                   Min_Age_Focal_season_days_earliest.z+
                   Col_Size.z+
                   sex+
                   (1|Season)  +(1|Pair) +
                   (1|Colony),#+ (1|Mate_ID),
                 control = list(adapt_delta = 0.9999, max_treedepth = 11.5),
                 iter = 8000,
                 cores = 4,
                 #prior = set_prior('normal(0, 3)'),
                 data = dat3_season,
                 family = "poisson",
                 file="breeding_onset_3yrs_nat3")


plot(mod1)

pp_check(mod1)
#pretty good fitting!

bayes_R2(mod3)
#   Estimate   Est.Error      Q2.5     Q97.5
#R2 0.9805663 0.003484591 0.9729184 0.9864637




#### main variable plot - NAT. - CROSS. ####

#extract Plotting values
marg<-marginal_effects(mod3, resolution=1000)
#select the variable of interest
breeding_onset<-marg$breeding_exp_pair_days_season_earliest.z

#plot
ggplot()+  
  geom_line(data = breeding_onset,
            aes(x=((sd(dat3_season$breeding_exp_pair_days_season_earliest)*
                      breeding_onset$breeding_exp_pair_days_season_earliest.z+
                      mean(dat3_season$breeding_exp_pair_days_season_earliest)+1)/365),
                y=estimate__, ymax=upper__ , ymin=lower__),size=1.3, color="#007373")+
  geom_ribbon(data = breeding_onset,
              aes(x=((sd(dat3_season$breeding_exp_pair_days_season_earliest)*
                        breeding_onset$breeding_exp_pair_days_season_earliest.z+
                        mean(dat3_season$breeding_exp_pair_days_season_earliest)+1)/365),
                  y=estimate__, ymax=upper__, ymin=lower__),fill="#007373",alpha=0.3)+
  geom_point(data = dat3_season,
             aes(x=((sd(dat3_season$breeding_exp_pair_days_season_earliest)*
                       dat3_season$breeding_exp_pair_days_season_earliest.z+
                       mean(dat3_season$breeding_exp_pair_days_season_earliest)+1)/365),
                 y=julian_date_first), 
             color="#007373",alpha=0.1)+
  ylim(0, 310)+
  xlim(0,2.4)+
  xlab(element_blank())+
  ylab(element_blank())+
  theme(text = element_text(size=15)) +
  #theme(legend.position = c(0.9, 0.9))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))



ggsave("breeding_onset_exp_nat3_3y_v1.eps", device=cairo_ps, width = 8, height = 8,
       units ="cm", fallback_resolution = 600)



####Forest Plot - NAT. - CROSS. ####

plot_model(mod3,type="std2", vline.color = "#007373", line.size = .35, 
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




#PROTECTED


#### FINAL MODEL PROTECTED - CROSS ####
mod1 = brms::brm(julian_date_first ~ breeding_exp_pair_days_season_earliest.z +
                   breeding_exp_Mate_days_season_earliest.z+
                   Min_Age_Mate_season_days_earliest.z+
                   Min_Age_Focal_season_days_earliest.z+
                   Col_Size.z+
                   sex+
                   mo(season_of_protection)+
                   (1|Season)  +(1|Colony)  +
                   (1|Pair) + (1|Mate_ID),
                 control = list(adapt_delta = 0.9999, max_treedepth = 17),
                 iter = 8000,
                 cores = 4,
                 #prior = set_prior('normal(0, 3)'),
                 data = dat3_season,
                 family = "poisson",
                 file="breeding_onset_3yrs_prot1")

plot(mod1)

pp_check(mod1)
#good fit


bayes_R2(mod1)
#     Estimate   Est.Error      Q2.5     Q97.5
# R2 0.9801413 0.004595974 0.9700251 0.9878485



#### main variable plot - PROT - CROSS. ####

#extract Plotting values
marg<-marginal_effects(mod1, resolution=1000)
#select the variable of interest
breeding_onset<-marg$breeding_exp_pair_days_season_earliest.z

 #plot

ggplot()+  
  geom_line(data = breeding_onset,
            aes(x=((sd(dat3_season$breeding_exp_pair_days_season_earliest)*
                      breeding_onset$breeding_exp_pair_days_season_earliest.z+
                      mean(dat3_season$breeding_exp_pair_days_season_earliest)+1)/365),
                y=estimate__, ymax=upper__ , ymin=lower__),size=1.3, color="#D55E00")+
  geom_ribbon(data = breeding_onset,
              aes(x=((sd(dat3_season$breeding_exp_pair_days_season_earliest)*
                        breeding_onset$breeding_exp_pair_days_season_earliest.z+
                        mean(dat3_season$breeding_exp_pair_days_season_earliest)+1)/365),
                  y=estimate__, ymax=upper__, ymin=lower__),fill="#D55E00",alpha=0.3)+
  geom_point(data = dat3_season,
             aes(x=((sd(dat3_season$breeding_exp_pair_days_season_earliest)*
                       dat3_season$breeding_exp_pair_days_season_earliest.z+
                       mean(dat3_season$breeding_exp_pair_days_season_earliest)+1)/365),
                 y=julian_date_first), 
             color="#D55E00",alpha=0.1)+
  ylim(0, 310)+
  xlim(0,2.4)+
  xlab(element_blank())+
  ylab(element_blank())+
  theme(text = element_text(size=15)) +
  #theme(legend.position = c(0.9, 0.9))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))



ggsave("breeding_onset_exp_prot1_3y_v1.eps", device=cairo_ps, width = 8, height = 8,
       units ="cm", fallback_resolution = 600)



####Forest Plot - PROT.  - CROSS. ####

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


