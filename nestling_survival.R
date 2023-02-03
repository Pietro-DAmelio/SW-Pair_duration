#Aim: Potential reasons for increased breeding success in case of long term monogamy: 
#is nestling survival higher?
#Author: P. D'Amelio


#Nestling survival (Fledging success) is intended from hatching, not from the egg stage.

#### libraries  ####
library(brms)
library(loo)
library(sjPlot)
library(ggplot2)
library(ggpubr)




### data handling ####

#load database
dat<- read.csv("YOUR_DIRECTORY/Longitudinal_dataset.csv")



#for colonies in the natural state, assign 0 at the length of protection

dat$season_of_protection[is.na(dat$season_of_protection)] <- 0



#select the variables of interest
dat_subset<-dat[,c( "succ_from_hatch","breeding_exp_pair_days",
                    "Pair", "Season", "predation", "breeding_exp_Mom_days", "breeding_exp_Dad_days",
                    "Col_Size", "Mom_MinAge", "Dad_MinAge", "Colony","Nest",
                    "BreederMom", "BreederDad", "season_of_protection","ColNestLaying", "unique_ID")]




dat_subset<-dat_subset[complete.cases(dat_subset), ] #select only complete cases


# variables that are factors
dat_subset$Colony<- as.factor(dat_subset$Colony)
#control for possible nest effect (nest naming changes between years)
dat_subset$nest_season<-paste(dat_subset$Nest, dat_subset$Season, sep="")



#Scale continuous variables
dat_subset$breeding_exp_pair_days.z <-scale(dat_subset$breeding_exp_pair_days)
dat_subset$breeding_exp_Dad_days.z <-scale(dat_subset$breeding_exp_Dad_days)
dat_subset$breeding_exp_Mom_days.z <-scale(dat_subset$breeding_exp_Mom_days)
dat_subset$Mom_MinAge.z <-scale(dat_subset$Mom_MinAge)
dat_subset$Dad_MinAge.z <- scale(dat_subset$Dad_MinAge)

dat_subset$Col_Size.z<- scale(dat_subset$Col_Size)

hist(dat_subset$succ_from_hatch)





###MODEL ####


#I will run bernoulli models (binomials with only 0 and 1), each row an hatched egg, 1 if it fledged, 0 if it failed.

mod1 = brms::brm(succ_from_hatch~ breeding_exp_pair_days.z*predation+
                   breeding_exp_Mom_days.z*predation+ 
                   breeding_exp_Dad_days.z*predation + 
                   Mom_MinAge.z*predation +
                   Dad_MinAge.z*predation +
                   Col_Size.z+
                   mo(season_of_protection)+
                   (1|Season) + (1|Colony)  +  (1|nest_season) + (1|ColNestLaying)+
                   (breeding_exp_pair_days.z|Pair)+ 
                   (breeding_exp_Dad_days.z|BreederDad)+ 
                   (breeding_exp_Mom_days.z|BreederMom),
                 control = list(adapt_delta = 0.999),
                 iter = 8000,
                 cores = 4,
                 #prior = set_prior('normal(0, 3)'),
                 data = dat_subset,
                 family="bernoulli",
                 file="fledge_succ_long")






#### PLOT ####


#extract Plotting values
marg<-conditional_effects(mod1,"breeding_exp_pair_days.z:predation", resolution=1000)

#I choose to extract the estimates without random factors

#select the variable of interest
pair_duration<-marg$breeding_exp_pair_days.z


#main plot
int<-ggplot()+
  geom_line(aes(x=sd(dat_subset$breeding_exp_pair_days)*
                  pair_duration$breeding_exp_pair_days.z[pair_duration$predation=="natural"]+
                  mean(dat_subset$breeding_exp_pair_days),
                y=pair_duration$estimate__[pair_duration$predation=="natural"],color="natural"),
            size=1.3)+
  
  geom_line(aes(x=sd(dat_subset$breeding_exp_pair_days)*
                  pair_duration$breeding_exp_pair_days.z[pair_duration$predation=="protected"]+
                  mean(dat_subset$breeding_exp_pair_days),
                y=pair_duration$estimate__[pair_duration$predation=="protected"], color="protected"),
            size=1.3)+
  
  geom_ribbon(aes(x=sd(dat_subset$breeding_exp_pair_days)*pair_duration$breeding_exp_pair_days.z[pair_duration$predation=="natural"]+
                    mean(dat_subset$breeding_exp_pair_days),
                  y=pair_duration$estimate__[pair_duration$predation=="natural"], 
                  ymax=pair_duration$upper__[pair_duration$predation=="natural"],
                  ymin=pair_duration$lower__[pair_duration$predation=="natural"]),
              fill="#007373",alpha=0.3)+
  
  geom_ribbon(aes(x=sd(dat_subset$breeding_exp_pair_days)*pair_duration$breeding_exp_pair_days.z[pair_duration$predation=="protected"]+
                    mean(dat_subset$breeding_exp_pair_days),
                  y=pair_duration$estimate__[pair_duration$predation=="protected"], 
                  ymax=pair_duration$upper__[pair_duration$predation=="protected"],
                  ymin=pair_duration$lower__[pair_duration$predation=="protected"]),
              fill="#D55E00",alpha=0.3)+
  
  geom_point(aes(x=sd(dat_subset$breeding_exp_pair_days)*dat_subset$breeding_exp_pair_days.z[dat_subset$predation=="natural"]+
                   mean(dat_subset$breeding_exp_pair_days),y=dat_subset$succ_from_hatch[dat_subset$predation=="natural"],
                 color="natural"),alpha=0.2) +
  
  geom_point(aes(x=(sd(dat_subset$breeding_exp_pair_days)*dat_subset$breeding_exp_pair_days.z[dat_subset$predation=="protected"]+
                      mean(dat_subset$breeding_exp_pair_days)),y=dat_subset$succ_from_hatch[dat_subset$predation=="protected"]+0.02,
                 color="protected"),alpha=0.2) +
  
  labs(x="",
       y="Fledging probability \n",
       title = "")+
  scale_color_manual(name="Predation", values= c("#007373","#D55E00"), 
                     labels=c("natural", "protected"), guide=guide_legend(override.aes = list(fill=c("#D55E00","#007373"))))+
  #ylim(0, 18)+
  # xlim(1,5)+
  #xlab(element_blank())+
  #scale_x_discrete(limits = seq(1:7))+
  #ylab(element_blank())+
  theme(text = element_text(size=20)) +
  theme(legend.title = element_text(size = 13, face="bold"),
        legend.text = element_text(size = 13))+
  theme(legend.position = "top")+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))



#forest plot
forest<-plot_model(mod1,type="std2", vline.color = "black", line.size = .5, 
                   sort.est = FALSE, transform = NULL, show.values = TRUE,
                   ci.style="whisker", value.size=3.5,value.offset=.25, dot.size = 1,
                   prob.inner=0,prob.outer=.95, width=0.2,
                   bpe.style = "dot",  bpe.color = "black",
                   rm.terms = c("simo_moseason_of_protection1[1]",
                                "simo_moseason_of_protection1[2]",
                                "simo_moseason_of_protection1[3]",
                                "simo_moseason_of_protection1[4]",
                                "simo_moseason_of_protection1[5]",
                                "simo_moseason_of_protection1[6]",
                                "simo_moseason_of_protection1[7]"))+ 
  scale_x_discrete(labels=c("Seasons of protection","Male age:Predation(protected)","Female age:Predation(protected)",
                            "Male experience:Predation(protected)","Female experience:Predation(protected)",
                            "Pair duration:Predation(protected)",
                            "Colony size", "Male age","Female age","Male experience","Female experience",
                            "Predation(protected)","Pair duration"))+
  #
  #scale_color_sjplot("simply")+ 
  ylim(-1.2, 4.5)+ 
  labs(y=" ")+
  # theme(text=element_blank())+
  theme(panel.grid = element_blank())+
  # theme(panel.background = element_blank())+
  theme(panel.background = element_rect(color = "white", fill="white")) + #basic theme
  theme(plot.title = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line.x = element_line(),
        axis.text.x = element_text(color="black",size=14),
        axis.text.y = element_text(color="black",size=14),
        axis.title = element_text(color="black",size=22))



#merge the 2 plots
ggarrange(int, forest, nrow=1, ncol=2, align = "h",widths=c(1,1), labels="AUTO")
#export pdf 7x14 landscape



#longitudinal and cross-sectional in one figure (run after running the cross-sectional part of the script)
ggarrange(int, forest, int2, forest2, nrow=2, ncol=2, align = "h",widths=c(1,1), labels="AUTO")






####TABLE #####

##save table summary

tab_model(mod1,file="fledging_success_review2.doc", transform=NULL, show.icc = FALSE, show.zeroinf = FALSE, show.re.var=TRUE)


#the random coefficient are not extracted, I have to extract them manually

summ. <-summary(mod1)

random_Pair <- data.frame(summ.$random$Pair)
random_Pair <- cbind(rownames(random_Pair), data.frame(random_Pair, row.names=NULL))
colnames(random_Pair)[colnames(random_Pair)=="rownames(random_Pair)"] <- "Random effects"
random_Pair<-random_Pair[,c(1,2,4,5)]

random_Breeder_Dad <- data.frame(summ.$random$BreederDad)
random_Breeder_Dad <- cbind(rownames(random_Breeder_Dad), data.frame(random_Breeder_Dad, row.names=NULL))
colnames(random_Breeder_Dad)[colnames(random_Breeder_Dad)=="rownames(random_Breeder_Dad)"] <- "Random effects"
random_Breeder_Dad <- random_Breeder_Dad[,c(1,2,4,5)]


random_BreederMom <- data.frame(summ.$random$BreederMom)
random_BreederMom <- cbind(rownames(random_BreederMom), data.frame(random_BreederMom, row.names=NULL))
colnames(random_BreederMom)[colnames(random_BreederMom)=="rownames(random_BreederMom)"] <- "Random effects"
random_BreederMom <- random_BreederMom[,c(1,2,4,5)]

random_Colony <- data.frame(summ.$random$Colony)
random_Colony <- cbind(rownames(random_Colony), data.frame(random_Colony, row.names=NULL))
colnames(random_Colony)[colnames(random_Colony)=="rownames(random_Colony)"] <- "Random effects"
random_Colony<-random_Colony[,c(1,2,4,5)]

random_Season <- data.frame(summ.$random$Season)
random_Season <- cbind(rownames(random_Season), data.frame(random_Season, row.names=NULL))
colnames(random_Season)[colnames(random_Season)=="rownames(random_Season)"] <-"Random effects"
random_Season<-random_Season[,c(1,2,4,5)]

random_Nest <- data.frame(summ.$random$nest_season)
random_Nest <- cbind(rownames(random_Nest), data.frame(random_Nest, row.names=NULL))
colnames(random_Nest)[colnames(random_Nest)=="rownames(random_Nest)"] <-"Random effects"
random_Nest<-random_Nest[,c(1,2,4,5)]

random_Brood <- data.frame(summ.$random$ColNestLaying)
random_Brood <- cbind(rownames(random_Brood), data.frame(random_Brood, row.names=NULL))
colnames(random_Brood)[colnames(random_Brood)=="rownames(random_Brood)"] <-"Random effects"
random_Brood<-random_Brood[,c(1,2,4,5)]




random_<-rbind(random_Pair,random_Breeder_Dad,random_BreederMom, random_Colony,random_Season,
               random_Nest, random_Brood)

random_f<-cbind(random_[,1],round(random_[,2:4],2))
random_f$CrI<-paste0(paste(random_f$l.95..CI, random_f$u.95..CI, sep = " - "))

random_f$l.95..CI <- NULL
random_f$u.95..CI <- NULL

colnames(random_f)[colnames(random_f)=="random_[, 1]"] <- "Variables"

random_f$Variables<- c("Pair ID (663)", "slope Pair duration | Pair ID","corr. Pair duration - Pair ID",
                       "Male ID (458)", "slope Male experience | Male ID", "corr. Male experience - Male ID",
                       "Female ID (499)", "slope Female experience | Female ID","corr. Female experience - Female ID",
                       "Colony ID (21)", "Season (10)", "Nest (575)", "Brood (1710)")


# N Season	10
# N Colony	21
# N nest_season	575
# N ColNestLaying	1710
# N Pair	663
# N BreederDad	458
# N BreederMom	499


write.csv(random_f, "random_long_review2.csv", row.names = F)





####Extract fixed factor estimates for paper####
s<-data.frame(fixef(mod1,summary = F))

#The reference group is natural so you have to add the interaction with protected if you want to extract their credible interval
mean(s$breeding_exp_pair_days.z+s$breeding_exp_pair_days.z.predationprotected)
#0.2038483
quantile(s$breeding_exp_pair_days.z+s$breeding_exp_pair_days.z.predationprotected,probs = c(0.025, 0.975))
#      2.5%      97.5% 
#-0.2927977  0.7126331


mean(s$breeding_exp_pair_days.z)
#0.07384381
quantile(s$breeding_exp_pair_days.z,probs = c(0.025, 0.975))
#  2.5%      97.5% 
#-0.3714934  0.5228067 










#####cross-sectional#########
#fixed breeding experience


#load dataset
dat_fixed_focal_breeding_exp<- read.csv("YOUR_DIRECTORY/Cross-sectional_dataset.csv")


#for colonies in the natural state, assign 0 at the length of protection
dat_fixed_focal_breeding_exp$season_of_protection[is.na(dat_fixed_focal_breeding_exp$season_of_protection)] <- 0


#select the variables of interest
dat_fixed_focal_breeding_exp<-dat_fixed_focal_breeding_exp[,c("succ_from_hatch", "breeding_exp_pair_days", "predation", 
                                    "sex", "Season", "breeding_exp_Mate_days", "Colony", "Col_Size",
                                    "Focal_MinAge", "Mate_MinAge", "season_of_protection",
                                    "Pair", "ring", "Mate_ID","ColNestLaying","unique_ID")]


#erase the incomplete cases
dat_fixed_focal_breeding_exp<-dat_fixed_focal_breeding_exp[complete.cases(dat_fixed_focal_breeding_exp), ] #select only complete cases



#variable transformation
dat_fixed_focal_breeding_exp$Colony<- as.factor(dat_fixed_focal_breeding_exp$Colony)
dat_fixed_focal_breeding_exp$sex<- as.factor(dat_fixed_focal_breeding_exp$sex)


##Scaling
dat_fixed_focal_breeding_exp$breeding_exp_Mate_days.z<- scale(dat_fixed_focal_breeding_exp$breeding_exp_Mate_days)
dat_fixed_focal_breeding_exp$breeding_exp_pair_days.z<- scale(dat_fixed_focal_breeding_exp$breeding_exp_pair_days)
dat_fixed_focal_breeding_exp$Focal_MinAge.z<- scale(dat_fixed_focal_breeding_exp$Focal_MinAge)
dat_fixed_focal_breeding_exp$Mate_MinAge.z<- scale(dat_fixed_focal_breeding_exp$Mate_MinAge)
dat_fixed_focal_breeding_exp$Col_Size.z<- scale(dat_fixed_focal_breeding_exp$Col_Size)


###MODEL ####

mod1 = brms::brm(succ_from_hatch ~ breeding_exp_pair_days.z*predation +
                   breeding_exp_Mate_days.z*predation+
                   Focal_MinAge.z*predation+
                   Mate_MinAge.z*predation+
                   Col_Size.z+
                   sex+
                   (1|Season)  +(1|Pair) + (1|ColNestLaying)+
                   (1|Colony)+ (1|ring) + (1|Mate_ID),
                 control = list(adapt_delta = 0.999, max_treedepth = 12),
                 iter = 10000,
                 cores = 4,
                 #prior = set_prior('normal(0, 3)'),
                 data = dat_fixed_focal_breeding_exp,
                 family="bernoulli",
                 file="fledg_succ_crossSec_rev2")


#check model fitting
pp_check(mod1)
plot(mod1)

conditional_effects(mod1)





#### PLOT ####


#extract Plotting values
marg<-conditional_effects(mod1,"breeding_exp_pair_days.z:predation", resolution=1000)


#select the variable of interest
pair_duration1<-marg$breeding_exp_pair_days.z

#main plot
int2<-ggplot()+
  geom_line(aes(x=sd(dat_fixed_focal_breeding_exp$breeding_exp_pair_days)*
                  pair_duration1$breeding_exp_pair_days.z[pair_duration1$predation=="natural"]+
                  mean(dat_fixed_focal_breeding_exp$breeding_exp_pair_days),
                y=pair_duration1$estimate__[pair_duration1$predation=="natural"],color="natural"),
            size=1.3)+
  
  geom_line(aes(x=sd(dat_fixed_focal_breeding_exp$breeding_exp_pair_days)*
                  pair_duration1$breeding_exp_pair_days.z[pair_duration1$predation=="protected"]+
                  mean(dat_fixed_focal_breeding_exp$breeding_exp_pair_days),
                y=pair_duration1$estimate__[pair_duration1$predation=="protected"], color="protected"),
            size=1.3)+
  
  geom_ribbon(aes(x=sd(dat_fixed_focal_breeding_exp$breeding_exp_pair_days)*pair_duration1$breeding_exp_pair_days.z[pair_duration1$predation=="natural"]+
                    mean(dat_fixed_focal_breeding_exp$breeding_exp_pair_days),
                  y=pair_duration1$estimate__[pair_duration1$predation=="natural"], 
                  ymax=pair_duration1$upper__[pair_duration1$predation=="natural"],
                  ymin=pair_duration1$lower__[pair_duration1$predation=="natural"]),
              fill="#007373",alpha=0.3)+
  
  geom_ribbon(aes(x=sd(dat_fixed_focal_breeding_exp$breeding_exp_pair_days)*pair_duration1$breeding_exp_pair_days.z[pair_duration1$predation=="protected"]+
                    mean(dat_fixed_focal_breeding_exp$breeding_exp_pair_days),
                  y=pair_duration1$estimate__[pair_duration1$predation=="protected"], 
                  ymax=pair_duration1$upper__[pair_duration1$predation=="protected"],
                  ymin=pair_duration1$lower__[pair_duration1$predation=="protected"]),
              fill="#D55E00",alpha=0.3)+
  
  geom_point(aes(x=sd(dat_fixed_focal_breeding_exp$breeding_exp_pair_days)*dat_fixed_focal_breeding_exp$breeding_exp_pair_days.z[dat_fixed_focal_breeding_exp$predation=="natural"]+
                   mean(dat_fixed_focal_breeding_exp$breeding_exp_pair_days),y=dat_fixed_focal_breeding_exp$succ_from_hatch[dat_fixed_focal_breeding_exp$predation=="natural"],
                 color="natural"),alpha=0.2) +
  
  geom_point(aes(x=(sd(dat_fixed_focal_breeding_exp$breeding_exp_pair_days)*dat_fixed_focal_breeding_exp$breeding_exp_pair_days.z[dat_fixed_focal_breeding_exp$predation=="protected"]+
                      mean(dat_fixed_focal_breeding_exp$breeding_exp_pair_days)),y=dat_fixed_focal_breeding_exp$succ_from_hatch[dat_fixed_focal_breeding_exp$predation=="protected"]+0.02,
                 color="protected"),alpha=0.2) +
  
  labs(x="\nPair duration (days)",
       y="Fledging probability \n",
       title = "")+
  scale_color_manual(name="Predation", values= c("#007373","#D55E00"), 
                     labels=c("natural", "protected"), guide=guide_legend(override.aes = list(fill=c("#D55E00","#007373"))))+
  #ylim(0, 18)+
  # xlim(1,5)+
  #xlab(element_blank())+
  #scale_x_discrete(limits = seq(1:7))+
  #ylab(element_blank())+
  theme(text = element_text(size=20)) +
  theme(legend.title = element_text(size = 13, face="bold"),
        legend.text = element_text(size = 13))+
  theme(legend.position = "top")+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))


#forest plot
forest2<-plot_model(mod1,type="std2", vline.color = "black", line.size = .5, 
                   sort.est = FALSE, transform = NULL, show.values = TRUE,
                   ci.style="whisker", value.size=3.5,value.offset=.25, dot.size = 1,
                   prob.inner=0,prob.outer=.95, width=0.2,
                   bpe.style = "dot",  bpe.color = "black",
                   rm.terms = c("simo_moseason_of_protection1[1]",
                                "simo_moseason_of_protection1[2]",
                                "simo_moseason_of_protection1[3]",
                                "simo_moseason_of_protection1[4]",
                                "simo_moseason_of_protection1[5]",
                                "simo_moseason_of_protection1[6]",
                                "simo_moseason_of_protection1[7]"))+ 
  scale_x_discrete(labels=c("Mate age:Predation(protected)","Focal age:Predation(protected)",
                            "Mate experience:Predation(protected)","Pair duration:Predation(protected)",
                            "Sex(female)", "Colony size","Mate age","Focal age","Mate experience","Predation(protected)","Pair duration"))+
  #
  #scale_color_sjplot("simply")+ 
  ylim(-4, 4.5)+ 
  labs(y="\nEffect size")+
  # theme(text=element_blank())+
  theme(panel.grid = element_blank())+
  # theme(panel.background = element_blank())+
  theme(panel.background = element_rect(color = "white", fill="white")) + #basic theme
  theme(plot.title = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line.x = element_line(),
        axis.text.x = element_text(color="black",size=14),
        axis.text.y = element_text(color="black",size=14),
        axis.title = element_text(color="black",size=22))




####TABLE CROSS-SECTIONAL #####


##save table summary

tab_model(mod1,file="fledging_success_cross_review2.doc", transform=NULL, show.icc = FALSE, show.zeroinf = FALSE, show.re.var=TRUE)


#the random coefficient are not extracted, I have to extract them manually

summ. <-summary(mod1)

random_Pair <- data.frame(summ.$random$Pair)
random_Pair <- cbind(rownames(random_Pair), data.frame(random_Pair, row.names=NULL))
colnames(random_Pair)[colnames(random_Pair)=="rownames(random_Pair)"] <- "Random effects"
random_Pair<-random_Pair[,c(1,2,4,5)]

random_FocalID <- data.frame(summ.$random$ring)
random_FocalID <- cbind(rownames(random_FocalID), data.frame(random_FocalID, row.names=NULL))
colnames(random_FocalID)[colnames(random_FocalID)=="rownames(random_FocalID)"] <- "Random effects"
random_FocalID <- random_FocalID[,c(1,2,4,5)]


random_MateID <- data.frame(summ.$random$Mate_ID)
random_MateID <- cbind(rownames(random_MateID), data.frame(random_MateID, row.names=NULL))
colnames(random_MateID)[colnames(random_MateID)=="rownames(random_MateID)"] <- "Random effects"
random_MateID <- random_MateID[,c(1,2,4,5)]

random_Colony <- data.frame(summ.$random$Colony)
random_Colony <- cbind(rownames(random_Colony), data.frame(random_Colony, row.names=NULL))
colnames(random_Colony)[colnames(random_Colony)=="rownames(random_Colony)"] <- "Random effects"
random_Colony<-random_Colony[,c(1,2,4,5)]

random_Season <- data.frame(summ.$random$Season)
random_Season <- cbind(rownames(random_Season), data.frame(random_Season, row.names=NULL))
colnames(random_Season)[colnames(random_Season)=="rownames(random_Season)"] <-"Random effects"
random_Season<-random_Season[,c(1,2,4,5)]


random_Brood <- data.frame(summ.$random$ColNestLaying)
random_Brood <- cbind(rownames(random_Brood), data.frame(random_Brood, row.names=NULL))
colnames(random_Brood)[colnames(random_Brood)=="rownames(random_Brood)"] <-"Random effects"
random_Brood<-random_Brood[,c(1,2,4,5)]




random_<-rbind(random_Pair,random_FocalID,random_MateID, random_Colony,random_Season,
               random_Brood)

random_f<-cbind(random_[,1],round(random_[,2:4],2))
random_f$CrI<-paste0(paste(random_f$l.95..CI, random_f$u.95..CI, sep = " - "))

random_f$l.95..CI <- NULL
random_f$u.95..CI <- NULL

colnames(random_f)[colnames(random_f)=="random_[, 1]"] <- "Variables"

random_f$Variables<- c("Pair ID (128)", 
                       "Focal ID (171)", 
                       "Mate ID (165)",  
                       "Colony ID (13)", "Season (8)", "Brood (134)")


# N Season	8
# N Pair	128
# N ColNestLaying	134
# N Colony	13
# N ring	171
# N Mate_ID	165



write.csv(random_f, "random_cross_review2.csv", row.names = F)















### erase pairs with more than 4 seasons together ####
#we erase the extreme values of pair duration because very rare,
#to check that the results are not dependent on them

#4 years =1460 days

dat_subset2<-subset(dat_subset, breeding_exp_pair_days<=1460)





#Scale continuous variables


dat_subset2$breeding_exp_pair_days.z <-scale(dat_subset2$breeding_exp_pair_days)
dat_subset2$breeding_exp_Dad_days.z <-scale(dat_subset2$breeding_exp_Dad_days)
dat_subset2$breeding_exp_Mom_days.z <-scale(dat_subset2$breeding_exp_Mom_days)
dat_subset2$Mom_MinAge.z <-scale(dat_subset2$Mom_MinAge)
dat_subset2$Dad_MinAge.z <- scale(dat_subset2$Dad_MinAge)

dat_subset2$Col_Size.z<- scale(dat_subset2$Col_Size)





mod2 = brms::brm(succ_from_hatch~ breeding_exp_pair_days.z*predation+
                   breeding_exp_Mom_days.z*predation+ 
                   breeding_exp_Dad_days.z*predation + 
                   Mom_MinAge.z*predation +
                   Dad_MinAge.z*predation +
                   Col_Size.z+
                   mo(season_of_protection)+
                   (1|Season) + (1|Colony)  +  (1|nest_season) + (1|ColNestLaying)+
                   (breeding_exp_pair_days.z|Pair)+ 
                   (breeding_exp_Dad_days.z|BreederDad)+ 
                   (breeding_exp_Mom_days.z|BreederMom),
                 control = list(adapt_delta = 0.999),
                 iter = 8000,
                 cores = 4,
                 #prior = set_prior('normal(0, 3)'),
                 data = dat_subset2,
                 family="bernoulli",
                 file="fledge_succ_long_trim_rev2")



s<-data.frame(fixef(mod2,summary = F))

#The reference group is natural so you have to add the interaction with protected if you want to extract their credible interval
mean(s$breeding_exp_pair_days.z+s$breeding_exp_pair_days.z.predationprotected)
#0.1735864
quantile(s$breeding_exp_pair_days.z+s$breeding_exp_pair_days.z.predationprotected,probs = c(0.025, 0.975))
#      2.5%      97.5% 
#-0.3195443  0.6673124 


mean(s$breeding_exp_pair_days.z)
#0.07158068
quantile(s$breeding_exp_pair_days.z,probs = c(0.025, 0.975))
#  2.5%      97.5% 
#-0.3600796  0.5076752  



#results are nearly identical to the full dataset


