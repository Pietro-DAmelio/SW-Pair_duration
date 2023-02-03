#Aim: Describe influence of pair duration on the mass of the fledglings 
#Author: P. D'Amelio




### libraries  ####
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
dat_subset<-dat[,c( "fledging_mass","breeding_exp_pair_days" ,
                    "Pair", "Season", "predation", "breeding_exp_Mom_days", "breeding_exp_Dad_days",
                    "Col_Size", "Mom_MinAge", "Dad_MinAge", "Colony", "tarsus_mom", "tarsus_dad",
                    "N_hatched","Nest", "wing_chick", "ringer", "season_of_protection",
                    "BreederMom", "BreederDad","ColNestLaying","unique_ID")] 





dat_subset<-dat_subset[complete.cases(dat_subset), ] #select only complete cases


# variables that are factors
dat_subset$Colony<- as.factor(dat_subset$Colony)

#control for possible nest effect (nest naming changes between years)
dat_subset$nest_season<-paste(dat_subset$Nest, dat_subset$Season, sep="")



#erase impossible values (below 19 and over 36)
dat_subset <- subset(dat_subset, fledging_mass>19 & fledging_mass<36)


# erase impossible tarsus values
dat_subset <- subset(dat_subset, tarsus_dad>19 & tarsus_dad<25.5)
dat_subset <- subset(dat_subset, tarsus_mom>19 & tarsus_mom<25.5)




#Scale continuous variables
dat_subset$breeding_exp_pair_days.z <-scale(dat_subset$breeding_exp_pair_days)
dat_subset$breeding_exp_Dad_days.z <-scale(dat_subset$breeding_exp_Dad_days)
dat_subset$breeding_exp_Mom_days.z <-scale(dat_subset$breeding_exp_Mom_days)
dat_subset$Mom_MinAge.z <-scale(dat_subset$Mom_MinAge)
dat_subset$Dad_MinAge.z <- scale(dat_subset$Dad_MinAge)

dat_subset$Col_Size.z<- scale(dat_subset$Col_Size)

dat_subset$tarsus_mom.z<- scale(dat_subset$tarsus_mom)
dat_subset$tarsus_dad.z<- scale(dat_subset$tarsus_dad)
dat_subset$wing_chick.z <- scale(dat_subset$wing_chick)

#dat_subset$Final_clutch_size.z<-scale(dat_subset$Final_clutch_size)
dat_subset$N_hatched.z<-scale(dat_subset$N_hatched)



hist(dat_subset$fledging_mass, breaks = 20)




###MODEL ####
mod1 = brms::brm(fledging_mass ~ breeding_exp_pair_days.z *predation+
                   breeding_exp_Mom_days.z*predation+
                   breeding_exp_Dad_days.z*predation+
                   Mom_MinAge.z*predation+
                   Dad_MinAge.z*predation+
                   Col_Size.z+
                   tarsus_mom.z+
                   tarsus_dad.z+
                   wing_chick.z+
                   N_hatched.z+
                   mo(season_of_protection)+
                   (1|Season)  + (1|Colony)+(1|nest_season) +
                   (1|ColNestLaying) + (1|ringer)+
                   (breeding_exp_pair_days.z|Pair) +
                   (breeding_exp_Dad_days.z|BreederDad) + 
                   (breeding_exp_Mom_days.z|BreederMom),
                 control = list(adapt_delta = 0.99, max_treedepth = 10),
                 iter = 10000,
                 cores = 4,
                 #prior = set_prior('normal(0, 3)'),
                 data = dat_subset,
                 file="fledging_mass_long")


#check model fitting
pp_check(mod1)
plot(mod1)
summary(mod1)




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
                   mean(dat_subset$breeding_exp_pair_days),y=dat_subset$fledging_mass[dat_subset$predation=="natural"],
                 color="natural"),alpha=0.2) +
  
  geom_point(aes(x=(sd(dat_subset$breeding_exp_pair_days)*dat_subset$breeding_exp_pair_days.z[dat_subset$predation=="protected"]+
                      mean(dat_subset$breeding_exp_pair_days)),y=dat_subset$fledging_mass[dat_subset$predation=="protected"],
                 color="protected"),alpha=0.2) +
  
  labs(x="",
       y="Fledging mass \n",
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
  theme(legend.position = c(0.85, 0.9),
        legend.box="horizontal")+
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
                            "Pair duration:Predation(protected)","Brood size", "Nestling wing length", "Male tarsus", "Female tarsus", 
                            "Colony size", "Male age","Female age","Male experience","Female experience",
                            "Predation(protected)","Pair duration"))+
  #
  #scale_color_sjplot("simply")+ 
  ylim(-.9, 1.25)+ 
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

tab_model(mod1,file="fledging_mass_review2.doc", transform=NULL, show.icc = FALSE, show.zeroinf = FALSE, show.re.var=TRUE)


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


random_Ringer <- data.frame(summ.$random$ringer)
random_Ringer <- cbind(rownames(random_Ringer), data.frame(random_Ringer, row.names=NULL))
colnames(random_Ringer)[colnames(random_Ringer)=="rownames(random_Ringer)"] <-"Random effects"
random_Ringer<-random_Ringer[,c(1,2,4,5)]

random_<-rbind(random_Pair,random_Breeder_Dad,random_BreederMom, random_Colony,random_Season,
               random_Nest, random_Brood, random_Ringer)

random_f<-cbind(random_[,1],round(random_[,2:4],2))
random_f$CrI<-paste0(paste(random_f$l.95..CI, random_f$u.95..CI, sep = " - "))

random_f$l.95..CI <- NULL
random_f$u.95..CI <- NULL

colnames(random_f)[colnames(random_f)=="random_[, 1]"] <- "Variables"

random_f$Variables<- c("Pair ID (511)", "slope Pair duration | Pair ID","corr. Pair duration - Pair ID",
                       "Male ID (372)", "slope Male experience | Male ID", "corr. Male experience - Male ID",
                       "Female ID (412)", "slope Female experience | Female ID","corr. Female experience - Female ID",
                       "Colony ID (18)", "Season (10)", "Nest (448)", "Brood (937)", "Ringer (22)")


# N Season	10
# N Colony	18
# N nest_season	448
# N ColNestLaying	937
# N ringer	22
# N Pair	511
# N BreederDad	372
# N BreederMom	412


write.csv(random_f, "random_long_review2.csv", row.names = F)



####Extract fixed factor estimates for paper####
s<-data.frame(fixef(mod1,summary = F))

#The reference group is natural so you have to add the interaction with protected if you want to extract their credible interval
mean(s$breeding_exp_pair_days.z+s$breeding_exp_pair_days.z.predationprotected)
#0.1454786
quantile(s$breeding_exp_pair_days.z+s$breeding_exp_pair_days.z.predationprotected,probs = c(0.025, 0.975))
#       2.5%     97.5% 
# -0.1614677  0.4577880  


mean(s$breeding_exp_pair_days.z)
#-0.05102819
quantile(s$breeding_exp_pair_days.z,probs = c(0.025, 0.975))
#  2.5%      97.5% 
# -0.3479663  0.2465450 



#male exp nat
mean(s$breeding_exp_Dad_days.z)
# 0.3487341
quantile(s$breeding_exp_Dad_days.z,probs = c(0.025, 0.975))
#   2.5%       97.5% 
#   0.004268472 0.688213919 






#####cross-sectional#########
#fixed breeding experience

#load dataset
dat_fixed_focal_breeding_exp<- read.csv("YOUR_DIRECTORY/Cross-sectional_dataset.csv")

#for colonies in the natural state, assign 0 at the length of protection
dat_fixed_focal_breeding_exp$season_of_protection[is.na(dat_fixed_focal_breeding_exp$season_of_protection)] <- 0


#select the variables of interest
dat_fixed_focal_breeding_exp<-dat_fixed_focal_breeding_exp[,c("fledging_mass", "N_hatched", "breeding_exp_pair_days", "predation",  
                                                              "sex", "Season", "breeding_exp_Mate_days", "Colony",
                                                              "Col_Size","wing_chick", "ringer",
                                                              "Focal_MinAge", "Mate_MinAge", "season_of_protection","Nest","unique_ID",
                                                              "Pair", "ring", "Mate_ID","ColNestLaying", "tarsus_Focal","tarsus_Mate")]







#erase the incomplete cases
dat_fixed_focal_breeding_exp<-dat_fixed_focal_breeding_exp[complete.cases(dat_fixed_focal_breeding_exp), ] #select only complete cases

#erase duplicates
dat_fixed_focal_breeding_exp<-unique(dat_fixed_focal_breeding_exp)


#variable transformation
dat_fixed_focal_breeding_exp$Colony<- as.factor(dat_fixed_focal_breeding_exp$Colony)
dat_fixed_focal_breeding_exp$sex<- as.factor(dat_fixed_focal_breeding_exp$sex)



##Scaling
dat_fixed_focal_breeding_exp$breeding_exp_Mate_days.z<- scale(dat_fixed_focal_breeding_exp$breeding_exp_Mate_days)
dat_fixed_focal_breeding_exp$breeding_exp_pair_days.z<- scale(dat_fixed_focal_breeding_exp$breeding_exp_pair_days)
dat_fixed_focal_breeding_exp$Focal_MinAge.z<- scale(dat_fixed_focal_breeding_exp$Focal_MinAge)
dat_fixed_focal_breeding_exp$Mate_MinAge.z<- scale(dat_fixed_focal_breeding_exp$Mate_MinAge)
dat_fixed_focal_breeding_exp$Col_Size.z<- scale(dat_fixed_focal_breeding_exp$Col_Size)
dat_fixed_focal_breeding_exp$tarsus_Focal.z <- scale(dat_fixed_focal_breeding_exp$tarsus_Focal)
dat_fixed_focal_breeding_exp$tarsus_Mate.z <- scale(dat_fixed_focal_breeding_exp$tarsus_Mate)

dat_fixed_focal_breeding_exp$N_hatched.z <- scale(dat_fixed_focal_breeding_exp$N_hatched)
dat_fixed_focal_breeding_exp$wing_chick.z <- scale(dat_fixed_focal_breeding_exp$wing_chick)



###MODEL ####
mod1 = brms::brm(fledging_mass ~ breeding_exp_pair_days.z*predation +
                   breeding_exp_Mate_days.z*predation+
                   Focal_MinAge.z*predation+
                   Mate_MinAge.z*predation+
                   Col_Size.z+
                   tarsus_Focal.z+
                   tarsus_Mate.z+
                   wing_chick.z+
                   N_hatched.z+
                   sex+
                   (1|Colony) + (1|ColNestLaying) +
                   (1|Season)+ (1|Pair) +(1|ringer) + (1|Mate_ID) +(1|ring),
                 control = list(adapt_delta = 0.9999, max_treedepth = 12),
                 iter = 8000,
                 cores = 4,
                 #prior = set_prior('normal(0, 3)'),
                 data = dat_fixed_focal_breeding_exp,
                 file="fledging_mass_cross")



#check model fitting
pp_check(mod1)
plot(mod1)
summary(mod1)


###PLOT####


#extract Plotting values
marg<-conditional_effects(mod1,"breeding_exp_pair_days.z:predation", resolution=1000)

#I choose to extract the estimates without random factors because graphically better. 
# (provide the scripts in the supplementary material to be clear about it )


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
                   mean(dat_fixed_focal_breeding_exp$breeding_exp_pair_days),y=dat_fixed_focal_breeding_exp$fledging_mass[dat_fixed_focal_breeding_exp$predation=="natural"],
                 color="natural"),alpha=0.2) +
  
  geom_point(aes(x=(sd(dat_fixed_focal_breeding_exp$breeding_exp_pair_days)*dat_fixed_focal_breeding_exp$breeding_exp_pair_days.z[dat_fixed_focal_breeding_exp$predation=="protected"]+
                      mean(dat_fixed_focal_breeding_exp$breeding_exp_pair_days))+0.2,y=dat_fixed_focal_breeding_exp$fledging_mass[dat_fixed_focal_breeding_exp$predation=="protected"],
                 color="protected"),alpha=0.2) +
  
  labs(x="\nPair duration (days)",
       y="Fledging mass \n",
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
  theme(legend.position = c(0.45, 0.9),
        legend.box="horizontal")+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))



#forest plot
forest2<-plot_model(mod1,type="std2", vline.color = "black", line.size = .5, 
                    sort.est = FALSE, transform = NULL, show.values = TRUE,
                    ci.style="whisker", value.size=3.5,value.offset=.25, dot.size = 2,
                    prob.inner=0,prob.outer=.95, width=0.2,
                    bpe.style = "dot",  bpe.color = "black")+ 
  scale_x_discrete(labels=c("Mate age:Predation(protected)","Focal age:Predation(protected)",
                            "Mate experience:Predation(protected)","Pair duration:Predation(protected)",
                            "Sex(female)", "Brood size", "Nestling wing length","Tarsus mate", "Tarsus focal",
                            "Colony size","Mate age","Focal age","Mate experience","Predation(protected)","Pair duration"))+
  #scale_color_sjplot("simply")+
  ylim(-2, 2.2)+ 
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

tab_model(mod1,file="fledging_mass_cross_review2.doc", transform=NULL, show.icc = FALSE, show.zeroinf = FALSE, show.re.var=TRUE)


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


random_Ringer <- data.frame(summ.$random$ringer)
random_Ringer <- cbind(rownames(random_Ringer), data.frame(random_Ringer, row.names=NULL))
colnames(random_Ringer)[colnames(random_Ringer)=="rownames(random_Ringer)"] <-"Random effects"
random_Ringer<-random_Ringer[,c(1,2,4,5)]

random_<-rbind(random_Pair,random_FocalID,random_MateID, random_Colony,random_Season,
                random_Brood, random_Ringer)

random_f<-cbind(random_[,1],round(random_[,2:4],2))
random_f$CrI<-paste0(paste(random_f$l.95..CI, random_f$u.95..CI, sep = " - "))

random_f$l.95..CI <- NULL
random_f$u.95..CI <- NULL

colnames(random_f)[colnames(random_f)=="random_[, 1]"] <- "Variables"

random_f$Variables<- c("Pair ID (72)", 
                       "Focal ID (97)", 
                       "Mate ID (96)",  
                       "Colony ID (12)", "Season (7)", "Brood (74)", "Ringer (12)")


# N Colony	12
# N ColNestLaying	74
# N Season	7
# N Pair	72
# N ringer	12
# N Mate_ID	96
# N ring	97


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

dat_subset2$tarsus_mom.z<- scale(dat_subset2$tarsus_mom)
dat_subset2$tarsus_dad.z<- scale(dat_subset2$tarsus_dad)
dat_subset2$wing_chick.z <- scale(dat_subset2$wing_chick)

#dat_subset$Final_clutch_size.z<-scale(dat_subset$Final_clutch_size)
dat_subset2$N_hatched.z<-scale(dat_subset2$N_hatched)


mod2 = brms::brm(fledging_mass ~ breeding_exp_pair_days.z *predation+
                   breeding_exp_Mom_days.z*predation+
                   breeding_exp_Dad_days.z*predation+
                   Mom_MinAge.z*predation+
                   Dad_MinAge.z*predation+
                   Col_Size.z+
                   tarsus_mom.z+
                   tarsus_dad.z+
                   wing_chick.z+
                   N_hatched.z+
                   mo(season_of_protection)+
                   (1|Season)  + (1|Colony)+(1|nest_season) +
                   (1|ColNestLaying) + (1|ringer)+
                   (breeding_exp_pair_days.z|Pair) +
                   (breeding_exp_Dad_days.z|BreederDad) + 
                   (breeding_exp_Mom_days.z|BreederMom),
                 control = list(adapt_delta = 0.99, max_treedepth = 10),
                 iter = 10000,
                 cores = 4,
                 #prior = set_prior('normal(0, 3)'),
                 data = dat_subset2,
                 file="fledging_mass_long_trim_rev2")





####Extract fixed factor estimates for paper####
s<-data.frame(fixef(mod2,summary = F))

#The reference group is natural so you have to add the interaction with protected if you want to extract their credible interval
mean(s$breeding_exp_pair_days.z+s$breeding_exp_pair_days.z.predationprotected)
#0.1206867
quantile(s$breeding_exp_pair_days.z+s$breeding_exp_pair_days.z.predationprotected,probs = c(0.025, 0.975))
#       2.5%     97.5% 
# -0.1720275  0.4133588 


mean(s$breeding_exp_pair_days.z)
#-0.06319816
quantile(s$breeding_exp_pair_days.z,probs = c(0.025, 0.975))
#  2.5%      97.5% 
# -0.3479057  0.2217148 



#male exp nat
mean(s$breeding_exp_Dad_days.z)
#0.333099
quantile(s$breeding_exp_Dad_days.z,probs = c(0.025, 0.975))
#    2.5%       97.5% 
#   -0.001934375  0.665480166 




#results are nearly identical



