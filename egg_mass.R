#Aim: Describe influence of pair duration, experience and age on the size of the eggs
#Author: P. D'Amelio



### libraries  ####
library(brms)
library(loo)
library(sjPlot)
library(ggplot2)
library(ggpubr)



### data handling ####

#load database ##LONGITUDINAL database 

dat<- read.csv("YOUR_DIRECTORY/Longitudinal_dataset.csv")



#for colonies in the natural state, assign 0 at the length of protection
dat$season_of_protection[is.na(dat$season_of_protection)] <- 0


#select the variables of interest
dat_subset<-dat[,c( "egg_mass", "Final_clutch_size","Mom_breeding_attempt_ordinal_season",
                    "breeding_exp_pair_days" , "season_of_protection",
                    "Pair", "Season", "predation", "breeding_exp_Mom_days", "breeding_exp_Dad_days",
                    "Col_Size", "Mom_MinAge", "Dad_MinAge", "Colony", "Nest", "tarsus_mom",
                    "BreederMom", "BreederDad","ColNestLaying", "unique_ID")] 





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

dat_subset$tarsus_mom.z<- scale(dat_subset$tarsus_mom)
dat_subset$Final_clutch_size.z<-scale(dat_subset$Final_clutch_size)

hist(dat_subset$egg_mass)



###MODEL ####

mod1 = brms::brm(egg_mass ~ breeding_exp_pair_days.z*predation +
                   breeding_exp_Mom_days.z*predation+
                   breeding_exp_Dad_days.z*predation+
                   Mom_MinAge.z*predation+
                   Dad_MinAge.z*predation+
                   Col_Size.z+
                   tarsus_mom.z+
                   Final_clutch_size.z+
                   mo(Mom_breeding_attempt_ordinal_season)+
                   mo(season_of_protection)+
                   (1|Season)  + (1|ColNestLaying) + (1|Colony)+
                   (1|nest_season)+
                   (breeding_exp_pair_days.z|Pair) +
                   (breeding_exp_Dad_days.z|BreederDad) + 
                   (breeding_exp_Mom_days.z|BreederMom),
                 control = list(adapt_delta = 0.99, max_treedepth = 12),
                 iter = 10000,
                 cores = 4,
                 #prior = set_prior('normal(0, 3)'),
                 data = dat_subset,
                 file="eggMass_long")


#check model fitting
pp_check(mod1)
conditional_effects(mod1)
summary(mod1)






###PLOT####


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
                   mean(dat_subset$breeding_exp_pair_days),y=dat_subset$egg_mass[dat_subset$predation=="natural"],
                 color="natural"),alpha=0.2) +
  
  geom_point(aes(x=(sd(dat_subset$breeding_exp_pair_days)*dat_subset$breeding_exp_pair_days.z[dat_subset$predation=="protected"]+
                      mean(dat_subset$breeding_exp_pair_days))+0.2,y=dat_subset$egg_mass[dat_subset$predation=="protected"],
                 color="protected"),alpha=0.2) +
  
  labs(x=" ",
       y="Egg mass \n",
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
                   ci.style="whisker", value.size=3.5,value.offset=.25, dot.size = 2,
                   prob.inner=0,prob.outer=.95, width=0.2,
                   bpe.style = "dot",  bpe.color = "black",
                   rm.terms = c("simo_moseason_of_protection1[1]",
                                "simo_moseason_of_protection1[2]",
                                "simo_moseason_of_protection1[3]",
                                "simo_moseason_of_protection1[4]",
                                "simo_moseason_of_protection1[5]",
                                "simo_moseason_of_protection1[6]",
                                "simo_moseason_of_protection1[7]",
                                "simo_moMom_breeding_attempt_ordinal_season1[1]",
                                "simo_moMom_breeding_attempt_ordinal_season1[2]",
                                "simo_moMom_breeding_attempt_ordinal_season1[3]",
                                "simo_moMom_breeding_attempt_ordinal_season1[4]",
                                "simo_moMom_breeding_attempt_ordinal_season1[5]",
                                "simo_moMom_breeding_attempt_ordinal_season1[6]",
                                "simo_moMom_breeding_attempt_ordinal_season1[7]",
                                "simo_moMom_breeding_attempt_ordinal_season1[8]",
                                "simo_moMom_breeding_attempt_ordinal_season1[9]",
                                "simo_moMom_breeding_attempt_ordinal_season1[10]",
                                "simo_moMom_breeding_attempt_ordinal_season1[11]",
                                "simo_moMom_breeding_attempt_ordinal_season1[12]"))+ 
  scale_x_discrete(labels=c("Seasons of protection", "Clutch order","Male age:Predation(protected)","Female age:Predation(protected)",
                            "Male experience:Predation(protected)","Female experience:Predation(protected)",
                            "Pair duration:Predation(protected)", "Clutch size", "Female tarsus",
                            "Colony size","Male age","Female age","Male experience","Female experience","Predation(protected)","Pair duration"))+
  #scale_color_sjplot("simply")+
  ylim(-.1, .1)+ 
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



#longitudinal and cross-sectional in one figure (run after running the cross-sectional part of the script)

ggarrange(int, forest, int2, forest2, nrow=2, ncol=2, align = "h",widths=c(1,1), labels="AUTO")
#erase labels in top plots
#export pdf 14x14 landscape











####TABLE #####

##save table summary

tab_model(mod1,file="egg_mass_review2.doc", transform=NULL, show.icc = FALSE, show.zeroinf = FALSE, show.re.var=TRUE)


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


random_<-rbind(random_Pair,random_Breeder_Dad,random_BreederMom, random_Colony,random_Season, random_Nest, random_Brood)

random_f<-cbind(random_[,1],round(random_[,2:4],2))
random_f$CrI<-paste0(paste(random_f$l.95..CI, random_f$u.95..CI, sep = " - "))

random_f$l.95..CI <- NULL
random_f$u.95..CI <- NULL

colnames(random_f)[colnames(random_f)=="random_[, 1]"] <- "Variables"

random_f$Variables<- c("Pair ID (579)", "slope Pair duration | Pair ID","corr. Pair duration - Pair ID",
                       "Male ID (401)", "slope Male experience | Male ID","corr. Male experience - Male ID",
                       "Female ID (432)", "slope Female experience | Female ID","corr. Female experience - Female ID",
                       "Colony ID (21)", "Season (10)", "Nest (531)", "Brood (2075)")



# N Season	10
# N ColNestLaying	2075
# N Colony	21
# N nest_season	531
# N Pair	579
# N BreederDad	401
# N BreederMom	432



write.csv(random_f, "random_review2.csv", row.names = F)






####Extract fixed factor estimates for paper####
s<-data.frame(fixef(mod1,summary = F))

#The reference group is natural so you have to add the interaction with protected if you want to extract their credible interval
mean(s$breeding_exp_pair_days.z+s$breeding_exp_pair_days.z.predationprotected)
#0.0007041958
quantile(s$breeding_exp_pair_days.z+s$breeding_exp_pair_days.z.predationprotected,probs = c(0.025, 0.975))
#   2.5%         97.5% 
#   -0.01738509  0.01875095   


mean(s$breeding_exp_pair_days.z)
# 0.005879288
quantile(s$breeding_exp_pair_days.z,probs = c(0.025, 0.975))
  #   2.5%        97.5% 
  # -0.008572801  0.020592755  




#female exp prot
mean(s$breeding_exp_Mom_days.z+s$breeding_exp_Mom_days.z)
#-0.03253443
quantile(s$breeding_exp_Mom_days.z+s$predationprotected.breeding_exp_Mom_days.z,probs = c(0.025, 0.975))
#    2.5%        97.5% 
#-0.054538781 -0.004422501 








#####cross-sectional#########
#fixed breeding experience

#load dataset
dat_fixed_focal_breeding_exp<- read.csv("YOUR_DIRECTORY/Cross-sectional_dataset.csv")


#for colonies in the natural state, assign 0 at the length of protection
dat_fixed_focal_breeding_exp$season_of_protection[is.na(dat_fixed_focal_breeding_exp$season_of_protection)] <- 0

#select the variables of interest
dat_fixed_focal_breeding_exp<-dat_fixed_focal_breeding_exp[,c("egg_mass", "Final_clutch_size", "breeding_exp_pair_days", "predation",  
                                                              "sex", "Season", "breeding_exp_Mate_days", "Colony", "Col_Size",
                                                              "Focal_MinAge", "Mate_MinAge", "season_of_protection","Nest","unique_ID",
                                                              "Pair", "ring", "Mate_ID","ColNestLaying", "tarsus_Focal","tarsus_Mate")]





#erase the incomplete cases
dat_fixed_focal_breeding_exp<-dat_fixed_focal_breeding_exp[complete.cases(dat_fixed_focal_breeding_exp), ] #select only complete cases

#erase duplicates
dat_fixed_focal_breeding_exp<-unique(dat_fixed_focal_breeding_exp)


#variable transformation
dat_fixed_focal_breeding_exp$Colony<- as.factor(dat_fixed_focal_breeding_exp$Colony)
dat_fixed_focal_breeding_exp$sex<- as.factor(dat_fixed_focal_breeding_exp$sex)



#run model only for females
dat_fixed_focal_breeding_exp_f<-subset(dat_fixed_focal_breeding_exp, sex=="2")


##Scaling
dat_fixed_focal_breeding_exp_f$breeding_exp_Mate_days.z<- scale(dat_fixed_focal_breeding_exp_f$breeding_exp_Mate_days)
dat_fixed_focal_breeding_exp_f$breeding_exp_pair_days.z<- scale(dat_fixed_focal_breeding_exp_f$breeding_exp_pair_days)
dat_fixed_focal_breeding_exp_f$Focal_MinAge.z<- scale(dat_fixed_focal_breeding_exp_f$Focal_MinAge)
dat_fixed_focal_breeding_exp_f$Mate_MinAge.z<- scale(dat_fixed_focal_breeding_exp_f$Mate_MinAge)
dat_fixed_focal_breeding_exp_f$Col_Size.z<- scale(dat_fixed_focal_breeding_exp_f$Col_Size)
dat_fixed_focal_breeding_exp_f$tarsus_Focal.z <- scale(dat_fixed_focal_breeding_exp_f$tarsus_Focal)
dat_fixed_focal_breeding_exp_f$Final_clutch_size.z <- scale(dat_fixed_focal_breeding_exp_f$Final_clutch_size)




###MODEL ####

mod1 = brms::brm(egg_mass ~ breeding_exp_pair_days.z*predation +
                   breeding_exp_Mate_days.z*predation+
                   Focal_MinAge.z*predation+
                   Mate_MinAge.z*predation+
                   Col_Size.z+
                   tarsus_Focal.z+
                   Final_clutch_size.z+
                   (1|Colony) + (1|ColNestLaying) +
                  (1|Season)+ (1|Pair) + (1|ring) + (1|Mate_ID),
                 control = list(adapt_delta = 0.9999, max_treedepth = 12),
                 iter = 8000,
                 cores = 4,
                 #prior = set_prior('normal(0, 3)'),
                 data = dat_fixed_focal_breeding_exp_f,
                 file="eggMass_cross")





#check model fitting
pp_check(mod1)
plot(mod1)
summary(mod1)




###PLOT####


#extract Plotting values
marg<-conditional_effects(mod1,"breeding_exp_pair_days.z:predation", resolution=1000)

#I choose to extract the estimates without random factors \
#select the variable of interest
pair_duration1<-marg$breeding_exp_pair_days.z




#main plot
int2<-ggplot()+
  geom_line(aes(x=sd(dat_fixed_focal_breeding_exp_f$breeding_exp_pair_days)*
                  pair_duration1$breeding_exp_pair_days.z[pair_duration1$predation=="natural"]+
                  mean(dat_fixed_focal_breeding_exp_f$breeding_exp_pair_days),
                y=pair_duration1$estimate__[pair_duration1$predation=="natural"],color="natural"),
            size=1.3)+
  
  geom_line(aes(x=sd(dat_fixed_focal_breeding_exp_f$breeding_exp_pair_days)*
                  pair_duration1$breeding_exp_pair_days.z[pair_duration1$predation=="protected"]+
                  mean(dat_fixed_focal_breeding_exp_f$breeding_exp_pair_days),
                y=pair_duration1$estimate__[pair_duration1$predation=="protected"], color="protected"),
            size=1.3)+
  
  geom_ribbon(aes(x=sd(dat_fixed_focal_breeding_exp_f$breeding_exp_pair_days)*pair_duration1$breeding_exp_pair_days.z[pair_duration1$predation=="natural"]+
                    mean(dat_fixed_focal_breeding_exp_f$breeding_exp_pair_days),
                  y=pair_duration1$estimate__[pair_duration1$predation=="natural"], 
                  ymax=pair_duration1$upper__[pair_duration1$predation=="natural"],
                  ymin=pair_duration1$lower__[pair_duration1$predation=="natural"]),
              fill="#007373",alpha=0.3)+
  
  geom_ribbon(aes(x=sd(dat_fixed_focal_breeding_exp_f$breeding_exp_pair_days)*pair_duration1$breeding_exp_pair_days.z[pair_duration1$predation=="protected"]+
                    mean(dat_fixed_focal_breeding_exp_f$breeding_exp_pair_days),
                  y=pair_duration1$estimate__[pair_duration1$predation=="protected"], 
                  ymax=pair_duration1$upper__[pair_duration1$predation=="protected"],
                  ymin=pair_duration1$lower__[pair_duration1$predation=="protected"]),
              fill="#D55E00",alpha=0.3)+
  
  geom_point(aes(x=sd(dat_fixed_focal_breeding_exp_f$breeding_exp_pair_days)*dat_fixed_focal_breeding_exp_f$breeding_exp_pair_days.z[dat_fixed_focal_breeding_exp_f$predation=="natural"]+
                   mean(dat_fixed_focal_breeding_exp_f$breeding_exp_pair_days),y=dat_fixed_focal_breeding_exp_f$egg_mass[dat_fixed_focal_breeding_exp_f$predation=="natural"],
                 color="natural"),alpha=0.2) +
  
  geom_point(aes(x=(sd(dat_fixed_focal_breeding_exp_f$breeding_exp_pair_days)*dat_fixed_focal_breeding_exp_f$breeding_exp_pair_days.z[dat_fixed_focal_breeding_exp_f$predation=="protected"]+
                      mean(dat_fixed_focal_breeding_exp_f$breeding_exp_pair_days))+0.2,y=dat_fixed_focal_breeding_exp_f$egg_mass[dat_fixed_focal_breeding_exp_f$predation=="protected"],
                 color="protected"),alpha=0.2) +
  
  labs(x="\nPair duration (days)",
       y="Egg mass \n",
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
  theme(legend.position = c(0.18, 0.9),
        legend.box="horizontal")+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))



#forest

forest2<-plot_model(mod1,type="std2", vline.color = "black", line.size = .5, 
                   sort.est = FALSE, transform = NULL, show.values = TRUE,
                   ci.style="whisker", value.size=3.5,value.offset=.25, dot.size = 2,
                   prob.inner=0,prob.outer=.95, width=0.2,
                   bpe.style = "dot",  bpe.color = "black")+ 
  scale_x_discrete(labels=c("Mate age:Predation(protected)","Focal age:Predation(protected)",
                            "Mate experience:Predation(protected)","Pair duration:Predation(protected)", "Clutch size", "Tarsus focal",
                            "Colony size","Mate age","Focal age","Mate experience","Predation(protected)","Pair duration"))+
  #scale_color_sjplot("simply")+
  ylim(-.25, .25)+ 
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

tab_model(mod1,file="egg_mass_cross_review2.doc", transform=NULL, show.icc = FALSE, show.zeroinf = FALSE, show.re.var=TRUE)


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


random_<-rbind(random_Pair,random_FocalID,random_MateID, random_Colony,random_Season, random_Brood)

random_f<-cbind(random_[,1],round(random_[,2:4],2))
random_f$CrI<-paste0(paste(random_f$l.95..CI, random_f$u.95..CI, sep = " - "))

random_f$l.95..CI <- NULL
random_f$u.95..CI <- NULL

colnames(random_f)[colnames(random_f)=="random_[, 1]"] <- "Variables"

random_f$Variables<- c("Pair ID (98)", 
                       "Focal ID (98)", 
                       "Mate ID (96)", 
                       "Colony ID (12)", "Season (6)",  "Brood (98)")



# N Colony	12
# N ColNestLaying	98
# N Season	6
# N Pair	98
# N ring	98
# N Mate_ID	96

write.csv(random_f, "random_cross_review2.csv", row.names = F)







####Extract fixed factor estimates for paper####
s<-data.frame(fixef(mod1,summary = F))

#The reference group is natural so you have to add the interaction with protected if you want to extract their credible interval
mean(s$breeding_exp_pair_days.z+s$breeding_exp_pair_days.z.predationprotected)
#-0.05015366
quantile(s$breeding_exp_pair_days.z+s$breeding_exp_pair_days.z.predationprotected,probs = c(0.025, 0.975))
#   2.5%         97.5% 
#  -0.12073267    0.02084192   


mean(s$breeding_exp_pair_days.z)
# -0.001128629
quantile(s$breeding_exp_pair_days.z,probs = c(0.025, 0.975))
#   2.5%        97.5% 
# -0.05369368   0.05628615  









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
dat_subset2$Final_clutch_size.z<-scale(dat_subset2$Final_clutch_size)







mod2 = brms::brm(egg_mass ~ breeding_exp_pair_days.z*predation +
                   breeding_exp_Mom_days.z*predation+
                   breeding_exp_Dad_days.z*predation+
                   Mom_MinAge.z*predation+
                   Dad_MinAge.z*predation+
                   Col_Size.z+
                   tarsus_mom.z+
                   Final_clutch_size.z+
                   mo(Mom_breeding_attempt_ordinal_season)+
                   mo(season_of_protection)+
                   (1|Season)  + (1|ColNestLaying) + (1|Colony)+
                   (1|nest_season)+
                   (breeding_exp_pair_days.z|Pair) +
                   (breeding_exp_Dad_days.z|BreederDad) + 
                   (breeding_exp_Mom_days.z|BreederMom),
                 control = list(adapt_delta = 0.99, max_treedepth = 12),
                 iter = 10000,
                 cores = 4,
                 #prior = set_prior('normal(0, 3)'),
                 data = dat_subset2,
                 file="eggMass_long_trim_rev2")

pp_check(mod2)
conditional_effects(mod2)
summary(mod2)





s<-data.frame(fixef(mod2,summary = F))

#The reference group is natural so you have to add the interaction with protected if you want to extract their credible interval
mean(s$breeding_exp_pair_days.z+s$breeding_exp_pair_days.z.predationprotected)
#-1.882357e-05
quantile(s$breeding_exp_pair_days.z+s$breeding_exp_pair_days.z.predationprotected,probs = c(0.025, 0.975))
#   2.5%         97.5% 
#   -0.01742335  0.01744585   


mean(s$breeding_exp_pair_days.z)
# 0.006141406
quantile(s$breeding_exp_pair_days.z,probs = c(0.025, 0.975))
#   2.5%        97.5% 
# -0.007664056  0.019978286 




#female exp prot
mean(s$breeding_exp_Mom_days.z+s$breeding_exp_Mom_days.z)
#-0.03310376
quantile(s$breeding_exp_Mom_days.z+s$predationprotected.breeding_exp_Mom_days.z,probs = c(0.025, 0.975))
#    2.5%        97.5% 
#-0.053227499 -0.003532317  



#results are nearly identical





