#Aim: Potential reasons for increased breeding success in case of long term monogamy: 
#Is relaying interval shorter in more experienced pairs?
#Author: P. D'Amelio


#fate of the clutch influences relaying interval (after predation relay is faster) categorical: successful, predated_eggs, predated_chicks
#also at what stage is the clutch lost is probably important (lost eggs are replaced faster than chicks) 

#relaying interval calculated as: "fate date - laying date" within the same season (and extra cut-off at 3 months),
#only pairs with at least one interval are considered



### libraries  ####
library(brms)
library(loo)
library(sjPlot)
library(ggplot2)
library(ggpubr)



### data handling ####

#load database
dat<- read.csv("YOUR_DIRECTORY/Longitudinal_dataset.csv")



#select only colonies with the same protection state throughout the year
dat<-subset(dat, double_protection_state=="no")


#assign 0 years to protection to natural colonies
dat$season_of_protection[is.na(dat$season_of_protection)] <- 0



#delete impossibly shorts and too long

dat<-subset(dat, Relaying_int<80)
dat<-subset(dat, Relaying_int>0) #impossible intervals, these are probably mistakes that need to be corrected in the database


dat_subset<-dat[,c( "Relaying_int","Fate_clutch","breeding_exp_pair_days",
                    "Pair", "Season", "predation", "breeding_exp_Mom_days", "breeding_exp_Dad_days",
                    "Col_Size", "Mom_MinAge", "Dad_MinAge", "Colony", "Nest",
                    "BreederMom", "BreederDad", "season_of_protection")] 



#delete seasons not fully monitored
dat_subset<-subset(dat_subset, Season!="2008/2009") 
dat_subset<-subset(dat_subset, Season!="2009/2010") 
dat_subset<-subset(dat_subset, Season!="2010/2011")



#control for possible nest effect (nest naming changes between years, but are consistent within year)
dat_subset$nest_season<-paste(dat_subset$Nest, dat_subset$Season, sep="")


#delete duplicates
dat_subset<- unique(dat_subset)

dat_subset<-dat_subset[complete.cases(dat_subset), ] #select only complete cases


# variables that are factors
dat_subset$Colony<- as.factor(dat_subset$Colony)



#Scale continuous variables
dat_subset$breeding_exp_pair_days.z <-scale(dat_subset$breeding_exp_pair_days)
dat_subset$breeding_exp_Dad_days.z <-scale(dat_subset$breeding_exp_Dad_days)
dat_subset$breeding_exp_Mom_days.z <-scale(dat_subset$breeding_exp_Mom_days)
dat_subset$Mom_MinAge.z <-scale(dat_subset$Mom_MinAge)
dat_subset$Dad_MinAge.z <- scale(dat_subset$Dad_MinAge)

dat_subset$Col_Size.z<- scale(dat_subset$Col_Size)

hist(dat_subset$Relaying_int, breaks = seq(0,80,1))


###MODEL ####

#skew normal
mod1 = brms::brm(Relaying_int~ breeding_exp_pair_days.z*predation+
                   breeding_exp_Mom_days.z*predation+
                   breeding_exp_Dad_days.z*predation +  
                   Mom_MinAge.z*predation + 
                   Dad_MinAge.z*predation +
                   Col_Size.z+
                   Fate_clutch+
                   mo(season_of_protection)+
                   (1|Season) + (1|Colony) + (1|nest_season)  + 
                   (breeding_exp_pair_days.z||Pair)+ 
                   (breeding_exp_Dad_days.z||BreederDad)+ 
                   (breeding_exp_Mom_days.z||BreederMom),
                 control = list(adapt_delta = 0.999),
                 iter = 8000,
                 cores = 4,
                 #prior = set_prior('normal(0, 3)'),
                 data = dat_subset,
                 family="skew_normal",
                 file="Relaying_int")



#check model fitting
pp_check(mod1)
plot(mod1)
summary(mod1)


#add loo, a form of information criteria to compare models
mod1<-add_criterion(mod1, "loo",reloo=FALSE, cores=4)



#poisson
mod2 = brms::brm(Relaying_int~ breeding_exp_pair_days.z*predation+
                   breeding_exp_Mom_days.z*predation+
                   breeding_exp_Dad_days.z*predation +  
                   Mom_MinAge.z*predation + 
                   Dad_MinAge.z*predation +
                   Col_Size.z+
                   Fate_clutch+
                   mo(season_of_protection)+
                   (1|Season) + (1|Colony) + (1|nest_season)  + 
                   (breeding_exp_pair_days.z||Pair)+ 
                   (breeding_exp_Dad_days.z||BreederDad)+ 
                   (breeding_exp_Mom_days.z||BreederMom),
                 control = list(adapt_delta = 0.999, max_treedepth = 15),
                 iter = 10000,
                 cores = 4,
                 #prior = set_prior('normal(0, 3)'),
                 data = dat_subset,
                 family="poisson",
                 file="Relaying_int_p")

# 
# Warning messages:
#   1: There were 16000 transitions after warmup that exceeded the maximum treedepth. Increase max_treedepth above 10. See
# https://mc-stan.org/misc/warnings.html#maximum-treedepth-exceeded 
# 2: Examine the pairs() plot to diagnose sampling problems
# 
# 3: The largest R-hat is 1.06, indicating chains have not mixed.
# Running the chains for more iterations may help. See
# https://mc-stan.org/misc/warnings.html#r-hat 
# 4: Bulk Effective Samples Size (ESS) is too low, indicating posterior means and medians may be unreliable.
# Running the chains for more iterations may help. See
# https://mc-stan.org/misc/warnings.html#bulk-ess 
# 5: Tail Effective Samples Size (ESS) is too low, indicating posterior variances and tail quantiles may be unreliable.
# Running the chains for more iterations may help. See
# https://mc-stan.org/misc/warnings.html#tail-ess 

#the large R-hat is for a random factor, all the rest seems well estimated.


#check model fitting
pp_check(mod2)
plot(mod2)
summary(mod2)

#add loo, a form of information criteria to compare models
mod2<-add_criterion(mod2, "loo",reloo=FALSE, cores=4)


#compare the 2 models
loo_compare(mod1, mod2)
#skew normal much much better...


#####PLOT#####

#extract Plotting values
marg<-conditional_effects(mod1,"breeding_exp_pair_days.z:predation", resolution=1000)

#I choose to extract the estimates without random factors.


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
                   mean(dat_subset$breeding_exp_pair_days),y=dat_subset$Relaying_int[dat_subset$predation=="natural"],
                 color="natural"),alpha=0.2) +
  
  geom_point(aes(x=(sd(dat_subset$breeding_exp_pair_days)*dat_subset$breeding_exp_pair_days.z[dat_subset$predation=="protected"]+
                      mean(dat_subset$breeding_exp_pair_days))+0.2,y=dat_subset$Relaying_int[dat_subset$predation=="protected"],
                 color="protected"),alpha=0.2) +
  
  labs(x="\nPair duration (days)",
       y="Days until following breeding attempt \n",
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
                            "Pair duration:Predation(protected)","Successful", "Failed eggs", 
                            "Colony size", "Male age","Female age","Male experience","Female experience",
                            "Predation(protected)","Pair duration"))+
  #
  #scale_color_sjplot("simply")+ 
  ylim(-3, 13)+ 
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


#merge the 2 plots

ggarrange(int, forest, nrow=1, ncol=2, align = "h",widths=c(1,1), labels="AUTO")
#export pdf 7x14 landscape






####TABLE #####

##save table summary

tab_model(mod1,file="relaying_int_review2.doc", transform=NULL, show.icc = FALSE, show.zeroinf = FALSE, show.re.var=TRUE)


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


random_<-rbind(random_Pair,random_Breeder_Dad,random_BreederMom, random_Colony,random_Season, random_Nest)

random_f<-cbind(random_[,1],round(random_[,2:4],2))
random_f$CrI<-paste0(paste(random_f$l.95..CI, random_f$u.95..CI, sep = " - "))

random_f$l.95..CI <- NULL
random_f$u.95..CI <- NULL

colnames(random_f)[colnames(random_f)=="random_[, 1]"] <- "Variables"

random_f$Variables<- c("Pair ID (435)", "slope Pair duration | Pair ID",
                       "Male ID (341)", "slope Male experience | Male ID",
                       "Female ID (360)", "slope Female experience | Female ID",
                       "Colony ID (16)", "Season (8)", "Nest (407)")

# N Season	8
# N Colony	16
# N nest_season	407
# N Pair	435
# N BreederDad	341
# N BreederMom	360


write.csv(random_f, "random_review2.csv", row.names = F)




####Extract fixed factor estimates for paper####
s<-data.frame(fixef(mod1,summary = F))

#The reference group is natural so you have to add the interaction with protected if you want to extract their credible interval
mean(s$breeding_exp_pair_days.z+s$breeding_exp_pair_days.z.predationprotected)
#-0.1643845
quantile(s$breeding_exp_pair_days.z+s$breeding_exp_pair_days.z.predationprotected,probs = c(0.025, 0.975))
#       2.5%     97.5% 
#  -1.699318  1.387056 


mean(s$breeding_exp_pair_days.z)
#-0.000728543
quantile(s$breeding_exp_pair_days.z,probs = c(0.025, 0.975))
  #  2.5%      97.5% 
  # -0.9849696  0.9967656 


#male exp prot

mean(s$breeding_exp_Dad_days.z+s$predationprotected.breeding_exp_Dad_days.z)
#0.500115
quantile(s$breeding_exp_Dad_days.z+s$predationprotected.breeding_exp_Dad_days.z,probs = c(0.025, 0.975))
#   2.5%      97.5% 
#   -1.206408  2.068398  






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




mod1 = brms::brm(Relaying_int~ breeding_exp_pair_days.z*predation+
                   breeding_exp_Mom_days.z*predation+
                   breeding_exp_Dad_days.z*predation +  
                   Mom_MinAge.z*predation + 
                   Dad_MinAge.z*predation +
                   Col_Size.z+
                   Fate_clutch+
                   mo(season_of_protection)+
                   (1|Season) + (1|Colony) + (1|nest_season)  + 
                   (breeding_exp_pair_days.z||Pair)+ 
                   (breeding_exp_Dad_days.z||BreederDad)+ 
                   (breeding_exp_Mom_days.z||BreederMom),
                 control = list(adapt_delta = 0.999),
                 iter = 8000,
                 cores = 4,
                 #prior = set_prior('normal(0, 3)'),
                 data = dat_subset2,
                 family="skew_normal",
                 file="Relaying_int_trim_rev2")






s<-data.frame(fixef(mod1,summary = F))

#The reference group is natural so you have to add the interaction with protected if you want to extract their credible interval
mean(s$breeding_exp_pair_days.z+s$breeding_exp_pair_days.z.predationprotected)
#-0.2036881
quantile(s$breeding_exp_pair_days.z+s$breeding_exp_pair_days.z.predationprotected,probs = c(0.025, 0.975))
#       2.5%     97.5% 
#  -1.704406  1.330728 


mean(s$breeding_exp_pair_days.z)
#0.04144634
quantile(s$breeding_exp_pair_days.z,probs = c(0.025, 0.975))
#  2.5%      97.5% 
# -0.9189539  0.9940205  


#results are nearly identical



