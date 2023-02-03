#Aim: Potential reasons for increased breeding success in case of long term monogamy: 
#do already established pair breed earlier?
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
dat_subset<-dat[,c( "julian_date_first","breeding_exp_pair_days_season_earliest" , 
                    "Pair", "Season", "predation", "breeding_exp_Mom_days_season_earliest", "breeding_exp_Dad_days_season_earliest",
                    "Col_Size", "Min_Age_Mom_season_days_earliest", "Min_Age_Dad_season_days_earliest", "Colony",
                    "BreederMom", "BreederDad", "season_of_protection")] 

# "julian_date_first" Puts 1 to the first pair breeding for each season

#remove NAs, duplicates
dat_subset<- unique(dat_subset)

dat_subset<-dat_subset[complete.cases(dat_subset), ] #select only complete cases

# variables that are factors
dat_subset$Colony<- as.factor(dat_subset$Colony)



#Scale continuous variables
dat_subset$breeding_exp_pair_days_season_earliest.z <-scale(dat_subset$breeding_exp_pair_days_season_earliest)
dat_subset$breeding_exp_Dad_days_season_earliest.z <-scale(dat_subset$breeding_exp_Dad_days_season_earliest)
dat_subset$breeding_exp_Mom_days_season_earliest.z <-scale(dat_subset$breeding_exp_Mom_days_season_earliest)
dat_subset$Min_Age_Mom_season_days_earliest.z <-scale(dat_subset$Min_Age_Mom_season_days_earliest)
dat_subset$Min_Age_Dad_season_days_earliest.z <- scale(dat_subset$Min_Age_Dad_season_days_earliest)

dat_subset$Col_Size.z<- scale(dat_subset$Col_Size)


hist(dat_subset$julian_date_first, breaks = seq(0,310,10))


###MODEL ####

mod1 = brms::brm(julian_date_first ~ breeding_exp_pair_days_season_earliest.z*predation+
                   breeding_exp_Mom_days_season_earliest.z*predation+
                   breeding_exp_Dad_days_season_earliest.z*predation+
                   Min_Age_Mom_season_days_earliest.z*predation+
                   Min_Age_Dad_season_days_earliest.z*predation+
                   Col_Size.z+
                   mo(season_of_protection)+
                   (1|Season)  +(breeding_exp_pair_days_season_earliest.z|Pair) +
                   (1|Colony)+ (breeding_exp_Dad_days_season_earliest.z|BreederDad) + 
                   (breeding_exp_Mom_days_season_earliest.z|BreederMom),
                 control = list(adapt_delta = 0.9999, max_treedepth = 15),
                 iter = 10000,
                 cores = 4,
                 #prior = set_prior('normal(0, 3)'),
                 data = dat_subset,
                 family = "poisson",
                 file="breeding_onset")




#check model fitting
pp_check(mod1)
#chains of main population factors mixed well
plot(mod1)
# good fit 

conditional_effects(mod1)





#####PLOT#####

#extract Plotting values
marg<-conditional_effects(mod1,"breeding_exp_pair_days_season_earliest.z:predation", resolution=1000)

#I choose to extract the estimates without random factors 


#select the variable of interest
pair_duration<-marg$breeding_exp_pair_days_season_earliest.z


#main plot
int<-ggplot()+
  geom_line(aes(x=sd(dat_subset$breeding_exp_pair_days_season_earliest)*
                  pair_duration$breeding_exp_pair_days_season_earliest.z[pair_duration$predation=="natural"]+
                  mean(dat_subset$breeding_exp_pair_days_season_earliest),
                y=pair_duration$estimate__[pair_duration$predation=="natural"],color="natural"),
            size=1.3)+
  
  geom_line(aes(x=sd(dat_subset$breeding_exp_pair_days_season_earliest)*
                  pair_duration$breeding_exp_pair_days_season_earliest.z[pair_duration$predation=="protected"]+
                  mean(dat_subset$breeding_exp_pair_days_season_earliest),
                y=pair_duration$estimate__[pair_duration$predation=="protected"], color="protected"),
            size=1.3)+
  
  geom_ribbon(aes(x=sd(dat_subset$breeding_exp_pair_days_season_earliest)*pair_duration$breeding_exp_pair_days_season_earliest.z[pair_duration$predation=="natural"]+
                    mean(dat_subset$breeding_exp_pair_days_season_earliest),
                  y=pair_duration$estimate__[pair_duration$predation=="natural"], 
                  ymax=pair_duration$upper__[pair_duration$predation=="natural"],
                  ymin=pair_duration$lower__[pair_duration$predation=="natural"]),
              fill="#007373",alpha=0.3)+
  
  geom_ribbon(aes(x=sd(dat_subset$breeding_exp_pair_days_season_earliest)*pair_duration$breeding_exp_pair_days_season_earliest.z[pair_duration$predation=="protected"]+
                    mean(dat_subset$breeding_exp_pair_days_season_earliest),
                  y=pair_duration$estimate__[pair_duration$predation=="protected"], 
                  ymax=pair_duration$upper__[pair_duration$predation=="protected"],
                  ymin=pair_duration$lower__[pair_duration$predation=="protected"]),
              fill="#D55E00",alpha=0.3)+
  
  geom_point(aes(x=sd(dat_subset$breeding_exp_pair_days_season_earliest)*dat_subset$breeding_exp_pair_days_season_earliest.z[dat_subset$predation=="natural"]+
                   mean(dat_subset$breeding_exp_pair_days_season_earliest),y=dat_subset$julian_date_first[dat_subset$predation=="natural"],
                 color="natural"),alpha=0.2) +
  
  geom_point(aes(x=(sd(dat_subset$breeding_exp_pair_days_season_earliest)*dat_subset$breeding_exp_pair_days_season_earliest.z[dat_subset$predation=="protected"]+
                      mean(dat_subset$breeding_exp_pair_days_season_earliest))+0.2,y=dat_subset$julian_date_first[dat_subset$predation=="protected"],
                 color="protected"),alpha=0.2) +
  
  labs(x="\nPair duration (days)",
       y="N. of days since season's first laying\n",
       title = "")+
  #scale_size_identity(breaks=seq(1:4), labels=c("1","25","50","75"), guide="legend")+
  # scale_size_continuous(breaks=c(1,25,50,75), labels=c("1","25","50","75"),
  #                       guide=guide_legend(override.aes = list(color="black")))+
  scale_color_manual(name="Predation", values= c("#007373","#D55E00"), 
                     labels=c("natural", "protected"), guide=guide_legend(override.aes = list(fill=c("#D55E00","#007373"))))+
  #scale_fill_discrete(name="N Pairs")+
  #guides(fill="black", color="black")+
  #scale_alpha_manual(guide=guide_legend(override.aes = list(color="black")))+
  # guides(shape=guide_legend(override.aes = list(fill="black")))+
  #ylim(0, 300)+
  # xlim(1,5)+
  #xlab(element_blank())+
  #scale_x_discrete(limits = seq(1:7))+
  #ylab(element_blank())+
  theme(text = element_text(size=20)) +
  theme(legend.title = element_text(size = 13, face="bold"),
        legend.text = element_text(size = 13))+
  theme(legend.position = c(0.9, 0.9),
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
                                "simo_moseason_of_protection1[7]"))+ 
  scale_x_discrete(labels=c("Seasons of protection","Male age:Predation(protected)","Female age:Predation(protected)",
                            "Male experience:Predation(protected)", "Female experience:Predation(protected)",
                            "Pair duration:Predation(protected)",
                            "Colony size","Male age","Female age","Male experience","Female experience",
                            "Predation(protected)","Pair duration"))+
  #,
  #scale_color_sjplot("simply")+ 
  
  ylim(-.6, .5)+ 
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

tab_model(mod1,file="laying_onset_review2.doc", transform=NULL, show.icc = FALSE, show.zeroinf = FALSE, show.re.var=TRUE)


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


random_<-rbind(random_Pair,random_Breeder_Dad,random_BreederMom, random_Colony,random_Season)

random_f<-cbind(random_[,1],round(random_[,2:4],2))
random_f$CrI<-paste0(paste(random_f$l.95..CI, random_f$u.95..CI, sep = " - "))

random_f$l.95..CI <- NULL
random_f$u.95..CI <- NULL

colnames(random_f)[colnames(random_f)=="random_[, 1]"] <- "Variables"

random_f$Variables<- c("Pair ID (700)", "slope Pair duration | Pair ID", "corr. Pair duration - Pair ID",
                       "Male ID (470)", "slope Male experience | Male ID", "corr. Male experience - Male ID",
                       "Female ID (517)", "slope Female experience | Female ID", "corr. Female experience - Female ID",
                       "Colony ID (21)", "Season (10)")

# N Season	10
# N Pair	700
# N Colony	21
# N BreederDad	470
# N BreederMom	517



write.csv(random_f, "random_review2.csv", row.names = F)








####Extract fixed factor estimates for paper####
s<-data.frame(fixef(mod1,summary = F))

#The reference group is natural so you have to add the interaction with protected if you want to extract their credible interval
mean(s$breeding_exp_pair_days_season_earliest.z+s$breeding_exp_pair_days_season_earliest.z.predationprotected)
#-0.4032596
quantile(s$breeding_exp_pair_days_season_earliest.z+s$breeding_exp_pair_days_season_earliest.z.predationprotected,probs = c(0.025, 0.975))
#   2.5%       97.5% 
#   -0.5642612 -0.2458637   


mean(s$breeding_exp_pair_days_season_earliest.z)
# 0.3040157
quantile(s$breeding_exp_pair_days_season_earliest.z,probs = c(0.025, 0.975))
#-0.4601470 -0.1516331 


#female exp prot
mean(s$breeding_exp_Mom_days_season_earliest.z+s$predationprotected.breeding_exp_Mom_days_season_earliest.z)
#-0.1524028
quantile(s$breeding_exp_Mom_days_season_earliest.z+s$predationprotected.breeding_exp_Mom_days_season_earliest.z,probs = c(0.025, 0.975))
#           2.5%       97.5% 
# -0.3257291  0.0223726  

#male exp prot
  mean(s$breeding_exp_Dad_days_season_earliest.z+s$predationprotected.breeding_exp_Dad_days_season_earliest.z)
  #0.1912883
  quantile(s$breeding_exp_Dad_days_season_earliest.z+s$predationprotected.breeding_exp_Dad_days_season_earliest.z,probs = c(0.025, 0.975))
  #  2.5%       97.5% 
  #0.02743603 0.35699373   


  
  
  
#female age prot
mean(s$Min_Age_Mom_season_days_earliest.z+s$predationprotected.Min_Age_Mom_season_days_earliest.z)
  #-0.2245576
quantile(s$Min_Age_Mom_season_days_earliest.z+s$predationprotected.Min_Age_Mom_season_days_earliest.z,probs = c(0.025, 0.975))
  #  2.5%       97.5% 
##-0.35950208 -0.09488777    
  

#male age prot

mean(s$Min_Age_Dad_season_days_earliest.z+s$predationprotected.Min_Age_Dad_season_days_earliest.z)
#-0.2045342
quantile(s$Min_Age_Dad_season_days_earliest.z+s$predationprotected.Min_Age_Dad_season_days_earliest.z,probs = c(0.025, 0.975))
#  2.5%       97.5% 
#-0.33992350 -0.07136737



#interpret the coefficient:
#NATURAL
# > exp(-0.30)
# [1] 0.74
# > 1-0.74
# [1] 0.26

#all other things being equal, if you increase the pair duration of 1 sd (335.8192 gg)
#the julian_date of the first breeding attempt will decrease of 26%

sd(dat_subset$breeding_exp_pair_days_season_earliest[dat_subset$predation=="natural"])
#335.8192

#PROTECTED
# > exp(-0.40)
# [1] 0.67
# > 1-0.67
# [1] 0.33

#all other things being equal, if you increase the pair duration of 1 sd (301.4786 gg)
#the julian_date of the first breeding attempt will decrease of 33%

sd(dat_subset$breeding_exp_pair_days_season_earliest[dat_subset$predation=="protected"])
#301.4786




#calculate what's happens after one year, not used not finished

d<-conditional_effects(mod1, "breeding_exp_pair_days_season_earliest.z",  re_formula=NULL, resolution=1000)

#average pair duration 
mean(dat_subset$breeding_exp_pair_days_season_earliest[dat_subset$predation=="protected"]) #159.2432 days,
#1 year of experience
#I divide by the sd of the experience because:
exp1<-365/sd(dat_subset$breeding_exp_pair_days_season_earliest[dat_subset$predation=="protected"]) #
#find closest number 
avg_duration<-which.min(abs(d$breeding_exp_pair_days_season_earliest.z$breeding_exp_pair_days_season_earliest.z- 0))
year1 <- which.min(abs(d$breeding_exp_pair_days_season_earliest.z$breeding_exp_pair_days_season_earliest.z- exp1))
#output after 1 year
d$breeding_exp_pair_days_season_earliest.z$estimate__[year1] - d$breeding_exp_pair_days_season_earliest.z$estimate__[avg_duration]
# -23.96652

#average julian date first 68.9

#alternative method: backtransform first
original<-sd(dat_subset$breeding_exp_pair_days_season_earliest)*d$breeding_exp_pair_days_season_earliest.z$breeding_exp_pair_days_season_earliest.z+mean(dat_subset$breeding_exp_pair_days_season_earliest)
avg<-mean(dat_subset$breeding_exp_pair_days_season_earliest)
avg_duration<-which.min(abs(original-avg))
year1 <- which.min(abs(original-(avg+365)))











####PLOT MALE AGE  #####

marg<-conditional_effects(mod1,"Min_Age_Dad_season_days_earliest.z:predation", resolution=1000)



#select the variable of interest
pair_duration<-marg$Min_Age_Dad_season_days_earliest.z


int<-ggplot()+
  geom_line(aes(x=sd(dat_subset$Min_Age_Dad_season_days_earliest)*
                  pair_duration$Min_Age_Dad_season_days_earliest.z[pair_duration$predation=="natural"]+
                  mean(dat_subset$Min_Age_Dad_season_days_earliest),
                y=pair_duration$estimate__[pair_duration$predation=="natural"],color="natural"),
            size=1.3)+
  
  geom_line(aes(x=sd(dat_subset$Min_Age_Dad_season_days_earliest)*
                  pair_duration$Min_Age_Dad_season_days_earliest.z[pair_duration$predation=="protected"]+
                  mean(dat_subset$Min_Age_Dad_season_days_earliest),
                y=pair_duration$estimate__[pair_duration$predation=="protected"], color="protected"),
            size=1.3)+
  
  geom_ribbon(aes(x=sd(dat_subset$Min_Age_Dad_season_days_earliest)*pair_duration$Min_Age_Dad_season_days_earliest.z[pair_duration$predation=="natural"]+
                    mean(dat_subset$Min_Age_Dad_season_days_earliest),
                  y=pair_duration$estimate__[pair_duration$predation=="natural"], 
                  ymax=pair_duration$upper__[pair_duration$predation=="natural"],
                  ymin=pair_duration$lower__[pair_duration$predation=="natural"]),
              fill="#007373",alpha=0.3)+
  
  geom_ribbon(aes(x=sd(dat_subset$Min_Age_Dad_season_days_earliest)*pair_duration$Min_Age_Dad_season_days_earliest.z[pair_duration$predation=="protected"]+
                    mean(dat_subset$Min_Age_Dad_season_days_earliest),
                  y=pair_duration$estimate__[pair_duration$predation=="protected"], 
                  ymax=pair_duration$upper__[pair_duration$predation=="protected"],
                  ymin=pair_duration$lower__[pair_duration$predation=="protected"]),
              fill="#D55E00",alpha=0.3)+
  
  geom_point(aes(x=sd(dat_subset$Min_Age_Dad_season_days_earliest)*dat_subset$Min_Age_Dad_season_days_earliest.z[dat_subset$predation=="natural"]+
                   mean(dat_subset$Min_Age_Dad_season_days_earliest),y=dat_subset$julian_date_first[dat_subset$predation=="natural"],
                 color="natural"),alpha=0.2) +
  
  geom_point(aes(x=(sd(dat_subset$Min_Age_Dad_season_days_earliest)*dat_subset$Min_Age_Dad_season_days_earliest.z[dat_subset$predation=="protected"]+
                      mean(dat_subset$Min_Age_Dad_season_days_earliest))+0.2,y=dat_subset$julian_date_first[dat_subset$predation=="protected"],
                 color="protected"),alpha=0.2) +
  
  labs(x="\n Male age (days)",
       y="Days (since season's first laying)\n",
       title = "")+
  #scale_size_identity(breaks=seq(1:4), labels=c("1","25","50","75"), guide="legend")+
  # scale_size_continuous(breaks=c(1,25,50,75), labels=c("1","25","50","75"),
  #                       guide=guide_legend(override.aes = list(color="black")))+
  scale_color_manual(name="Predation", values= c("#007373","#D55E00"), 
                     labels=c("natural", "protected"), guide=guide_legend(override.aes = list(fill=c("#D55E00","#007373"))))+
  #scale_fill_discrete(name="N Pairs")+
  #guides(fill="black", color="black")+
  #scale_alpha_manual(guide=guide_legend(override.aes = list(color="black")))+
  # guides(shape=guide_legend(override.aes = list(fill="black")))+
  #ylim(0, 300)+
  # xlim(1,5)+
  #xlab(element_blank())+
  #scale_x_discrete(limits = seq(1:7))+
  #ylab(element_blank())+
  theme(text = element_text(size=20)) +
  theme(legend.title = element_text(size = 13, face="bold"),
        legend.text = element_text(size = 13))+
  theme(legend.position = c(0.9, 0.9),
        legend.box="horizontal")+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))


#plot(conditional_effects(mod3), points=TRUE)



#### MALE EXPERIENCE #####

marg<-conditional_effects(mod1,"breeding_exp_Dad_days_season_earliest.z:predation", resolution=1000)


#select the variable of interest
pair_duration<-marg$breeding_exp_Dad_days_season_earliest.z


int<-ggplot()+
  geom_line(aes(x=sd(dat_subset$breeding_exp_Dad_days_season_earliest)*
                  pair_duration$breeding_exp_Dad_days_season_earliest.z[pair_duration$predation=="natural"]+
                  mean(dat_subset$breeding_exp_Dad_days_season_earliest),
                y=pair_duration$estimate__[pair_duration$predation=="natural"],color="natural"),
            size=1.3)+
  
  geom_line(aes(x=sd(dat_subset$breeding_exp_Dad_days_season_earliest)*
                  pair_duration$breeding_exp_Dad_days_season_earliest.z[pair_duration$predation=="protected"]+
                  mean(dat_subset$breeding_exp_Dad_days_season_earliest),
                y=pair_duration$estimate__[pair_duration$predation=="protected"], color="protected"),
            size=1.3)+
  
  geom_ribbon(aes(x=sd(dat_subset$breeding_exp_Dad_days_season_earliest)*pair_duration$breeding_exp_Dad_days_season_earliest.z[pair_duration$predation=="natural"]+
                    mean(dat_subset$breeding_exp_Dad_days_season_earliest),
                  y=pair_duration$estimate__[pair_duration$predation=="natural"], 
                  ymax=pair_duration$upper__[pair_duration$predation=="natural"],
                  ymin=pair_duration$lower__[pair_duration$predation=="natural"]),
              fill="#007373",alpha=0.3)+
  
  geom_ribbon(aes(x=sd(dat_subset$breeding_exp_Dad_days_season_earliest)*pair_duration$breeding_exp_Dad_days_season_earliest.z[pair_duration$predation=="protected"]+
                    mean(dat_subset$breeding_exp_Dad_days_season_earliest),
                  y=pair_duration$estimate__[pair_duration$predation=="protected"], 
                  ymax=pair_duration$upper__[pair_duration$predation=="protected"],
                  ymin=pair_duration$lower__[pair_duration$predation=="protected"]),
              fill="#D55E00",alpha=0.3)+
  
  geom_point(aes(x=sd(dat_subset$breeding_exp_Dad_days_season_earliest)*dat_subset$breeding_exp_Dad_days_season_earliest.z[dat_subset$predation=="natural"]+
                   mean(dat_subset$breeding_exp_Dad_days_season_earliest),y=dat_subset$julian_date_first[dat_subset$predation=="natural"],
                 color="natural"),alpha=0.2) +
  
  geom_point(aes(x=(sd(dat_subset$breeding_exp_Dad_days_season_earliest)*dat_subset$breeding_exp_Dad_days_season_earliest.z[dat_subset$predation=="protected"]+
                      mean(dat_subset$breeding_exp_Dad_days_season_earliest))+0.2,y=dat_subset$julian_date_first[dat_subset$predation=="protected"],
                 color="protected"),alpha=0.2) +
  
  labs(x="\n Male experience (days)",
       y="Days (since season's first laying)\n",
       title = "")+
  #scale_size_identity(breaks=seq(1:4), labels=c("1","25","50","75"), guide="legend")+
  # scale_size_continuous(breaks=c(1,25,50,75), labels=c("1","25","50","75"),
  #                       guide=guide_legend(override.aes = list(color="black")))+
  scale_color_manual(name="Predation", values= c("#007373","#D55E00"), 
                     labels=c("natural", "protected"), guide=guide_legend(override.aes = list(fill=c("#D55E00","#007373"))))+
  #scale_fill_discrete(name="N Pairs")+
  #guides(fill="black", color="black")+
  #scale_alpha_manual(guide=guide_legend(override.aes = list(color="black")))+
  # guides(shape=guide_legend(override.aes = list(fill="black")))+
  #ylim(0, 300)+
  # xlim(1,5)+
  #xlab(element_blank())+
  #scale_x_discrete(limits = seq(1:7))+
  #ylab(element_blank())+
  theme(text = element_text(size=20)) +
  theme(legend.title = element_text(size = 13, face="bold"),
        legend.text = element_text(size = 13))+
  theme(legend.position = c(0.9, 0.9),
        legend.box="horizontal")+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))







###plot raw data - "slopes" #######


ggplot(data=dat_subset, aes(x=breeding_exp_pair_days_season_earliest, y=julian_date_first, col=as.factor(Pair)))+
  viridis::scale_color_viridis(discrete = TRUE)+
  geom_point(size=1, alpha=.8)+
  geom_line(alpha=.6)+theme_bw()+
  theme(legend.position = "none")+
  theme(axis.text.x = element_text(size = 15),
        axis.text.y = element_text( size = 15),  
        axis.title.x = element_text( size = 14),
        axis.title.y = element_text( size = 14))+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"))+
  ggtitle("Laying onset")+
  theme(plot.title = element_text(hjust = 0.5,size=18))+
  ylab(bquote("N. of days since season's "* ~ 1^st*" laying"))+
  xlab("Pair duration (days)") 
# ylab(bquote("Days (since season's "1^st~" breeding)"))












### erase pairs with more than 4 seasons together ####
#3 years + 1 day (beginning of the season) = 1096 days
#we erase the extreme values of pair duration because very rare,
#to check that the results are not dependent on them

dat_subset2<-subset(dat_subset, breeding_exp_pair_days_season_earliest<=1096)





#Scale continuous variables


#Scale continuous variables
dat_subset2$breeding_exp_pair_days_season_earliest.z <-scale(dat_subset2$breeding_exp_pair_days_season_earliest)
dat_subset2$breeding_exp_Dad_days_season_earliest.z <-scale(dat_subset2$breeding_exp_Dad_days_season_earliest)
dat_subset2$breeding_exp_Mom_days_season_earliest.z <-scale(dat_subset2$breeding_exp_Mom_days_season_earliest)
dat_subset2$Min_Age_Mom_season_days_earliest.z <-scale(dat_subset2$Min_Age_Mom_season_days_earliest)
dat_subset2$Min_Age_Dad_season_days_earliest.z <- scale(dat_subset2$Min_Age_Dad_season_days_earliest)

dat_subset2$Col_Size.z<- scale(dat_subset2$Col_Size)



mod2 = brms::brm(julian_date_first ~ breeding_exp_pair_days_season_earliest.z*predation+
                   breeding_exp_Mom_days_season_earliest.z*predation+
                   breeding_exp_Dad_days_season_earliest.z*predation+
                   Min_Age_Mom_season_days_earliest.z*predation+
                   Min_Age_Dad_season_days_earliest.z*predation+
                   Col_Size.z+
                   mo(season_of_protection)+
                   (1|Season)  +(breeding_exp_pair_days_season_earliest.z|Pair) +
                   (1|Colony)+ (breeding_exp_Dad_days_season_earliest.z|BreederDad) + 
                   (breeding_exp_Mom_days_season_earliest.z|BreederMom),
                 control = list(adapt_delta = 0.9999, max_treedepth = 15),
                 iter = 10000,
                 cores = 4,
                 #prior = set_prior('normal(0, 3)'),
                 data = dat_subset2,
                 family = "poisson",
                 file="breeding_onset_trim_rev2")




s<-data.frame(fixef(mod2,summary = F))

#The reference group is natural so you have to add the interaction with protected if you want to extract their credible interval
mean(s$breeding_exp_pair_days_season_earliest.z+s$breeding_exp_pair_days_season_earliest.z.predationprotected)
#-0.450798
quantile(s$breeding_exp_pair_days_season_earliest.z+s$breeding_exp_pair_days_season_earliest.z.predationprotected,probs = c(0.025, 0.975))
#   2.5%       97.5% 
#   -0.5828212 -0.3209023   


mean(s$breeding_exp_pair_days_season_earliest.z)
# -0.3363728
quantile(s$breeding_exp_pair_days_season_earliest.z,probs = c(0.025, 0.975))
#-0.4618157 -0.2126797 



#results are nearly identical to the full dataset


