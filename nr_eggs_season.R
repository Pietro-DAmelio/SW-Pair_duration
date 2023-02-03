#Aim:  study if the number of eggs in a season is influenced by pair duration, experience or age
#(while accounting for other variables)
#Author: P. D'Amelio

### libraries  ####
library(brms)
library(loo)
library(sjPlot)
library(ggplot2)
library(ggpubr)



#load database
dat<- read.csv("YOUR_DIRECTORY/Longitudinal_dataset.csv")


#select only colonies with the same protection state throughout the year
dat<-subset(dat, double_protection_state=="no")


#for colonies in the natural state, assign 0 at the length of protection
dat$season_of_protection[is.na(dat$season_of_protection)] <- 0

#select the variables of interest
dat_subset<-dat[,c( "N_eggs_season", "breeding_exp_pair_nogaps",
                    "Pair", "Season", "predation", "breeding_exp_Mom_nogaps", "breeding_exp_Dad_nogaps",
                    "Col_Size", "Mom_min_Seasons", "Dad_min_Seasons", "Colony",
                    "BreederMom", "BreederDad", "season_of_protection")] 

#delete duplicates
dat_subset<- unique(dat_subset)

#select only complete cases
dat_subset<-dat_subset[complete.cases(dat_subset), ] 


#delete seasons not fully monitored
dat_subset<-subset(dat_subset, Season!="2008/2009") 
dat_subset<-subset(dat_subset, Season!="2009/2010") 
dat_subset<-subset(dat_subset, Season!="2010/2011")

# variables that are factors
dat_subset$Colony<- as.factor(dat_subset$Colony)


#Scale continuous variables
dat_subset$breeding_exp_pair_nogaps.z <-scale(dat_subset$breeding_exp_pair_nogaps)
dat_subset$breeding_exp_Dad_nogaps.z <-scale(dat_subset$breeding_exp_Dad_nogaps)
dat_subset$breeding_exp_Mom_nogaps.z <-scale(dat_subset$breeding_exp_Mom_nogaps)
dat_subset$Mom_min_Seasons.z <-scale(dat_subset$Mom_min_Seasons)
dat_subset$Dad_min_Seasons.z <- scale(dat_subset$Dad_min_Seasons)

dat_subset$Col_Size.z<- scale(dat_subset$Col_Size)




###MODEL ####

mod1 = brms::brm(N_eggs_season ~  breeding_exp_pair_nogaps.z*predation +
                   breeding_exp_Mom_nogaps.z*predation+
                   breeding_exp_Dad_nogaps.z*predation+
                   Mom_min_Seasons.z*predation+
                   Dad_min_Seasons.z*predation+
                   Col_Size.z+
                   mo(season_of_protection)+
                   (1|Season)  +(breeding_exp_pair_nogaps.z|Pair) +
                   (1|Colony)+ (breeding_exp_Dad_nogaps.z|BreederDad) + 
                   (breeding_exp_Mom_nogaps.z|BreederMom),
                 control = list(adapt_delta = 0.999, max_treedepth = 12),
                 iter = 8000,
                 cores = 4,
                 #prior = set_prior('normal(0, 3)'),
                 data = dat_subset,
                 family = "skew_normal",
                 file="n_eggs_season")


#check model fitting
pp_check(mod1)
plot(mod1)
summary(mod1)




#####PLOT#####

marg<-conditional_effects(mod1,"breeding_exp_pair_nogaps.z:predation", resolution=1000)
#I choose to extract the estimates without random factors 


#select the variable of interest
pair_duration<-marg$breeding_exp_pair_nogaps.z

#count the number of pairs with a certain duration and success
dat_subset <- within(dat_subset, Count <- ave(N_eggs_season, list(N_eggs_season, breeding_exp_pair_nogaps,predation), FUN=length))  


#main plot
int<-ggplot()+
  geom_line(aes(x=sd(dat_subset$breeding_exp_pair_nogaps)*
                  pair_duration$breeding_exp_pair_nogaps.z[pair_duration$predation=="natural"]+
                  mean(dat_subset$breeding_exp_pair_nogaps),
                y=pair_duration$estimate__[pair_duration$predation=="natural"],color="natural"),
            size=1.3)+
  
  geom_line(aes(x=sd(dat_subset$breeding_exp_pair_nogaps)*
                  pair_duration$breeding_exp_pair_nogaps.z[pair_duration$predation=="protected"]+
                  mean(dat_subset$breeding_exp_pair_nogaps),
                y=pair_duration$estimate__[pair_duration$predation=="protected"], color="protected"),
            size=1.3)+
  
  geom_ribbon(aes(x=sd(dat_subset$breeding_exp_pair_nogaps)*pair_duration$breeding_exp_pair_nogaps.z[pair_duration$predation=="natural"]+
                    mean(dat_subset$breeding_exp_pair_nogaps),
                  y=pair_duration$estimate__[pair_duration$predation=="natural"], 
                  ymax=pair_duration$upper__[pair_duration$predation=="natural"],
                  ymin=pair_duration$lower__[pair_duration$predation=="natural"]),
              fill="#007373",alpha=0.3)+
  
  geom_ribbon(aes(x=sd(dat_subset$breeding_exp_pair_nogaps)*pair_duration$breeding_exp_pair_nogaps.z[pair_duration$predation=="protected"]+
                    mean(dat_subset$breeding_exp_pair_nogaps),
                  y=pair_duration$estimate__[pair_duration$predation=="protected"], 
                  ymax=pair_duration$upper__[pair_duration$predation=="protected"],
                  ymin=pair_duration$lower__[pair_duration$predation=="protected"]),
              fill="#D55E00",alpha=0.3)+
  
  geom_point(aes(size = dat_subset$Count[dat_subset$predation=="natural"],
                 x=sd(dat_subset$breeding_exp_pair_nogaps)*dat_subset$breeding_exp_pair_nogaps.z[dat_subset$predation=="natural"]+
                   mean(dat_subset$breeding_exp_pair_nogaps),y=dat_subset$N_eggs_season[dat_subset$predation=="natural"],
                 color="natural")) +
  
  geom_point(aes(size = dat_subset$Count[dat_subset$predation=="protected"],
                 x=(sd(dat_subset$breeding_exp_pair_nogaps)*dat_subset$breeding_exp_pair_nogaps.z[dat_subset$predation=="protected"]+
                      mean(dat_subset$breeding_exp_pair_nogaps))+0.2,y=dat_subset$N_eggs_season[dat_subset$predation=="protected"],
                 color="protected")) +
  
  labs(x="\nPair duration (years)",
       y="Eggs (n/pair/season)\n",
       title = "",
       size="N Pairs")+
  #scale_size_identity(breaks=seq(1:4), labels=c("1","25","50","75"), guide="legend")+
  scale_size_continuous(breaks=c(1,25,50,75), labels=c("1","25","50","75"),
                        guide=guide_legend(override.aes = list(color="black")))+
  scale_color_manual(name="Predation", values= c("#007373","#D55E00"), 
                     labels=c("natural", "protected"), guide=guide_legend(override.aes = list(fill=c("#D55E00","#007373"))))+
  #scale_fill_discrete(name="N Pairs")+
  #guides(fill="black", color="black")+
  #scale_alpha_manual(guide=guide_legend(override.aes = list(color="black")))+
  # guides(shape=guide_legend(override.aes = list(fill="black")))+
  ylim(0, 41)+
  # xlim(1,5)+
  #xlab(element_blank())+
  scale_x_discrete(limits = seq(1:7))+
  #ylab(element_blank())+
  theme(text = element_text(size=20)) +
  theme(legend.title = element_text(size = 13, face="bold"),
        legend.text = element_text(size = 13))+
  theme(legend.position = c(0.75, 0.9),
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
                            "Male experience:Predation(protected)","Female experience:Predation(protected)",
                            "Pair duration:Predation(protected)",
                            "Colony size","Male age","Female age",
                            "Male experience","Female experience","Predation(protected)","Pair duration"))+
  #scale_color_sjplot("simply")+ 
  ylim(-2.2, 1.35)+ 
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

tab_model(mod1,file="n_eggs_revision2.doc", transform=NULL, show.icc = FALSE, show.zeroinf = FALSE, show.re.var=TRUE)


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

random_f$Variables<- c("Pair ID (616)", "slope Pair duration | Pair ID", "corr. Pair duration - Pair ID",
                       "Male ID (412)", "slope Male experience | Male ID", "corr. Male experience - Male ID",
                       "Female ID (458)", "slope Female experience | Female ID", "corr. Female experience - Female ID",
                       "Colony ID (17)", "Season (8)")

# N Season	8
# N Pair	616
# N Colony	17
# N BreederDad	412
# N BreederMom	458

write.csv(random_f, "random_revision2.csv", row.names = F)



####Extract fixed factor estimates for paper####
s<-data.frame(fixef(mod1,summary = F))

#The reference group is natural so you have to add the interaction with protected if you want to extract their credible interval
mean(s$breeding_exp_pair_nogaps.z+s$breeding_exp_pair_nogaps.z.predationprotected)
# 0.2457501
quantile(s$breeding_exp_pair_nogaps.z+s$breeding_exp_pair_nogaps.z.predationprotected,probs = c(0.025, 0.975))
#   2.5%      97.5% 
#   -0.1630804  0.7016925  


mean(s$breeding_exp_pair_nogaps.z)
# 0.4692888
quantile(s$breeding_exp_pair_nogaps.z,probs = c(0.025, 0.975))
#    2.5%     97.5% 
#   0.1500818 0.8335751



#age females

mean(s$Mom_min_Seasons.z)
# -0.1609302
quantile(s$Mom_min_Seasons.z,probs = c(0.025, 0.975))
# 2.5%      97.5% 
#   -0.5834797  0.2108195 


mean(s$Mom_min_Seasons.z+s$predationprotected.Mom_min_Seasons.z)
# -0.1652612
quantile(s$Mom_min_Seasons.z+s$predationprotected.Mom_min_Seasons.z,probs = c(0.025, 0.975))
  #  2.5%      97.5% 
  # -0.5422701  0.1640575 








### erase pairs with more than 4 seasons together ####
#we erase the extreme values of pair duration because very rare,
#to check that the results are not dependent on them


dat_subset2<-subset(dat_subset, breeding_exp_pair_nogaps<=4)





#Scale continuous variables
dat_subset2$breeding_exp_pair_nogaps.z <-scale(dat_subset2$breeding_exp_pair_nogaps)
dat_subset2$breeding_exp_Dad_nogaps.z <-scale(dat_subset2$breeding_exp_Dad_nogaps)
dat_subset2$breeding_exp_Mom_nogaps.z <-scale(dat_subset2$breeding_exp_Mom_nogaps)
dat_subset2$Mom_min_Seasons.z <-scale(dat_subset2$Mom_min_Seasons)
dat_subset2$Dad_min_Seasons.z <- scale(dat_subset2$Dad_min_Seasons)

dat_subset2$Col_Size.z<- scale(dat_subset2$Col_Size)


mod2 = brms::brm(N_eggs_season ~  breeding_exp_pair_nogaps.z*predation +
                   breeding_exp_Mom_nogaps.z*predation+
                   breeding_exp_Dad_nogaps.z*predation+
                   Mom_min_Seasons.z*predation+
                   Dad_min_Seasons.z*predation+
                   Col_Size.z+
                   mo(season_of_protection)+
                   (1|Season)  +(breeding_exp_pair_nogaps.z|Pair) +
                   (1|Colony)+ (breeding_exp_Dad_nogaps.z|BreederDad) + 
                   (breeding_exp_Mom_nogaps.z|BreederMom),
                 control = list(adapt_delta = 0.999, max_treedepth = 12),
                 iter = 8000,
                 cores = 2,
                 #prior = set_prior('normal(0, 3)'),
                 data = dat_subset2,
                 family = "skew_normal",
                 file="n_eggs_season_trim_rev2")



####Extract fixed factor estimates for paper####
s<-data.frame(fixef(mod2,summary = F))

#The reference group is natural so you have to add the interaction with protected if you want to extract their credible interval
mean(s$breeding_exp_pair_nogaps.z+s$breeding_exp_pair_nogaps.z.predationprotected)
# 0.2298775
quantile(s$breeding_exp_pair_nogaps.z+s$breeding_exp_pair_nogaps.z.predationprotected,probs = c(0.025, 0.975))
#   2.5%      97.5% 
#-0.1732008  0.6467577 


mean(s$breeding_exp_pair_nogaps.z)
# 0.5845613
quantile(s$breeding_exp_pair_nogaps.z,probs = c(0.025, 0.975))
#    2.5%     97.5% 
#   0.2543657 0.9536074 




