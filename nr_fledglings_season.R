#Aim:  study if the number of chicks in a season is influenced by pair duration, experience or age
#(while accounting for other variables)
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


#select only colonies with the same protection state throughout the year
dat<-subset(dat, double_protection_state=="no")


#for colonies in the natural state, assign 0 at the length of protection
dat$season_of_protection[is.na(dat$season_of_protection)] <- 0


#select the variables of interest
dat_subset<-dat[,c( "tot_fitness", "breeding_exp_pair_nogaps",
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

mod1 = brms::brm(tot_fitness ~ breeding_exp_pair_nogaps.z*predation +
                   breeding_exp_Mom_nogaps.z*predation+
                   breeding_exp_Dad_nogaps.z*predation+
                   Mom_min_Seasons.z*predation+
                   Dad_min_Seasons.z*predation+
                   Col_Size.z+
                   mo(season_of_protection)+
                   (1|Season) + (1|Colony)+
                   (breeding_exp_pair_nogaps.z|Pair)+
                   (breeding_exp_Dad_nogaps.z|BreederDad) + 
                   (breeding_exp_Mom_nogaps.z|BreederMom),
                 control = list(adapt_delta = 0.999, max_treedepth = 12),
                 iter = 8000,
                 cores = 4,
                 #prior = set_prior('normal(0, 3)'),
                 data = dat_subset,
                 family = "zero_inflated_poisson",
                 file="YOUR_NAME")


#check model fitting
pp_check(mod1)
plot(mod1)
summary(mod1)



#####PLOT#####

#extract Plotting values
marg<-conditional_effects(mod1,"breeding_exp_pair_nogaps.z:predation", resolution=1000)

#I choose to extract the estimates without random factors 

#select the variable of interest
pair_duration<-marg$breeding_exp_pair_nogaps.z

#count the number of pairs with a certain duration and success
dat_subset <- within(dat_subset, Count <- ave(tot_fitness, list(tot_fitness, breeding_exp_pair_nogaps,predation), FUN=length))  


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
                   mean(dat_subset$breeding_exp_pair_nogaps),y=dat_subset$tot_fitness[dat_subset$predation=="natural"],
                  color="natural")) +

  geom_point(aes(size = dat_subset$Count[dat_subset$predation=="protected"],
                 x=(sd(dat_subset$breeding_exp_pair_nogaps)*dat_subset$breeding_exp_pair_nogaps.z[dat_subset$predation=="protected"]+
                   mean(dat_subset$breeding_exp_pair_nogaps))+0.2,y=dat_subset$tot_fitness[dat_subset$predation=="protected"],
                 color="protected")) +

    labs(x="\nPair duration (years)",
       y="Fledglings (pair/season)\n",
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
  ylim(0, 18)+
  # xlim(1,5)+
  #xlab(element_blank())+
  scale_x_discrete(limits = seq(1:7))+
  #ylab(element_blank())+
  theme(text = element_text(size=20)) +
  theme(legend.title = element_text(size = 13, face="bold"),
        legend.text = element_text(size = 13))+
  theme(legend.position = c(0.25, 0.9),
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
                            "Male experience:Predation(protected)","Female experience:Predation(protected)","Pair duration:Predation(protected)",
                            "Colony size","Male age","Female age","Male experience","Female experience","Predation(protected)","Pair duration"))+
  #scale_color_sjplot("simply")+ 
  ylim(-.3, 1.35)+ 
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

tab_model(mod1,file="tot_fitness_review2.doc", transform=NULL, show.icc = FALSE, show.zeroinf = FALSE, show.re.var=TRUE)


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
# N Colony	17
# N Pair	616
# N BreederDad	412
# N BreederMom	458



write.csv(random_f, "random_review2.csv", row.names = F)








####Extract fixed factor estimates for paper####
s<-data.frame(fixef(mod1,summary = F))

#The reference group is natural so you have to add the interaction with protected if you want to extract their credible interval
mean(s$breeding_exp_pair_nogaps.z+s$breeding_exp_pair_nogaps.z.predationprotected)
#0.1260782
quantile(s$breeding_exp_pair_nogaps.z+s$breeding_exp_pair_nogaps.z.predationprotected,probs = c(0.025, 0.975))
#   2.5%      97.5% 
#   0.0339110  0.2186917  


mean(s$breeding_exp_pair_nogaps.z)
#0.0933034
quantile(s$breeding_exp_pair_nogaps.z,probs = c(0.025, 0.975))
#    2.5%       97.5% 
#   0.001935203 0.195107436 
length(which(s$breeding_exp_pair_nogaps.z < 0))/16000
# 2.5%       97.5% 
#rev3   -0.002282235  0.187827201 


#female age
mean(s$Mom_min_Seasons.z)
#-0.1109337
quantile(s$Mom_min_Seasons.z,probs = c(0.025, 0.975))
# 2.5%             97.5% 
#   -0.24446249    0.02011215  


length(which(s$Mom_min_Seasons.z > 0))/16000

s$Intercept 




###calculations effects  for the paper #####
d<-conditional_effects(mod1, "breeding_exp_pair_nogaps.z:predation", re_formula=NULL, resolution=1000)

#average pair duration prot.
mean(dat_subset$breeding_exp_pair_nogaps[dat_subset$predation=="protected"]) 
#1.493506
#average pair duration nat
mean(dat_subset$breeding_exp_pair_nogaps[dat_subset$predation=="natural"]) 
#1.6609



#variables with "_p" refer to protected colonies
#variables with "_n" refer to natural colonies

#1 year of experience
exp1_p<-1/sd(dat_subset$breeding_exp_pair_nogaps[dat_subset$predation=="protected"])
#1 year of experience
exp1_n<-1/sd(dat_subset$breeding_exp_pair_nogaps[dat_subset$predation=="natural"])


d_p<-d$breeding_exp_pair_nogaps.z[d$breeding_exp_pair_nogaps.z$predation=="protected",]


#find closest number 
avg_duration<-which.min(abs(d_p$breeding_exp_pair_nogaps.z- 0))

year1 <- which.min(abs(d_p$breeding_exp_pair_nogaps.z- exp1_p))
#output after 1 year
d_p$estimate__[year1] - d_p$estimate__[avg_duration]
# 0.5735398


d_n<-d$breeding_exp_pair_nogaps.z[d$breeding_exp_pair_nogaps.z$predation=="natural",]


#find closest number 
avg_duration<-which.min(abs(d_n$breeding_exp_pair_nogaps.z- 0))

year1 <- which.min(abs(d_n$breeding_exp_pair_nogaps.z- exp1_n))
#output after 1 year
d_n$estimate__[year1] - d_n$estimate__[avg_duration]
# 0.1794843




#4 year of experience
exp1_p<-4/sd(dat_subset$breeding_exp_pair_nogaps[dat_subset$predation=="protected"])
#1 year of experience
exp1_n<-4/sd(dat_subset$breeding_exp_pair_nogaps[dat_subset$predation=="natural"])


d_p<-d$breeding_exp_pair_nogaps.z[d$breeding_exp_pair_nogaps.z$predation=="protected",]


#find closest number 
avg_duration<-which.min(abs(d_p$breeding_exp_pair_nogaps.z- 0))

year4 <- which.min(abs(d_p$breeding_exp_pair_nogaps.z- exp1_p))
#output after 4 year
d_p$estimate__[year4] - d_p$estimate__[avg_duration]
# 2.934781


d_n<-d$breeding_exp_pair_nogaps.z[d$breeding_exp_pair_nogaps.z$predation=="natural",]


#find closest number 
avg_duration<-which.min(abs(d_n$breeding_exp_pair_nogaps.z- 0))

year4 <- which.min(abs(d_n$breeding_exp_pair_nogaps.z- exp1_n))
#output after 4 year (from the average) 
d_n$estimate__[year4] - d_n$estimate__[avg_duration]
#  0.8024954



#extract the values directly from the estimates (results sould be the same)
pair_duration2<-d$breeding_exp_pair_nogaps.z
pair_duration2$pair_d<-round(sd(dat_subset$breeding_exp_pair_nogaps)*
                               pair_duration2$breeding_exp_pair_nogaps.z+
       mean(dat_subset$breeding_exp_pair_nogaps),2)



pair_duration2$estimate__[which(pair_duration2$pair_d==1 & pair_duration2$predation=="natural")]
#1.657266  

pair_duration2$estimate__[which(pair_duration2$pair_d==4& pair_duration2$predation=="natural")]
#2.243713



pair_duration2$estimate__[which(pair_duration2$pair_d==1 & pair_duration2$predation=="protected")]
#3.219382

pair_duration2$estimate__[which(pair_duration2$pair_d==4& pair_duration2$predation=="protected")]
#4.882943 


dat1<-dat[,c( "tot_fitness","Pair","breeding_exp_pair_nogaps",  "predation")]
dat1<-unique(dat1)
dat1<-na.omit(dat1)

mean(dat1$tot_fitness[dat1$breeding_exp_pair_nogaps==1&dat1$predation=="natural"])
#1.746702
mean(dat1$tot_fitness[dat1$breeding_exp_pair_nogaps==1&dat1$predation=="protected"])
#2.665517






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



mod2 = brms::brm(tot_fitness ~ breeding_exp_pair_nogaps.z*predation +
                   breeding_exp_Mom_nogaps.z*predation+
                   breeding_exp_Dad_nogaps.z*predation+
                   Mom_min_Seasons.z*predation+
                   Dad_min_Seasons.z*predation+
                   Col_Size.z+
                   mo(season_of_protection)+
                   (1|Season) + (1|Colony)+
                   (breeding_exp_pair_nogaps.z|Pair)+
                   (breeding_exp_Dad_nogaps.z|BreederDad) + 
                   (breeding_exp_Mom_nogaps.z|BreederMom),
                 control = list(adapt_delta = 0.999, max_treedepth = 12),
                 iter = 8000,
                 cores = 4,
                 #prior = set_prior('normal(0, 3)'),
                 data = dat_subset2,
                 family = "zero_inflated_poisson",
                 file="YOUR_MODEL_trim")





s<-data.frame(fixef(mod2,summary = F))

#The reference group is natural so you have to add the interaction with protected if you want to extract their credible interval
mean(s$breeding_exp_pair_nogaps.z+s$breeding_exp_pair_nogaps.z.predationprotected)
#0.1334063
quantile(s$breeding_exp_pair_nogaps.z+s$breeding_exp_pair_nogaps.z.predationprotected,probs = c(0.025, 0.975))
#   2.5%      97.5% 
#   0.04454021 0.22035618  


mean(s$breeding_exp_pair_nogaps.z)
#0.09055867
quantile(s$breeding_exp_pair_nogaps.z,probs = c(0.025, 0.975))
#    2.5%       97.5% 
#   -0.0003642275  0.1830595979 


#results are nearly identical



