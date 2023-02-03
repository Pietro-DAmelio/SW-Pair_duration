#Aim: Potential reasons for increased breeding success in case of long term monogamy: 
#do the number of helpers increase in more experienced pairs?

# One value per pair per breeding attempt
# Data available only for less seasons 


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
dat_subset<-dat[,c( "Group_Size","breeding_exp_pair_days","season_of_protection",
                    "Pair", "Season", "predation", "breeding_exp_Mom_days", "breeding_exp_Dad_days",
                    "Col_Size", "Mom_MinAge", "Dad_MinAge", "Colony", "Nest", "ColNestLaying",
                    "BreederMom", "BreederDad")] 



#delete duplicates
dat_subset<- unique(dat_subset)

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





###MODEL ####

mod1 = brms::brm(Group_Size ~ breeding_exp_pair_days.z*predation+
                   breeding_exp_Mom_days.z*predation+
                   breeding_exp_Dad_days.z*predation+
                   Mom_MinAge.z*predation +
                   Dad_MinAge.z*predation + 
                   Col_Size.z + 
                   mo(season_of_protection)+
                   (1|Colony)+ (1|Season)+ (1|nest_season) +
                   (breeding_exp_pair_days.z|Pair) +
                   (breeding_exp_Dad_days.z|BreederDad) + 
                   (breeding_exp_Mom_days.z|BreederMom),
                 control = list(adapt_delta = 0.99, max_treedepth = 12),
                 iter = 8000,
                 cores = 4,
                 #prior = set_prior('normal(0, 3)'),
                 data = dat_subset,
                 family="skew_normal",
                 file="Nr_helpers")



#check model fitting
pp_check(mod1)
plot(mod1)
summary(mod1)

#quite ok, given the very peculiar variable distribution




#####PLOT#####


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
                   mean(dat_subset$breeding_exp_pair_days),y=dat_subset$Group_Size[dat_subset$predation=="natural"],
                 color="natural"),alpha=0.2) +
  
  geom_point(aes(x=(sd(dat_subset$breeding_exp_pair_days)*dat_subset$breeding_exp_pair_days.z[dat_subset$predation=="protected"]+
                      mean(dat_subset$breeding_exp_pair_days)),y=dat_subset$Group_Size[dat_subset$predation=="protected"],
                 color="protected"),alpha=0.2) +
  
  labs(x="\nPair duration (days)",
       y="Group size (pair/season)\n",
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
                            "Male experience:Predation(protected)","Female experience:Predation(protected)",
                            "Pair duration:Predation(protected)",
                            "Colony size","Male age","Female age","Male experience","Female experience",
                            "Predation(protected)","Pair duration"))+
  #
  #scale_color_sjplot("simply")+ 
  ylim(-.55, .55)+ 
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

tab_model(mod1,file="group_size_review2.doc", transform=NULL, show.icc = FALSE, show.zeroinf = FALSE, show.re.var=TRUE)


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

random_f$Variables<- c("Pair ID (510)", "slope Pair duration | Pair ID", "corr. Pair duration - Pair ID",
                       "Male ID (373)", "slope Male experience | Male ID", "corr. Male experience - Male ID",
                       "Female ID (398)", "slope Female experience | Female ID", "corr. Female experience - Female ID",
                       "Colony ID (15)", "Season (8)", "Nest (417)")

# N Colony	15
# N Season	8
# N nest_season	417
# N Pair	510
# N BreederDad	373
# N BreederMom	398




write.csv(random_f, "random_review2.csv", row.names = F)




####Extract fixed factor estimates for paper####
s<-data.frame(fixef(mod1,summary = F))

#The reference group is natural so you have to add the interaction with protected if you want to extract their credible interval
mean(s$breeding_exp_pair_days.z+s$breeding_exp_pair_days.z.predationprotected)
#0.1244185
quantile(s$breeding_exp_pair_days.z+s$breeding_exp_pair_days.z.predationprotected,probs = c(0.025, 0.975))
#   2.5%        97.5% 
#-0.0381108   0.2930046 

length(which(s$breeding_exp_pair_days.z+s$breeding_exp_pair_days.z.predationprotected < 0))/16000


mean(s$breeding_exp_pair_days.z)
# 0.2146342
quantile(s$breeding_exp_pair_days.z,probs = c(0.025, 0.975))
#  2.5%       97.5% 
#0.07291969 0.35396696


#exp females nat
mean(s$breeding_exp_Mom_days.z)
# 0.2196481
quantile(s$breeding_exp_Mom_days.z,probs = c(0.025, 0.975))
# 2.5%       97.5% 
#   0.04079082 0.40290088

#female age nat
mean(s$Mom_MinAge.z)
# -0.2046117
quantile(s$Mom_MinAge.z,probs = c(0.025, 0.975))
# 2.5%         97.5% 
# -0.37981846 -0.03755061


#male age nat
mean(s$Dad_MinAge.z)
# 0.1750157
quantile(s$Dad_MinAge.z,probs = c(0.025, 0.975))
# 2.5%         97.5% 
# 0.03000476 0.32010749








###calculations effects  for the paper #####
d<-conditional_effects(mod1, "breeding_exp_pair_days.z:predation", re_formula=NULL, resolution=1000)

#average pair duration prot.
mean(dat_subset$breeding_exp_pair_days[dat_subset$predation=="protected"]) 
#257.8676
#average pair duration nat
mean(dat_subset$breeding_exp_pair_days[dat_subset$predation=="natural"]) 
#329.5718



#average pair duration 
mean(dat_subset$breeding_exp_pair_days) #299.9115 days,
#1 year of experience
#I divide by the sd of the experience because:
exp1<-365/sd(dat_subset$breeding_exp_pair_days) #
#find closest number 
avg_duration<-which.min(abs(d$breeding_exp_pair_days.z$breeding_exp_pair_days.z- 0))
year1 <- which.min(abs(d$breeding_exp_pair_days.z$breeding_exp_pair_days.z- exp1))
#output after 1 year
d$breeding_exp_pair_days.z$estimate__[year1] - d$breeding_exp_pair_days.z$estimate__[avg_duration]
#0.2180725 skew normal





#variables with "_p" refer to protected colonies
#variables with "_n" refer to natural colonies

#1 year of experience
exp1_p<-365/sd(dat_subset$breeding_exp_pair_days[dat_subset$predation=="protected"])
#1 year of experience
exp1_n<-365/sd(dat_subset$breeding_exp_pair_days[dat_subset$predation=="natural"])


d_p<-d$breeding_exp_pair_days.z[d$breeding_exp_pair_days.z$predation=="protected",]


#find closest number 
avg_duration<-which.min(abs(d_p$breeding_exp_pair_days.z- 0))

year1 <- which.min(abs(d_p$breeding_exp_pair_days.z- exp1_p))
#output after 1 year
d_p$estimate__[year1] - d_p$estimate__[avg_duration]
# 0.1408671


d_n<-d$breeding_exp_pair_days.z[d$breeding_exp_pair_days.z$predation=="natural",]


#find closest number 
avg_duration<-which.min(abs(d_n$breeding_exp_pair_days.z- 0))

year1 <- which.min(abs(d_n$breeding_exp_pair_days.z- exp1_n))
#output after 1 year
d_n$estimate__[year1] - d_n$estimate__[avg_duration]
# 0.2062967











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



mod2 = brms::brm(Group_Size ~ breeding_exp_pair_days.z*predation+
                   breeding_exp_Mom_days.z*predation+
                   breeding_exp_Dad_days.z*predation+
                   Mom_MinAge.z*predation +
                   Dad_MinAge.z*predation + 
                   Col_Size.z + 
                   mo(season_of_protection)+
                   (1|Colony)+ (1|Season)+ (1|nest_season) +
                   (breeding_exp_pair_days.z|Pair) +
                   (breeding_exp_Dad_days.z|BreederDad) + 
                   (breeding_exp_Mom_days.z|BreederMom),
                 control = list(adapt_delta = 0.99, max_treedepth = 12),
                 iter = 8000,
                 cores = 4,
                 #prior = set_prior('normal(0, 3)'),
                 data = dat_subset2,
                 family="skew_normal",
                 file="Nr_helpers_trim_rev2")



####Extract fixed factor estimates for paper####
s<-data.frame(fixef(mod2,summary = F))

#The reference group is natural so you have to add the interaction with protected if you want to extract their credible interval
mean(s$breeding_exp_pair_days.z+s$breeding_exp_pair_days.z.predationprotected)
#0.1443208
quantile(s$breeding_exp_pair_days.z+s$breeding_exp_pair_days.z.predationprotected,probs = c(0.025, 0.975))
#   2.5%        97.5% 
#-0.02077512  0.31073728  

length(which(s$breeding_exp_pair_days.z+s$breeding_exp_pair_days.z.predationprotected < 0))/16000


mean(s$breeding_exp_pair_days.z)
# 0.2017785
quantile(s$breeding_exp_pair_days.z,probs = c(0.025, 0.975))
#  2.5%       97.5% 
#0.06403093 0.33909313






#results are nearly identical to the full dataset






