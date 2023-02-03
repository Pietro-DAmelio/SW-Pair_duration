#Aim: Describe the Seasonality of success and whether it is depending on predation
#Author: P. D'Amelio


### libraries  ###

library(ggplot2)
library(brms)
library(loo)

#load database
dat<- read.csv("YOUR_DIRECTORY/Longitudinal_dataset.csv")




####Plot laying date vs success ####

dat$Laying_date_dd_mm<- format(as.Date(dat$Laying_date, format="%Y-%m-%d"),"%m-%d")

dat$Laying_date_dd_mm <-as.POSIXct(strptime(dat$Laying_date_dd_mm, format="%m-%d"))
#dat$Laying_date_dd_mm<-as.factor(format(dat$Laying_date_dd_mm, format="%m-%d"))


#run natural and protected models separately
dat_Succ<-dat[,c("Laying_date_dd_mm","succ", "Pair", "Season", "predation", "Colony", "Col_Size",
                 "ColNestLaying", "unique_ID")]
#natural
dat_Succ_nat<- subset(dat_Succ, predation=="natural")
dat_Succ_nat<-unique(dat_Succ_nat)
dat_Succ_nat<-dat_Succ_nat[complete.cases(dat_Succ_nat), ]
dat_Succ_nat$Col_Size.z<- scale(dat_Succ_nat$Col_Size)
dat_Succ_nat$laying_julian<-as.numeric(format(dat_Succ_nat$Laying_date_dd_mm, "%j"))
dat_Succ_nat$Colony<- as.factor(dat_Succ_nat$Colony)

dat_Succ_nat$laying_julian_shift<- ifelse(dat_Succ_nat$laying_julian>244, dat_Succ_nat$laying_julian-243, dat_Succ_nat$laying_julian+122)
dat_Succ_nat$Laying_date_dd_mm_shift<-as.Date(dat_Succ_nat$laying_julian_shift, origin = "2018-08-31")



#protected
dat_Succ_prot<- subset(dat_Succ, predation=="protected")
dat_Succ_prot<-unique(dat_Succ_prot)
dat_Succ_prot<-dat_Succ_prot[complete.cases(dat_Succ_prot), ]
dat_Succ_prot$Col_Size.z<- scale(dat_Succ_prot$Col_Size)
dat_Succ_prot$laying_julian<-as.numeric(format(dat_Succ_prot$Laying_date_dd_mm, "%j"))
dat_Succ_prot$Colony<- as.factor(dat_Succ_prot$Colony)




dat_Succ_prot$laying_julian_shift<- ifelse(dat_Succ_prot$laying_julian>244, dat_Succ_prot$laying_julian-243, dat_Succ_prot$laying_julian+122)
dat_Succ_prot$Laying_date_dd_mm_shift<-as.Date(dat_Succ_prot$laying_julian_shift, origin = "2018-08-31")


#run the model with September as first month
mod_s = brms::brm(succ ~ s(laying_julian_shift)+Col_Size.z+
                  (1|Pair)+(1|Season)+(1|Colony) + (1|ColNestLaying),
                data = dat_Succ_nat, family = "bernoulli", cores = 4,
                iter = 8000, thin = 10,
                control = list(adapt_delta = 0.99, max_treedepth = 14),
                file="FledgeSucc_Fenology_shift_nat")
mod_s<-add_criterion(mod_s, "loo", cores=4)


mod_s_ = brms::brm(succ ~ s(laying_julian_shift)+Col_Size.z+
                    (1|Pair)+(1|Season)+(1|Colony) + (1|ColNestLaying),
                  data = dat_Succ_prot, family = "bernoulli", cores = 4,
                  iter = 8000, thin = 10,
                  control = list(adapt_delta = 0.99, max_treedepth = 14),
                  file="FledgeSucc_Fenology_shift_prot")
mod_s_<-add_criterion(mod_s_, "loo", cores=4)


#extract Plotting values
marg<-conditional_effects(mod_s, resolution=1000)
#select the variable of interest
laying_date<-marg$laying_julian_shift

#extract Plotting values
marg_<-conditional_effects(mod_s_, resolution=1000)
#select the variable of interest
laying_date_<-marg_$laying_julian_shift

#plot months instead of julian date

laying_date$Laying_date_dd_mm<-as.Date(laying_date$laying_julian, origin = "2018-09-01")

laying_date_$Laying_date_dd_mm<-as.Date(laying_date_$laying_julian, origin = "2018-09-01")



ggplot()+
  geom_line(data = laying_date, aes(x=Laying_date_dd_mm,y=estimate__, ymax=upper__ , ymin=lower__),size=1.3, color="#007373")+
  geom_line(data = laying_date_, aes(x=Laying_date_dd_mm,y=estimate__, ymax=upper__ , ymin=lower__),size=1.3, color="#D55E00")+
  geom_ribbon(data = laying_date, aes(x=Laying_date_dd_mm,y=estimate__, ymax=upper__, ymin=lower__),fill="#007373",alpha=0.3)+
  geom_ribbon(data = laying_date_, aes(x=Laying_date_dd_mm,y=estimate__, ymax=upper__, ymin=lower__),fill="#D55E00",alpha=0.3)+
  geom_point(data = dat_Succ_nat, aes(x=Laying_date_dd_mm_shift,y=ifelse(succ==0,succ-0.15,succ+0.05)), 
             color="#007373",alpha=0.1,position = position_jitter(w = 0, h = 0.04)) + 
  geom_point(data = dat_Succ_prot, aes(x=Laying_date_dd_mm_shift,y=ifelse(succ==1,succ+0.15,succ-0.05)), 
             color="#D55E00",alpha=0.1,position = position_jitter(w = 0, h = 0.04)) + 
  scale_x_date(name="Laying date", date_breaks = "months" , date_labels = "%b")+
  scale_y_continuous(name="Fledging success", breaks=seq(0,1, 0.1))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

ggsave("FledgeSucc_Fenology_revision.eps", device=cairo_ps, width = 10, height = 7,
       units ="cm", fallback_resolution = 600)


##extract credible intervals for the paper

#extract Plotting values
marg_v<-conditional_effects(mod_s, resolution=352) #re_formula=NULL is to include the random factors
#why 352? Because we have data from the 10th of September to the 28th of august so the model does not estimate further

#select the variable of interest
laying_all_days<-marg_v$laying_julian_shift
laying_all_days$Laying_date_dd_mm<-as.Date(laying_all_days$laying_julian, origin = "2018-08-31")



#"2018-09-15"
marg_v$laying_julian_shift$estimate__[6]
marg_v$laying_julian_shift$lower__[6]
marg_v$laying_julian_shift$upper__[6]

#0.4159526 [0.135813, 0.7724666}


#first of May
marg_v$laying_julian_shift$estimate__[234]
marg_v$laying_julian_shift$lower__[234]
marg_v$laying_julian_shift$upper__[234]

#0.8729608[0.6265177,0.9740603]

#"2018-12-15"
marg_v$laying_julian_shift$estimate__[97]
marg_v$laying_julian_shift$lower__[97]
marg_v$laying_julian_shift$upper__[97]

#0.01114076[0.002619073,0.04523553]


#extract Plotting values
marg_v_<-conditional_effects(mod_s_, resolution=358) #,  re_formula=NULL
#select the variable of interest
laying_all_days_<-marg_v_$laying_julian_shift
laying_all_days_$Laying_date_dd_mm<-as.Date(laying_all_days_$laying_julian, origin = "2018-08-31")

#"2018-09-15"
marg_v_$laying_julian_shift$estimate__[12]
marg_v_$laying_julian_shift$lower__[12]
marg_v_$laying_julian_shift$upper__[12]
#0.6386401[0.4009761, 0.8330594]


#first of May
marg_v_$laying_julian_shift$estimate__[240]
marg_v_$laying_julian_shift$lower__[240]
marg_v_$laying_julian_shift$upper__[240]
#0.5683703 [0.3488524,  0.7723545]


#"2018-12-15"
marg_v_$laying_julian_shift$estimate__[103]
marg_v_$laying_julian_shift$lower__[103]
marg_v_$laying_julian_shift$upper__[103]
#0.4627981[0.272463,0.6726402]




