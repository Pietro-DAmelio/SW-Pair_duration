#Aim: Describe the Seasonality of success (nestling survival) and whether it is depending on predation
#Author: P. D'Amelio


### libraries  ###
library(ggplot2)
library(brms)
library(loo)

#load database
dat<- read.csv("YOUR_DIRECTORY/Longitudinal_dataset.csv")


#adjust data format
dat$Laying_date_dd_mm<- format(as.Date(dat$Laying_date, format="%Y-%m-%d"),"%m-%d")

dat$Laying_date_dd_mm <-as.POSIXct(strptime(dat$Laying_date_dd_mm, format="%m-%d"))


#select columns of interest
dat_Succ<-dat[,c("Laying_date_dd_mm","succ", "Pair", "Season", "predation", "Colony", "Col_Size", "ColNestLaying", "unique_identifier")]

#natural
dat_Succ_nat<- subset(dat_Succ, predation=="natural")
dat_Succ_nat<-unique(dat_Succ_nat)
dat_Succ_nat<-dat_Succ_nat[complete.cases(dat_Succ_nat), ]
dat_Succ_nat$Col_Size.z<- scale(dat_Succ_nat$Col_Size)
dat_Succ_nat$laying_julian<-as.numeric(format(dat_Succ_nat$Laying_date_dd_mm, "%j"))
dat_Succ_nat$Colony<- as.factor(dat_Succ_nat$Colony)

#adjust first day of the seaason
dat_Succ_nat$laying_julian_shift<- ifelse(dat_Succ_nat$laying_julian>244, dat_Succ_nat$laying_julian-243, dat_Succ_nat$laying_julian+122)
dat_Succ_nat$Laying_date_dd_mm_shift<-as.Date(dat_Succ_nat$laying_julian_shift, origin = "2018-08-31")

#protected
dat_Succ_prot<- subset(dat_Succ, predation=="protected")
dat_Succ_prot<-unique(dat_Succ_prot)
dat_Succ_prot<-dat_Succ_prot[complete.cases(dat_Succ_prot), ]
dat_Succ_prot$Col_Size.z<- scale(dat_Succ_prot$Col_Size)
dat_Succ_prot$laying_julian<-as.numeric(format(dat_Succ_prot$Laying_date_dd_mm, "%j"))
dat_Succ_prot$Colony<- as.factor(dat_Succ_prot$Colony)



#adjust first day of the seaason
dat_Succ_prot$laying_julian_shift<- ifelse(dat_Succ_prot$laying_julian>244, dat_Succ_prot$laying_julian-243, dat_Succ_prot$laying_julian+122)
dat_Succ_prot$Laying_date_dd_mm_shift<-as.Date(dat_Succ_prot$laying_julian_shift, origin = "2018-08-31")




#run models with September as first month
mod_s = brms::brm(succ ~ s(laying_julian_shift)+Col_Size.z+
                  (1|Pair)+(1|Season)+(1|Colony) + (1|ColNestLaying),
                data = dat_Succ_nat, family = "bernoulli", cores = 4,
                iter = 8000, thin = 10,
                control = list(adapt_delta = 0.99, max_treedepth = 14),
                file="FledgeSucc_Fenology_shift_nat1")
mod_s<-add_criterion(mod_s, "loo", cores=4)


mod_s_ = brms::brm(succ ~ s(laying_julian_shift)+Col_Size.z+
                    (1|Pair)+(1|Season)+(1|Colony) + (1|ColNestLaying),
                  data = dat_Succ_prot, family = "bernoulli", cores = 4,
                  iter = 8000, thin = 10,
                  control = list(adapt_delta = 0.99, max_treedepth = 14),
                  file="FledgeSucc_Fenology_shift_prot1")
mod_s_<-add_criterion(mod_s_, "loo", cores=4)


#extract Plotting values
marg<-marginal_effects(mod_s, resolution=1000)
#select the variable of interest
laying_date<-marg$laying_julian_shift

#extract Plotting values
marg_<-marginal_effects(mod_s_, resolution=1000)
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
  scale_x_date(date_breaks = "months" , date_labels = "%b")+
  scale_y_continuous(name="fledging success", breaks=seq(0,1, 0.1))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

ggsave("FledgeSucc_Fenology1.eps", device=cairo_ps, width = 10, height = 7,
       units ="cm", fallback_resolution = 600)

