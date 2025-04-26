#***************************************************************************
#***************************************************************************
#** Project: Mammal and Fisheries Interaction in the Southern North Sea  ***
#**              File: Import Ecopath files and export MTI               ***
#**                    Developer: Matt Woodstock                         ***
#***************************************************************************
#***************************************************************************

## Citation ##
#* Woodstock, M.S., J. J. Kiszka, P. Evans, J. Waggitt, Y. Zhang. (in prep).


## Clear workspace##
rm(list=ls())

## Load libraries ##
library(MASS)
require(svMisc)
library(xlsx)
library(ggplot2)
library(ggpubr)


## Set global working directory
workdir<-"D:/North Sea Ecosystem Model/1990 Start Time/"
setwd(workdir)

## Load species list ##
species<-read.csv("Species.csv",header=T)
fleet <- read.csv("fleets.csv",header=T)

## Set global parameters ##
n_sim<-100
n_spec<-length(species[,2])
n_year<-25
n_year_concern<-25
n_fisher<-length(fleet$Fleet) #Number of fisheries in the model
year_start<-1990
year_end<-2014
plotwd <- "D:/North Sea Ecosystem Model/1990 Start Time/Plots"
#### Plots for manuscript ####
load(file="Mean Mammals/Ecosim Output/MTI Output/Mixed Trophic Impact Input.RData")

## For all species #### Figure 2

all_obj <- n_spec+n_fisher
spec_dat <- data.frame(Species_1 = rep("",(all_obj*all_obj*n_year)),
                    Species_2 = rep("",(all_obj*all_obj*n_year)),
                    Year = rep("",(all_obj*all_obj*n_year)),
                    Average = rep(0,(all_obj*all_obj*n_year)),
                    sd = rep(0,(all_obj*all_obj*n_year)),
                    Type = rep("",(all_obj*all_obj*n_year)),
                    Order_1 = rep(0,(all_obj*all_obj*n_year)),
                    Order_2 = rep(0,(all_obj*all_obj*n_year)),
                    Sig = rep("",all_obj*all_obj*n_year))
                    

spec_name <- c(species[,2],fleet$Fleet)
count<-1

for (spec1 in 1:all_obj){
  for (spec2 in 1:all_obj){
    for (year in year_start:year_end){
      spec_dat$Species_1[count] <- spec_name[spec1]
      spec_dat$Species_2[count] <- spec_name[spec2]
      spec_dat$Year[count] <- year
      spec_dat$Average[count] <- mean(na.omit(MTI[spec1,spec2,(year-(year_start-1)),c(1:n_sim)]))
      spec_dat$sd[count] <- sd(na.omit(MTI[spec1,spec2,(year-(year_start-1)),c(1:n_sim)]))
      spec_dat$Order_1[count] <- spec1
      spec_dat$Order_2[count] <- spec2
      if (spec2 > spec1){
        spec_dat$Type[count]<-"Enforce"
      } else if (spec2 < spec1){
        spec_dat$Type[count]<-"Receive"
      } else {spec_dat$Type[count]<-"Same Group"}
      #if (p[spec1,spec2] <= 0.05){
      #  spec_dat$Sig[count] <- "*"
      #}
      count <- count +1
    }
    print(c(spec1,spec2))
    Sys.sleep(0.01)
  }
}
spec_dat$CI <- c(spec_dat$sd/sqrt(n_sim)*qt(0.975,n_sim-1))

## Heatmap to show magnitudes ##
year_one <- subset(spec_dat,Year==1990)
year_last <- subset(spec_dat,Year==2014)

difference <- year_last
difference$Average <- year_last$Average-year_one$Average

  for (a in 1:dim(year_one)[1]){
    if (year_one$Average[a]> 1){
      year_one$Average[a] <- 1
    } else if (year_one$Average[a] < -1){
      year_one$Average[a] <- -1
    }
  }
year_one$Average[year_one$Average>1] <- 1
year_one$Average[year_one$Average< -1] <- -1

min(year_one$Average)

p_1<- ggplot(difference,aes(x=reorder(Order_2,Order_2),y=reorder(Species_1,-Order_1),fill=Average))+geom_tile()+
  scale_fill_gradientn(colours=c("red","white","blue"),limits=c(-0.3,0.3),labels=c("Negative","Neutral","Positive"),breaks=c(-0.3,0,0.3))+scale_x_discrete(position="top")+ylab("Impacting Group")+ 
  xlab("Impacted Group")+ylab("")+
  #geom_text(aes(label=Sig),vjust=0.75)+
  theme(legend.title = element_blank(),
        axis.text = element_text(size=8),
        legend.position = "right",axis.text.x = element_text(size=6))+
  geom_vline(xintercept=50.5)+
  geom_hline(yintercept = 12.5)+
  geom_vline(xintercept=9.5)+
  geom_hline(yintercept = 53.5)

setwd(plotwd)
ggsave("Difference Mean MTI.png",p_1)



p_2 <-ggplot(year_one,aes(x=reorder(Order_2,Order_2),y=reorder(Species_1,-Order_1),fill=Average))+geom_tile()+
  scale_fill_gradientn(colours=c("red","white","blue"),limits=c(-1,1),labels=c("Negative","Neutral","Positive"),breaks=c(-1,0,1))+scale_x_discrete(position="top")+
  xlab("Impacted Group")+ylab("Impacting Group")+ 
  #geom_text(aes(label=Sig),vjust=0.75)+
  theme(legend.title = element_blank(),
        axis.text = element_text(size=8),
        legend.position = "right",axis.text.x = element_text(size=6))+
  geom_vline(xintercept=50.5)+
  geom_hline(yintercept = 12.5)+
  geom_vline(xintercept=9.5)+
  geom_hline(yintercept = 53.5)

setwd(plotwd)
ggsave("Year One Mean MTI.png",p_2)

ggarrange(p_1,p_2,labels = c("A","B"),ncol=1)
ggsave("All MTI.png",last_plot(),height = 12,units = "in")
## Stacked Area Plot of total consumption ####
#* Going to try seabird,mammals,and fisheries
#* Fisheries catches = consumption

## Import total consumption ## 
