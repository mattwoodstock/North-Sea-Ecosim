## Stacked Area Plot of total consumption ####
#* Going to try seabird,mammals,and fisheries
#* Fisheries catches = consumption

## Import total consumption ## 
## Clear workspace##
rm(list=ls())

## Load libraries ##
library(ggplot2)
library(ggpubr)
library(gplots)
library(abind)
library(colortools)

## Set global working directory
workdir<-"D:/North Sea Ecosystem Model/1990 Start Time/"
setwd(workdir)

## Load species list ##
species<-read.csv("Species.csv",header=T)
species<-as.matrix(species)
ord <- read.csv("Ordered Species.csv",header=T)
ord<-as.matrix(ord)

## Set global parameters ##
n_sim<-100
n_spec<-length(species[,2])
n_year<-25
n_fisher<-12 #Number of fisheries in the model
year_start <- 1990
year_end <- 2014

## Blank matrices

biomass<-array(0,dim=c(n_spec,n_year,n_sim))
qb<-array(0,dim=c(n_spec,n_year,n_sim))
catch <- array(0,dim=c(n_fisher,n_year,n_sim))

#Run Simulation####
for (file_count in 1:n_sim){
  if (file_count < 10){
    foldername<-paste0(workdir,"Mean Mammals/Ecosim Output/mc_North Sea Balanced Simulation/mc_output_trial000",file_count,sep="")
    if ((dir.exists(foldername))){
      setwd(foldername)
    }
  }
  if (file_count < 100){
    foldername<-paste0(workdir,"Mean Mammals/Ecosim Output/mc_North Sea Balanced Simulation/mc_output_trial00",file_count,sep="")
    if ((dir.exists(foldername))){
      setwd(foldername)
    }
  }
  if (file_count < 1000){
    foldername<-paste0(workdir,"Mean Mammals/Ecosim Output/mc_North Sea Balanced Simulation/mc_output_trial0",file_count,sep="")
    if ((dir.exists(foldername))){
      setwd(foldername)
    }
  }
  if (file_count < 10000){
    foldername<-paste0(workdir,"Mean Mammals/Ecosim Output/mc_North Sea Balanced Simulation/mc_output_trial",file_count,sep="")
    if ((dir.exists(foldername))){
      setwd(foldername)
    }
  }
  
  ## Import Biomass ##
  filename<-paste("biomass_annual.csv",sep="")
  if (file.exists(filename)){
    dat<-read.csv(filename,header=T) #Necessary data always starts on row 10
    dat<-dat[,-1]
    for (a in 1:n_year){
      for (b in 1:n_spec){
        biomass[b,a,file_count]<-dat[a,b]
      }
    }
    if (a == n_year){
      mess <- paste("Biomass Loaded For Iteration: ",file_count,sep="")
      print(mess)
      Sys.sleep(0.01)
    }
  }
    
    ## Import QB ##
    filename<-paste("consumption-biomass_annual.csv",sep="")
    if (file.exists(filename)){
      dat<-read.csv(filename,header=T) #Necessary data always starts on row 10
      dat<-dat[,-1]
      for (a in 1:n_year){
        for (b in 1:n_spec){
          qb[b,a,file_count]<-dat[a,b]
        }
      }
      if (a == n_year){
        mess <- paste("QB Loaded For Iteration: ",file_count,sep="")
        print(mess)
        Sys.sleep(0.01)
      }
    }
    
    ## Import catches ##
    filename<-"catch-fleet-group_annual.csv" #Fishery Catches
    if (file.exists(filename)){
      fishery<-read.csv(filename,header=T)
      for (y in year_start:year_end){ #Input years your model starts and ends
        for (x in 1:n_fisher){
          sub_catch <- subset(fishery,fishery$year==y & fishery$fleet==x)
          for (z in 1:length(sub_catch[,1])){
            for (w in 1:n_spec){
              if (sub_catch[z,3]==w){
                catch[x,(y-(year_start-1)),file_count] <- catch[x,(y-(year_start-1)),file_count]+sub_catch[z,4]
              }
            }
          }
        }
      }
      if (y == year_end){
        mess <- paste("Catches Loaded For Iteration: ",file_count,sep="")
        print(mess)
        Sys.sleep(0.01)
      }
    }
}
## Need total consumption for each species in iteration ##
q <- biomass * qb

## Combine species and fisheries arrays ##
new_q <- abind(q,catch,along = 1)


## Average all iterations and arrange guilds in a dataframe ##
avg_q <- data.frame()
years <- year_start:year_end
count<-1


for (spec in 1:dim(new_q)[1]){
  for (year in 1:dim(new_q)[2]){
    avg_q[count,1] <- ord[spec,2]
    avg_q[count,2] <- years[year]
    avg_q[count,3] <- ord[spec,3]
    avg_q[count,4] <- mean(na.omit(new_q[spec,year,]))
    count <- count +1
  }
}
colnames(avg_q) <- c("Species","Year","Group","Consumption")


head(avg_q)
aggregate(avg_q$Consumption,by=list(avg_q$Year,avg_q$Group),FUN="sum")

## Plot stacked area chart ##
sub_q <- subset(avg_q,Group =="Mammal"|Group=="Seabird"|Group=="Fishery")

head(sub_q)
dat<-aggregate(sub_q$Consumption,by=list(sub_q$Year,sub_q$Group),FUN="sum")

#Set Color Scheme
group_c<- splitComp("steelblue")
bird_col<- sequential(group_c[1])
mammal_col <- sequential(group_c[2])
fishery_col <- sequential(group_c[3])


dat$x[51:75]/dat$x[1:25]



#Assign colors to groups#
bird_spec <- subset(sub_q,Group=="Seabird")
mammal_spec <- subset(sub_q,Group=="Mammal")
fishery_spec <- subset(sub_q,Group=="Fishery")

for (a in 1:length(sub_q[,1])){
  if (sub_q$Group[a]=="Seabird"){
    for (bird in 1:length(unique(bird_spec$Species))){
      if (bird_spec$Species[bird] == sub_q$Species[a]){
        sub_q$Col[a] <- bird_col[length(bird_col)-2*bird]
      }
    }
  } else if (sub_q$Group[a]=="Mammal"){
    for (mam in 1:length(unique(mammal_spec$Species))){
      if (mammal_spec$Species[mam] == sub_q$Species[a]){
        sub_q$Col[a] <- mammal_col[length(mammal_col)-2*mam]
      }
    }
  } else {
    for (fish in 1:length(unique(fishery_spec$Species))){
      if (fishery_spec$Species[fish] == sub_q$Species[a]){
        sub_q$Col[a] <- fishery_col[length(fishery_col)-fish]
      }
    }
  }
}

## Reorder factors ##
sub_q$Species<-factor(sub_q$Species,levels=
                        c("Beam Trawl","Demersal Trawl & Seine","Sandeel Trawl",
                          "Dredge","Drift and Fixed Nets","Hooked Gears","Nephrops Trawl",
                          "Shrimp Trawls","Pelagic Trawl","Pots","Shellfish Gear",
                          "Other Gear","Harbour Porpoise","Minke whales",
                          "White-beaked dolphin","Grey Seal",
                          "Harbour Seal","Auks","Fulmars and Petrels","Gulls",
                          "Gannets"))


head(sub_q)
test1<-sub_q[sub_q$Group=="Mammal",]
ans1<-aggregate(test1$Consumption,by=list(test1$Year),FUN="sum")

test2<-sub_q[sub_q$Group=="Fishery",]
ans2<-aggregate(test2$Consumption,by=list(test2$Year),FUN="sum")

min(ans1$x/ans2$x)
max(ans1$x/ans2$x)

ans1$x/ans2$x

1.89/0.77
4.6/2.4

#Make plot
dev.new()
ggplot(sub_q, aes(x=Year, y=Consumption, fill=Species)) +
  geom_area(alpha=1,size=0.1,colour="gray20")+
  theme_bw()+xlim(year_start,year_end)+ylab("Population Consumption (t/km2)")+
  scale_fill_manual(values=c(fishery_col[21],fishery_col[19],
                             fishery_col[17],fishery_col[16],
                             fishery_col[15],fishery_col[14],
                             fishery_col[13],fishery_col[11],
                             fishery_col[9],fishery_col[7],
                             fishery_col[5],fishery_col[3],
                             mammal_col[21],mammal_col[19],
                             mammal_col[17],mammal_col[15],
                             mammal_col[13],mammal_col[11],
                             bird_col[21],bird_col[19],
                             bird_col[17],bird_col[15],
                             bird_col[13],bird_col[11],
                             bird_col[9]))+
  scale_x_continuous(name="Year",breaks=c(years),limits=c(year_start,year_end),
                     position = "bottom")+ylim(0,8)+
  guides(fill=guide_legend(title="Fishery/Predator"))+
  xlab("Year")+  theme(panel.grid.minor = element_blank(),
                       panel.grid.major.x = element_blank(),
                       legend.position = "bottom",
                       axis.text = element_text(size=12),
                       axis.title = element_text(size=13))

save(sub_q,q,file="Consumption values for Figure 6.RData")
head(sub_q)
## Subset for results section ##
new_sub <- subset(sub_q,Group=="Fishery")
head(new_sub)
sum_1 <-0
sum_2 <-0
for (a in 1:length(new_sub$Species)){
  if (new_sub$Year[a]==2003){
    sum_1 <- sum_1 + new_sub$Consumption[a]
  } else if (new_sub$Year[a]==2014){
    sum_2 <- sum_2 + new_sub$Consumption[a]
  }
}

new_sub <- subset(sub_q,Species=="Trammel")
new_sub[12,4]/new_sub[1,4]-1

#*Beam = -0.119
#*Bottom = -0.063
#*Dredge = 0.162
#*Gillnet = 0.031
#*Longline = 0.027
#*Otter Trawl = 0.236
#*Pelagic Seine = 0.033
#*Pots = 0.066
#*Trammel = 0.580

sum_1 <-0
sum_2 <-0
for (a in 1:length(new_sub$Species)){
  if (new_sub$Year[a]==2003){
    sum_1 <- sum_1 + new_sub$Consumption[a]
  } else if (new_sub$Year[a]==2014){
    sum_2 <- sum_2 + new_sub$Consumption[a]
  }
}
sum_2/sum_1-1
