##### North Sea Ecosystem Model Results ####

#### Begin ####
{
## Clear workspace ##
rm(list=ls())

## Set Working Directory ##
workdir <- "D:/North Sea Ecosystem Model/1990 Start Time/North Sea Ecosim Runs"
setwd(workdir)

plotwd <- "D:/North Sea Ecosystem Model/1990 Start Time/Plots"

## Scenario directory ##
mean_scendir <- paste(workdir,"/Mean Mammals/mc_NS_mean",sep="")
low_scendir <- paste(workdir,"/Low Mammals/mc_New Ecosim scenario",sep="")
high_scendir <- paste(workdir,"/High Mammals/mc_High Mammals",sep="")
SCAN_scendir <- paste(workdir,"/SCAN Mammals/mc_New Ecosim scenario",sep="")

## Load libraries ##
packages = c("ncdf4", "RNetCDF","ggplot2","R.utils","wesanderson","tidyverse",
             "scales","cowplot","here","broom","ggridges","viridis","forcats","hrbrthemes","svMisc",
             "marmap","rworldmap","sf","ggpubr","colortools","FSA","ggspatial")

package.check <- lapply(packages,FUN = function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x, dependencies = TRUE)
    library(x, character.only = TRUE)
  }
})



## Global Attributes ##
n_spec<-50 #Number of functional groups
n_fisher<- 12 #Number of fisheries in the model
year_start<- 1990 #Model Start Year
year_end<- 2014  #Model End Year
n_year <- 25  #Could be different/implemented if you include a spin up to the years of concern
species <- read.csv("Species.csv")
fleet <- read.csv("fleets.csv")
}

#### Map (Ready) ----
{
  #### Create map
  Bathy = getNOAA.bathy(lon1 = -5, lon2 = 10, lat1 = 48, lat2 = 60, 
                        resolution = 1)
  Bathy <- Bathy * -1
  Bathy[Bathy<0] <- NA
  bf = fortify.bathy(Bathy)

  autoplot.bathy(Bathy, geom=c("tile")) +
    scale_fill_gradient2(low="dodgerblue", mid="lightskyblue", high="darkblue") +
    labs(y = "Latitude", x = "Longitude", fill = "Depth (m)") + 
    theme(text=element_text(size=16,family="serif",face="bold"),plot.background = element_rect(colour="black",size=1))+
    #xlim(-5,10)+ylim(48,60)+
    theme_bw()+
    annotate(geom = "text", x = 3, y = 55.9, label = "4b", color = "black", size = 6)+
    annotate(geom = "text", x = 3, y = 52.3, label = "4c", color = "black", size = 6)+
    annotate(geom = "text", x = 0, y = 50.2, label = "7d", color = "black", size = 6)+
    geom_segment(aes(x = -1.8, y = 57.5, xend = 7.8, yend = 57.5),colour="black",size=2)+
    geom_segment(aes(x = -1.9, y = 49.7, xend = -1.9, yend = 50.7),colour="black",size=2)+
    geom_segment(aes(x = 1, y = 51, xend = 1.7, yend = 51),colour="black",size=2)+
    geom_segment(aes(x = 0.3, y = 53.2, xend = 6.1, yend = 53.2),colour="black",size=2)+
    geom_segment(aes(x = 7.8, y = 57.5, xend = 8.3, yend = 56.9),colour="black",size=2)+
    annotation_north_arrow(location = "bl", which_north = "true",pad_x = unit(0.1, "in"), pad_y = unit(0.1, "in"),style = north_arrow_fancy_orienteering)

  setwd(plotwd)
  ggsave("Map.png",last_plot())
}

#### Ecosystem Indicators (Ready) ----
{
  cat("\nKempton Sq Calculation\n")
## High Scenario ##
kempton_sq <- data.frame()
setwd(high_scendir)
files <- list.files()
for (file_count in 1:n_distinct(files)){
  if (files[file_count]!= "mc_input"){
    setwd(paste(high_scendir,"/",files[file_count],sep=""))
    dat <- read.csv("kemptonsq_annual.csv")
    dat$Sim <- files[file_count]
    kempton_sq <- rbind(kempton_sq,dat)
  }
}

summed_high <- kempton_sq %>% group_by(year) %>% 
  summarise(mean(value)) %>% rename(Average = `mean(value)`)

summed_high_sd <- kempton_sq %>% group_by(year) %>% 
  summarise(sd(value)) %>% rename(SD = `sd(value)`)

data_high <- merge(summed_high,summed_high_sd,by="year")

data_high$Scenario <- c("High Predator")

  ## Mean Scenario ##
kempton_sq <- data.frame()
setwd(mean_scendir)
files <- list.files()
for (file_count in 1:n_distinct(files)){
  if (files[file_count]!= "mc_input"){
    setwd(paste(mean_scendir,"/",files[file_count],sep=""))
    dat <- read.csv("kemptonsq_annual.csv")
    dat$Sim <- files[file_count]
    kempton_sq <- rbind(kempton_sq,dat)
  }
}

summed_mean <- kempton_sq %>% group_by(year) %>% 
  summarise(mean(value)) %>% rename(Average = `mean(value)`)

summed_mean_sd <- kempton_sq %>% group_by(year) %>% 
  summarise(sd(value)) %>% rename(SD = `sd(value)`)

data_mean <- merge(summed_mean,summed_mean_sd,by="year")

data_mean$Scenario <- c("Mean Predator")


## Low Scenario ##
kempton_sq <- data.frame()
setwd(low_scendir)
files <- list.files()
for (file_count in 1:n_distinct(files)){
  if (files[file_count]!= "mc_input"){
    setwd(paste(low_scendir,"/",files[file_count],sep=""))
    dat <- read.csv("kemptonsq_annual.csv")
    dat$Sim <- files[file_count]
    kempton_sq <- rbind(kempton_sq,dat)
  }
}

summed_low <- kempton_sq %>% group_by(year) %>% 
  summarise(mean(value)) %>% rename(Average = `mean(value)`)
summed_low_sd <- kempton_sq %>% group_by(year) %>% 
  summarise(sd(value)) %>% rename(SD = `sd(value)`)

data_low <- merge(summed_low,summed_low_sd,by="year")

data_low$Scenario <- c("Low Predator")


## SCANS Scenario ##
kempton_sq <- data.frame()
setwd(SCAN_scendir)
files <- list.files()
for (file_count in 1:n_distinct(files)){
  if (files[file_count]!= "mc_input"){
    setwd(paste(SCAN_scendir,"/",files[file_count],sep=""))
    dat <- read.csv("kemptonsq_annual.csv")
    dat$Sim <- files[file_count]
    kempton_sq <- rbind(kempton_sq,dat)
  }
}

summed_SCAN <- kempton_sq %>% group_by(year) %>% 
  summarise(mean(value)) %>% rename(Average = `mean(value)`)
summed_SCAN_sd <- kempton_sq %>% group_by(year) %>% 
  summarise(sd(value)) %>% rename(SD = `sd(value)`)
data_SCAN <- merge(summed_SCAN,summed_SCAN_sd,by="year")

data_SCAN$Scenario <- c("SCAN Predator")


all_dat <- rbind(data_high,data_mean,data_low,data_SCAN)

all_dat$Scenario <- factor(all_dat$Scenario,levels=rev(c("SCAN Predator","Low Predator","Mean Predator","High Predator")))

library(colortools)
cols<-tetradic("blue",plot=F)
cols[3] <- "orange"
cols[4] <- "black"


all_dat %>% filter(Scenario %in% "Low Predator")

fig3.1<-ggplot(all_dat,aes(x=year,y=Average,colour=Scenario))+
  geom_ribbon(aes(xmin=min(year),xmax=max(year),ymin=Average-SD,ymax=Average+SD,fill=Scenario),alpha=0.2,linetype="dotted")+scale_fill_manual(values=c(cols[1],cols[4],cols[3],cols[2]))+
  geom_line(size=1.5)+theme(text = element_text(size=16,family="serif"),axis.text = element_text(size=16,family="serif"))+
  scale_colour_manual(values=c(cols[1],cols[4],cols[3],cols[2]))+
  theme_classic()+
  ylab("Kempton's Q")+
  xlab("")+ylim(4,7.5)

####Total Catch ###
cat("\nTotal Catch Calculation\n")
## High Scenario ##
catch <- data.frame()
setwd(high_scendir)
files <- list.files()

for (file_count in 1:n_distinct(files)){
  if (files[file_count]!= "mc_input"){
    setwd(paste(high_scendir,"/",files[file_count],sep=""))
    dat <- read.csv("totalcatch_annual.csv")
    dat$Sim <- files[file_count]
    catch <- rbind(catch,dat)
  }
}
summed_high <- catch %>% group_by(year) %>% 
  summarise(mean(value)) %>% rename(Average = `mean(value)`)
summed_high_sd <- catch %>% group_by(year) %>% 
  summarise(sd(value)) %>% rename(SD = `sd(value)`)
data_high <- merge(summed_high,summed_high_sd,by="year")
data_high$Scenario <- c("High Predator")


## Mean Scenario ##
catch <- data.frame()
setwd(mean_scendir)
files <- list.files()

for (file_count in 1:n_distinct(files)){
  if (files[file_count]!= "mc_input"){
    setwd(paste(mean_scendir,"/",files[file_count],sep=""))
    dat <- read.csv("totalcatch_annual.csv")
    dat$Sim <- files[file_count]
    catch <- rbind(catch,dat)
  }
}

summed_mean <- catch %>% group_by(year) %>% 
  summarise(mean(value)) %>% rename(Average = `mean(value)`)
summed_mean_sd <- catch %>% group_by(year) %>% 
  summarise(sd(value)) %>% rename(SD = `sd(value)`)
data_mean <- merge(summed_mean,summed_mean_sd,by="year")
data_mean$Scenario <- c("Mean Predator")


## Low Scenario ##
catch <- data.frame()
setwd(low_scendir)
files <- list.files()

for (file_count in 1:n_distinct(files)){
  if (files[file_count]!= "mc_input"){
    setwd(paste(low_scendir,"/",files[file_count],sep=""))
    dat <- read.csv("totalcatch_annual.csv")
    dat$Sim <- files[file_count]
    catch <- rbind(catch,dat)
  }
}


summed_low <- catch %>% group_by(year) %>% 
  summarise(mean(value)) %>% rename(Average = `mean(value)`)
summed_low_sd <- catch %>% group_by(year) %>% 
  summarise(sd(value)) %>% rename(SD = `sd(value)`)
data_low <- merge(summed_low,summed_low_sd,by="year")
data_low$Scenario <- c("Low Predator")

setwd("D:/North Sea Ecosystem Model/1990 Start Time")
write.csv(catch,"Catch Data.csv")

## SCAN Scenario ##
catch <- data.frame()
setwd(SCAN_scendir)
files <- list.files()

for (file_count in 1:n_distinct(files)){
  if (files[file_count]!= "mc_input"){
    setwd(paste(SCAN_scendir,"/",files[file_count],sep=""))
    dat <- read.csv("totalcatch_annual.csv")
    dat$Sim <- files[file_count]
    catch <- rbind(catch,dat)
  }
}
summed_SCAN <- catch %>% group_by(year) %>% 
  summarise(mean(value)) %>% rename(Average = `mean(value)`)
summed_SCAN_sd <- catch %>% group_by(year) %>% 
  summarise(sd(value)) %>% rename(SD = `sd(value)`)
data_SCAN <- merge(summed_SCAN,summed_SCAN_sd,by="year")
data_SCAN$Scenario <- c("SCAN Predator")

all_dat <- rbind(data_high,data_mean,data_low,data_SCAN)



library(colortools)
cols<-tetradic("blue",plot=F)
cols[3] <- "orange"
cols[4] <- "black"

fig3.2<- ggplot(all_dat,aes(x=year,y=Average,colour=Scenario))+
  geom_ribbon(aes(xmin=min(year),xmax=max(year),ymin=Average-SD,ymax=Average+SD),alpha=0.2,linetype="dotted")+
  scale_colour_manual(values=c(cols[1],cols[4],cols[3],cols[2]))+ylab("Catch (t/km2)")+scale_fill_manual(values=c(cols[1],cols[4],cols[3],cols[2]))+
  geom_line(size=1.5)+
  theme_classic()+
  
  theme(text = element_text(size=16,family="serif"),
        axis.text = element_text(size=16,family="serif",colour="white"),
        legend.text = element_text(size=16,colour="white"),
        panel.background = element_rect(fill="transparent",colour=NA),
        plot.background = element_rect(fill="transparent",colour=NA),
        axis.title = element_text(colour="white"),
        legend.background = element_rect(fill="transparent",colour=NA),
        axis.line = element_line(colour="white"),axis.ticks = element_line(colour="white"),legend.title = element_text(colour="white"),strip.text = element_text(colour="white"))+
  xlab("")+
  xlim(1990,2014)+
  ylim(0,15)

fig3.2

getwd()
ggsave("Total catch_2.png",fig3.2)
#### Cumulative Biomass vs. Trophic Level ###

  cat("\nCumB vs TL and CumB vs CumB Calculation\n")

setwd(paste(plotwd,"/CumB_tl",sep=""))
files <- list.files()
all_dat <- data.frame()

for (a in 1:n_distinct(files)){
  dat <- read.csv(files[a])
  
  all_dat <- rbind(all_dat,dat)
}

tl_median <- all_dat %>% group_by(year.group.x,Species) %>% 
  summarize(median(value.x)) %>% 
  rename(TL_median = `median(value.x)`)

biom_median <- all_dat %>% group_by(year.group.x,Species) %>%  
  summarize(median(CumB)) %>% 
  rename(B_median = `median(CumB)`)

prod_median <- all_dat %>% group_by(year.group.x,Species) %>%  
  summarize(median(CumP)) %>% 
  rename(P_median = `median(CumP)`)

colfunc <- colorRampPalette(c("lightblue", "darkblue"))
cols <-colfunc(25)
plotable <- data.frame(Year = tl_median$year.group.x,
                      Species = tl_median$Species,
                      `TL median` = tl_median$TL_median,
                      `Biomass median` = biom_median$B_median,
                      `Production median` = prod_median$P_median)

plotable$Year <- as.factor(plotable$Year)

fig3.3<- ggplot(plotable,aes(x=TL.median,y=Biomass.median,colour=Year))+
  geom_point()+geom_line()+scale_color_manual(values = cols)+
  #scale_colour_gradient(low="lightblue",mid="blue",high="darkblue")+
  theme_classic()+
  theme(legend.position="none")+
  ylab("Cumulative Biomass (t/km2)")+xlab("Trophic Level")
  
plotable <- plotable[order(plotable$Year,plotable$Biomass.median),]
dim(plotable)
count <- 1
while (count <= 1200){
  for (b in 1:48){
    if (b == 1){
      plotable$CumP[count] <- plotable$Production.median[count]
      count <- count + 1
    } else {
      plotable$CumP[count] <- plotable$CumP[count-1] + abs(plotable$Production.median[count]-plotable$Production.median[count-1])
      count <- count + 1
      
    }
  }
}

fig3.4<- ggplot(plotable,aes(x=Biomass.median,y=CumP,colour=Year))+
  geom_point()+geom_line()+scale_color_manual(values = cols)+
  #scale_colour_gradient(low="lightblue",mid="blue",high="darkblue")+
  theme_classic()+
  theme(legend.position="none")+
  ylab("Cumulative Production (/year)")+xlab("Cumulative Biomass (t/km2)")

plotable %>% group_by(Year) %>% summarise(max(Biomass.median)) %>% as.data.frame()

fig3top <- ggarrange(fig3.1,fig3.2,labels=c('A', 'B'),
                       common.legend = T)

fig3bottom <- ggarrange(fig3.3,fig3.4,labels=c('C', 'D'),common.legend = F)

fig3 <-ggarrange(fig3top,fig3bottom,ncol=1,
                 common.legend = F)
fig3

setwd(plotwd)
ggsave("Ecosystem Indicators.png",fig3,dpi=300,height = unit(6,"in"),width=unit(6,"in"))
}

#### Fisheries Catches and Value (End/Start) Plot (Ready) ----
{
## High Scenario ##
setwd(high_scendir)
files <- list.files()

count <- 1
for (file_count in 1:n_distinct(files)){
  if (files[file_count]!= "mc_input"){
    setwd(paste(high_scendir,"/",files[file_count],sep=""))
  
    getwd()
    
    c <- read.csv("landings_annual.csv")
    v <- read.csv("value-fleet-group_annual.csv")

    if (count ==1){
      cat <- array(0,dim=c(dim(c),n_distinct(files)-1))
      val <- array(0,dim=c(dim(v),n_distinct(files)-1))
      count <- 2
    }
    
    cat[,,file_count-1] <- as.matrix(c)
    val[,,file_count-1] <- as.matrix(v)
  }
}

number <- n_fisher * dim(cat)[1] * 2
res <- data.frame(Fleet <- rep("",number),
                  Value <- rep(0,number),
                  Set <- rep(0,number),
                  Type <- rep("",number))


colnames(res) <- c("Fleet","Value","Set","Type")


count <- 1
for (set in 1:dim(cat)[3]){
  for (type in 1:2){
    if (type == 1){

      catch <- data.frame(cat[,,set])
      colnames(catch) <- c("Year","Fleet","Group","Value")
      
      agg_catch <- aggregate(catch$Value,by=list(catch$Year,catch$Fleet),FUN="sum")
      
      for (fish in 1:n_fisher){
        sub_agg <- agg_catch %>% filter(agg_catch$Group.2==fish)
        
        res$Set[count] <- set
        res$Type[count] <- "Landings"
        res$Fleet[count] <- fleet$Fleet[fish]
        res$Value[count] <- sub_agg$x[n_year]/sub_agg$x[1]
        count <- count + 1
      }
    } else {
      value <- data.frame(val[,,set])
      colnames(value) <- c("Year","Fleet","Group","Value")
      
      agg_val <- aggregate(value$Value,by=list(value$Year,value$Fleet),FUN="sum")
      
      for (fish in 1:n_fisher){
        sub_agg <- agg_val %>% filter(agg_val$Group.2==fish)
        
        res$Set[count] <- set
        res$Type[count] <- "Value"
        res$Fleet[count] <- fleet$Fleet[fish]
        res$Value[count] <- sub_agg$x[n_year]/sub_agg$x[1]
        count <- count + 1
      }
    }
  }
}

res<-res[1:count-1,]

dat_high <- merge(res,fleet,by="Fleet") %>% select(c(colnames(res),"Macrogroup"))

dat_high$Scenario <- c("High Predator")


## Low Scenario ##
setwd(low_scendir)
files <- list.files()

count <- 1
for (file_count in 1:n_distinct(files)){
  if (files[file_count]!= "mc_input"){
    setwd(paste(low_scendir,"/",files[file_count],sep=""))
    
    getwd()
    
    c <- read.csv("landings_annual.csv")
    v <- read.csv("value-fleet-group_annual.csv")
    
    if (count ==1){
      cat <- array(0,dim=c(dim(c),n_distinct(files)-1))
      val <- array(0,dim=c(dim(v),n_distinct(files)-1))
      count <- 2
    }
    
    cat[,,file_count-1] <- as.matrix(c)
    val[,,file_count-1] <- as.matrix(v)
  }
}

number <- n_fisher * dim(cat)[1] * 2
res <- data.frame(Fleet <- rep("",number),
                  Value <- rep(0,number),
                  Set <- rep(0,number),
                  Type <- rep("",number))


colnames(res) <- c("Fleet","Value","Set","Type")


count <- 1
for (set in 1:dim(cat)[3]){
  for (type in 1:2){
    if (type == 1){
      
      catch <- data.frame(cat[,,set])
      colnames(catch) <- c("Year","Fleet","Group","Value")
      
      agg_catch <- aggregate(catch$Value,by=list(catch$Year,catch$Fleet),FUN="sum")
      
      for (fish in 1:n_fisher){
        sub_agg <- agg_catch %>% filter(agg_catch$Group.2==fish)
        
        res$Set[count] <- set
        res$Type[count] <- "Landings"
        res$Fleet[count] <- fleet$Fleet[fish]
        res$Value[count] <- sub_agg$x[n_year]/sub_agg$x[1]
        count <- count + 1
      }
    } else {
      value <- data.frame(val[,,set])
      colnames(value) <- c("Year","Fleet","Group","Value")
      
      agg_val <- aggregate(value$Value,by=list(value$Year,value$Fleet),FUN="sum")
      
      for (fish in 1:n_fisher){
        sub_agg <- agg_val %>% filter(agg_val$Group.2==fish)
        
        res$Set[count] <- set
        res$Type[count] <- "Value"
        res$Fleet[count] <- fleet$Fleet[fish]
        res$Value[count] <- sub_agg$x[n_year]/sub_agg$x[1]
        count <- count + 1
      }
    }
  }
}

res<-res[1:count-1,]

dat_low <- merge(res,fleet,by="Fleet") %>% select(c(colnames(res),"Macrogroup"))

dat_low$Scenario <- c("Low Predator")

## Mean Scenario ##
setwd(mean_scendir)
files <- list.files()

count <- 1
for (file_count in 1:n_distinct(files)){
  if (files[file_count]!= "mc_input"){
    setwd(paste(mean_scendir,"/",files[file_count],sep=""))
    
    getwd()
    
    c <- read.csv("landings_annual.csv")
    v <- read.csv("value-fleet-group_annual.csv")
    
    if (count ==1){
      cat <- array(0,dim=c(dim(c),n_distinct(files)-1))
      val <- array(0,dim=c(dim(v),n_distinct(files)-1))
      count <- 2
    }
    
    cat[,,file_count-1] <- as.matrix(c)
    val[,,file_count-1] <- as.matrix(v)
  }
}

number <- n_fisher * dim(cat)[1] * 2
res <- data.frame(Fleet <- rep("",number),
                  Value <- rep(0,number),
                  Set <- rep(0,number),
                  Type <- rep("",number))


colnames(res) <- c("Fleet","Value","Set","Type")


count <- 1
for (set in 1:dim(cat)[3]){
  for (type in 1:2){
    if (type == 1){
      
      catch <- data.frame(cat[,,set])
      colnames(catch) <- c("Year","Fleet","Group","Value")
      
      agg_catch <- aggregate(catch$Value,by=list(catch$Year,catch$Fleet),FUN="sum")
      
      for (fish in 1:n_fisher){
        sub_agg <- agg_catch %>% filter(agg_catch$Group.2==fish)
        
        res$Set[count] <- set
        res$Type[count] <- "Landings"
        res$Fleet[count] <- fleet$Fleet[fish]
        res$Value[count] <- sub_agg$x[n_year]/sub_agg$x[1]
        count <- count + 1
      }
    } else {
      value <- data.frame(val[,,set])
      colnames(value) <- c("Year","Fleet","Group","Value")
      
      agg_val <- aggregate(value$Value,by=list(value$Year,value$Fleet),FUN="sum")
      
      for (fish in 1:n_fisher){
        sub_agg <- agg_val %>% filter(agg_val$Group.2==fish)
        
        res$Set[count] <- set
        res$Type[count] <- "Value"
        res$Fleet[count] <- fleet$Fleet[fish]
        res$Value[count] <- sub_agg$x[n_year]/sub_agg$x[1]
        count <- count + 1
      }
    }
  }
}

res<-res[1:count-1,]

dat_mean <- merge(res,fleet,by="Fleet") %>% select(c(colnames(res),"Macrogroup"))

dat_mean$Scenario <- c("Mean Predator")

## SCAN Scenario ##
setwd(SCAN_scendir)
files <- list.files()

count <- 1
for (file_count in 1:n_distinct(files)){
  if (files[file_count]!= "mc_input"){
    setwd(paste(SCAN_scendir,"/",files[file_count],sep=""))
    
    getwd()
    
    c <- read.csv("landings_annual.csv")
    v <- read.csv("value-fleet-group_annual.csv")
    
    if (count ==1){
      cat <- array(0,dim=c(dim(c),n_distinct(files)-1))
      val <- array(0,dim=c(dim(v),n_distinct(files)-1))
      count <- 2
    }
    
    cat[,,file_count-1] <- as.matrix(c)
    val[,,file_count-1] <- as.matrix(v)
  }
}

number <- n_fisher * dim(cat)[1] * 2
res <- data.frame(Fleet <- rep("",number),
                  Value <- rep(0,number),
                  Set <- rep(0,number),
                  Type <- rep("",number))


colnames(res) <- c("Fleet","Value","Set","Type")


count <- 1
for (set in 1:dim(cat)[3]){
  for (type in 1:2){
    if (type == 1){
      
      catch <- data.frame(cat[,,set])
      colnames(catch) <- c("Year","Fleet","Group","Value")
      
      agg_catch <- aggregate(catch$Value,by=list(catch$Year,catch$Fleet),FUN="sum")
      
      for (fish in 1:n_fisher){
        sub_agg <- agg_catch %>% filter(agg_catch$Group.2==fish)
        
        res$Set[count] <- set
        res$Type[count] <- "Landings"
        res$Fleet[count] <- fleet$Fleet[fish]
        res$Value[count] <- sub_agg$x[n_year]/sub_agg$x[1]
        count <- count + 1
      }
    } else {
      value <- data.frame(val[,,set])
      colnames(value) <- c("Year","Fleet","Group","Value")
      
      agg_val <- aggregate(value$Value,by=list(value$Year,value$Fleet),FUN="sum")
      
      for (fish in 1:n_fisher){
        sub_agg <- agg_val %>% filter(agg_val$Group.2==fish)
        
        res$Set[count] <- set
        res$Type[count] <- "Value"
        res$Fleet[count] <- fleet$Fleet[fish]
        res$Value[count] <- sub_agg$x[n_year]/sub_agg$x[1]
        count <- count + 1
      }
    }
  }
}

res<-res[1:count-1,]

dat_SCAN <- merge(res,fleet,by="Fleet") %>% select(c(colnames(res),"Macrogroup"))

dat_SCAN$Scenario <- c("SCAN Predator")



all_dat <- rbind(dat_high,dat_low,dat_mean,dat_SCAN)
cols<-tetradic("blue",plot=F)
cols[3] <- "firebrick"
cols[2] <- "black"
cols[4] <- "gold"


all_day <- all_dat

all_day$Fleet <- factor(all_day$Fleet,levels=rev(c("Beam Trawl","Demersal Trawl & Seine","Dredge","Nephrops Trawl","Sandeel Trawl","Shrimp Trawls","Drift and Fixed Nets","Hooked Gears","Pots","Shellfish Gear","Pelagic Trawl","Other Gear")))
all_day$Scenario <- factor(all_day$Scenario,levels=rev(c("SCAN Predator","Low Predator","Mean Predator","High Predator")))


land <- all_day %>% filter(Type %in% "Landings")


all_day %>% group_by(Fleet,Scenario,Type) %>% summarize(mean(c(Value))) %>% data.frame()

fig2<-ggplot(all_day)+
  geom_boxplot(aes(x=Fleet,y=Value,fill=Macrogroup),alpha=0.5,outlier.alpha=0.2)+
  scale_fill_manual(values=c(cols))+
  theme_bw()+
  facet_grid(Scenario~Type)+
  geom_hline(yintercept = 1)+
  coord_flip()+
  ylab("Relative Change (2014/1990)")+
  xlab("")+
  theme(legend.position = "top",legend.title = element_blank(),panel.grid = element_blank(),text=element_text(size=12))
fig2

test <- all_day[all_day$Type=="Landings",]
test2 <- aggregate(test$Value,by=list(test$Fleet,test$Scenario),FUN="mean")

setwd(plotwd)
ggsave("Landings and Value Relative Change.png",fig2,height=unit(7,"in"),width=unit(8,"in"))

}

land <- all_day %>% filter(Type == "Landings")
m_land <- land %>% group_by(Fleet,Scenario) %>% summarize(mean(Value)) %>% data.frame()

s_land <- land %>% group_by(Fleet,Scenario) %>% summarize(sd(Value)) %>% data.frame()
#### BMSY and FMSY Plot (Ready)----
{
fmort <- read.csv("D:/North Sea Ecosystem Model/Stock Assessment Trends/Fishing Mortality.csv")

fmort <- fmort[-26,]
bench <- read.csv("D:/North Sea Ecosystem Model/Stock Assessment Trends/Current Fishery Benchmarks.csv")
setwd(mean_scendir)
files <- list.files()
biom <- array(0,dim=c(n_year,n_spec-1,n_distinct(files)-1))

for (file_count in 1:n_distinct(files)){
  if (files[file_count] != "mc_input"){
    setwd(paste(mean_scendir,"/",files[file_count],sep=""))
    dat <- read.csv("biomass_annual.csv")
    biom[,,file_count-1] <- as.matrix(dat)
  }
}

biom_df <- data.frame()


pb <- txtProgressBar(min=0,max=dim(biom)[3],style=3)
for (set in 1:dim(biom)[3]){
  sub <- as.data.frame(biom[,,set])
  colnames(sub) <- c("Year", species$Species[1:dim(biom)[2]-1])

  
  pivot <- sub %>% pivot_longer(sub,cols = c(`Minke whales`,`Harbour Porpoise`,`White-beaked dolphin`,`Grey Seal`,`Harbour Seal`,Auks,`Fulmars and Petrels`,Gulls,Gannets,Elasmobranchs,`Cod (Adult)`,`Cod (Juvenile, 0-2; 0-40 cm)`,`Whiting (Adult)`,`Whiting (Juvenile 0-1; 0-20 cm)`,`Haddock (Adult)`,`Haddock (Juvenile 0-1; 0-20 cm)`,`Saithe (Adult)`,`Saithe (Juvenile 0-3; 0-40 cm)`,Hake,`Other piscivorous fishes`,`Other planktivorous fishes`,`Norway Pout`,`Herring (Adult)`,`Herring (Juvenile 0-1)`,`Sprat (Adult)`,`Sprat (Juvenile 0-2; 0-10 cm)`,`Horse Mackerel`,Mackerel,`Sandeel (Adult)`,`Sandeel (Juvenile 0-1; 0-10cm)`,`Other flatfishes`,Dab,Turbot,Plaice,Sole,`Larval fishes`,Cephalopods,`Sessile epifauna`,Echinoderms,`Norway Lobster`,`Other Benthic Invertebrates`,`Carnivorous zooplankton`,`Herbivorous zooplankton`,Microzooplankton,`Gelatinous zooplankton`,`Benthic crustaceans`,`Benthic meiofauna`,Phytoplankton),names_to = c("Species"))
  
  pivot$Set <- set
  biom_df <- rbind(biom_df,pivot)
  setTxtProgressBar(pb,set)
}

avg_biom <- biom_df %>% group_by(Year,Species) %>% 
  summarise(median(c(value))) %>% 
  rename(Average = `median(c(value))`)

sd_biom <- biom_df %>% group_by(Year,Species) %>% 
  summarise(sd(c(value))) %>% 
  rename(SD = `sd(c(value))`)

data <- merge(avg_biom,sd_biom,by=c("Year","Species"))

## Cod ##
dat <- data %>%
  filter(Species %in% c("Cod (Adult)","Cod (Juvenile, 0-2; 0-40 cm")) %>%
  select(c("Year","Species","Average","SD"))

agg_cod_avg <- aggregate(dat$Average,by=list(dat$Year),FUN="sum")
agg_cod_sd <- aggregate(dat$SD,by=list(dat$Year),FUN="sum")

cod <- merge(agg_cod_avg,agg_cod_sd,by="Group.1")
colnames(cod) <- c("Year","Average","SD")
cod$Species <- c("Cod")

cod <- cod[,c("Year","Species","Average","SD")]

## Whiting ##
dat <- data %>%
  filter(Species %in% c("Whiting (Adult)","Whiting (Juvenile 0-1; 0-20 cm)")) %>%
  select(c("Year","Species","Average","SD"))

agg_whit_avg <- aggregate(dat$Average,by=list(dat$Year),FUN="sum")
agg_whit_sd <- aggregate(dat$SD,by=list(dat$Year),FUN="sum")

whiting <- merge(agg_whit_avg,agg_whit_sd,by="Group.1")
colnames(whiting) <- c("Year","Average","SD")
whiting$Species <- c("Whiting")
whiting <- whiting[,c("Year","Species","Average","SD")]


## Haddock ##
dat <- data %>%
  filter(Species %in% c("Haddock (Adult)","Haddock (Juvenile 0-1; 0-20 cm)")) %>%
  select(c("Year","Species","Average","SD"))

agg_hadd_avg <- aggregate(dat$Average,by=list(dat$Year),FUN="sum")
agg_hadd_sd <- aggregate(dat$SD,by=list(dat$Year),FUN="sum")

haddock <- merge(agg_hadd_avg,agg_hadd_sd,by="Group.1")
colnames(haddock) <- c("Year","Average","SD")
haddock$Species <- c("Haddock")

haddock <- haddock[,c("Year","Species","Average","SD")]

## Herring ##
dat <- data %>%
  filter(Species %in% c("Herring (Adult)","Herring (Juvenile 0-1)")) %>%
  select(c("Year","Species","Average","SD"))

agg_herr_avg <- aggregate(dat$Average,by=list(dat$Year),FUN="sum")
agg_herr_sd <- aggregate(dat$SD,by=list(dat$Year),FUN="sum")

herring <- merge(agg_herr_avg,agg_herr_sd,by="Group.1")
colnames(herring) <- c("Year","Average","SD")
herring$Species <- c("Herring")

herring <- herring[,c("Year","Species","Average","SD")]


## Hake ##
hake <- data %>%
  filter(Species %in% c("Hake")) %>%
  select(c("Year","Species","Average","SD"))

## Turbot ##
turbot <- data %>%
  filter(Species %in% c("Turbot")) %>%
  select(c("Year","Species","Average","SD"))

## Plaice ##
plaice <- data %>%
  filter(Species %in% c("Plaice")) %>%
  select(c("Year","Species","Average","SD"))

## Sole ##
sole <- data %>%
  filter(Species %in% c("Sole")) %>%
  select(c("Year","Species","Average","SD"))

mean_dat <- rbind(cod,whiting,haddock,herring,hake,turbot,plaice,sole)


for (a in 1:dim(mean_dat)[1]){
  for (year in 1:dim(fmort)[1]){
    for (spec in 2:dim(fmort)[2]){
      if (fmort[year,1]==mean_dat$Year[a] && colnames(fmort)[spec]==mean_dat$Species[a]){
        mean_dat$Fmort[a] <- fmort[year,spec]
      }
    }
  }
}

for (a in 1:dim(mean_dat)[1]){
  for (spec in 1:dim(bench)[1]){
    if (bench$Species[spec]==mean_dat$Species[a]){
      mean_dat$BMSY[a] <- bench$BMSY[spec]
      mean_dat$FMSY[a] <- bench$F.MSY[spec]
    }
  }
}

mean_dat$B_BMSY <- mean_dat$Average/mean_dat$BMSY
mean_dat$F_FMSY <- mean_dat$Fmort/mean_dat$FMSY

mean_dat$Scenario <- c("Mean Predator")

## Low Scenario ##
setwd(low_scendir)
files <- list.files()
biom <- array(0,dim=c(n_year,n_spec-1,n_distinct(files)-1))

for (file_count in 1:n_distinct(files)){
  if (files[file_count] != "mc_input"){
    setwd(paste(low_scendir,"/",files[file_count],sep=""))
    dat <- read.csv("biomass_annual.csv")
    biom[,,file_count-1] <- as.matrix(dat)
  }
}

biom_df <- data.frame()


pb <- txtProgressBar(min=0,max=dim(biom)[3],style=3)
for (set in 1:dim(biom)[3]){
  sub <- as.data.frame(biom[,,set])
  colnames(sub) <- c("Year", species$Species[1:dim(biom)[2]-1])
  
  
  pivot <- sub %>% pivot_longer(sub,cols = c(`Minke whales`,`Harbour Porpoise`,`White-beaked dolphin`,`Grey Seal`,`Harbour Seal`,Auks,`Fulmars and Petrels`,Gulls,Gannets,Elasmobranchs,`Cod (Adult)`,`Cod (Juvenile, 0-2; 0-40 cm)`,`Whiting (Adult)`,`Whiting (Juvenile 0-1; 0-20 cm)`,`Haddock (Adult)`,`Haddock (Juvenile 0-1; 0-20 cm)`,`Saithe (Adult)`,`Saithe (Juvenile 0-3; 0-40 cm)`,Hake,`Other piscivorous fishes`,`Other planktivorous fishes`,`Norway Pout`,`Herring (Adult)`,`Herring (Juvenile 0-1)`,`Sprat (Adult)`,`Sprat (Juvenile 0-2; 0-10 cm)`,`Horse Mackerel`,Mackerel,`Sandeel (Adult)`,`Sandeel (Juvenile 0-1; 0-10cm)`,`Other flatfishes`,Dab,Turbot,Plaice,Sole,`Larval fishes`,Cephalopods,`Sessile epifauna`,Echinoderms,`Norway Lobster`,`Other Benthic Invertebrates`,`Carnivorous zooplankton`,`Herbivorous zooplankton`,Microzooplankton,`Gelatinous zooplankton`,`Benthic crustaceans`,`Benthic meiofauna`,Phytoplankton),names_to = c("Species"))
  
  pivot$Set <- set
  biom_df <- rbind(biom_df,pivot)
  setTxtProgressBar(pb,set)
}

avg_biom <- biom_df %>% group_by(Year,Species) %>% 
  summarise(median(c(value))) %>% 
  rename(Average = `median(c(value))`)

sd_biom <- biom_df %>% group_by(Year,Species) %>% 
  summarise(sd(c(value))) %>% 
  rename(SD = `sd(c(value))`)

data <- merge(avg_biom,sd_biom,by=c("Year","Species"))

## Cod ##
dat <- data %>%
  filter(Species %in% c("Cod (Adult)","Cod (Juvenile, 0-2; 0-40 cm")) %>%
  select(c("Year","Species","Average","SD"))

agg_cod_avg <- aggregate(dat$Average,by=list(dat$Year),FUN="sum")
agg_cod_sd <- aggregate(dat$SD,by=list(dat$Year),FUN="sum")

cod <- merge(agg_cod_avg,agg_cod_sd,by="Group.1")
colnames(cod) <- c("Year","Average","SD")
cod$Species <- c("Cod")

cod <- cod[,c("Year","Species","Average","SD")]

## Whiting ##
dat <- data %>%
  filter(Species %in% c("Whiting (Adult)","Whiting (Juvenile 0-1; 0-20 cm)")) %>%
  select(c("Year","Species","Average","SD"))

agg_whit_avg <- aggregate(dat$Average,by=list(dat$Year),FUN="sum")
agg_whit_sd <- aggregate(dat$SD,by=list(dat$Year),FUN="sum")

whiting <- merge(agg_whit_avg,agg_whit_sd,by="Group.1")
colnames(whiting) <- c("Year","Average","SD")
whiting$Species <- c("Whiting")
whiting <- whiting[,c("Year","Species","Average","SD")]


## Haddock ##
dat <- data %>%
  filter(Species %in% c("Haddock (Adult)","Haddock (Juvenile 0-1; 0-20 cm)")) %>%
  select(c("Year","Species","Average","SD"))

agg_hadd_avg <- aggregate(dat$Average,by=list(dat$Year),FUN="sum")
agg_hadd_sd <- aggregate(dat$SD,by=list(dat$Year),FUN="sum")

haddock <- merge(agg_hadd_avg,agg_hadd_sd,by="Group.1")
colnames(haddock) <- c("Year","Average","SD")
haddock$Species <- c("Haddock")

haddock <- haddock[,c("Year","Species","Average","SD")]

## Herring ##
dat <- data %>%
  filter(Species %in% c("Herring (Adult)","Herring (Juvenile 0-1)")) %>%
  select(c("Year","Species","Average","SD"))

agg_herr_avg <- aggregate(dat$Average,by=list(dat$Year),FUN="sum")
agg_herr_sd <- aggregate(dat$SD,by=list(dat$Year),FUN="sum")

herring <- merge(agg_herr_avg,agg_herr_sd,by="Group.1")
colnames(herring) <- c("Year","Average","SD")
herring$Species <- c("Herring")

herring <- herring[,c("Year","Species","Average","SD")]


## Hake ##
hake <- data %>%
  filter(Species %in% c("Hake")) %>%
  select(c("Year","Species","Average","SD"))

## Turbot ##
turbot <- data %>%
  filter(Species %in% c("Turbot")) %>%
  select(c("Year","Species","Average","SD"))

## Plaice ##
plaice <- data %>%
  filter(Species %in% c("Plaice")) %>%
  select(c("Year","Species","Average","SD"))

## Sole ##
sole <- data %>%
  filter(Species %in% c("Sole")) %>%
  select(c("Year","Species","Average","SD"))

low_dat <- rbind(cod,whiting,haddock,herring,hake,turbot,plaice,sole)


for (a in 1:dim(low_dat)[1]){
  for (year in 1:dim(fmort)[1]){
    for (spec in 2:dim(fmort)[2]){
      if (fmort[year,1]==low_dat$Year[a] && colnames(fmort)[spec]==low_dat$Species[a]){
        low_dat$Fmort[a] <- fmort[year,spec]
      }
    }
  }
}

for (a in 1:dim(low_dat)[1]){
  for (spec in 1:dim(bench)[1]){
    if (bench$Species[spec]==low_dat$Species[a]){
      low_dat$BMSY[a] <- bench$BMSY[spec]
      low_dat$FMSY[a] <- bench$F.MSY[spec]
    }
  }
}

low_dat$B_BMSY <- low_dat$Average/low_dat$BMSY
low_dat$F_FMSY <- low_dat$Fmort/low_dat$FMSY

low_dat$Scenario <- c("Low Predator")


## High Scenario ##
setwd(high_scendir)
files <- list.files()
biom <- array(0,dim=c(n_year,n_spec-1,n_distinct(files)-1))

for (file_count in 1:n_distinct(files)){
  if (files[file_count] != "mc_input"){
    setwd(paste(high_scendir,"/",files[file_count],sep=""))
    dat <- read.csv("biomass_annual.csv")
    biom[,,file_count-1] <- as.matrix(dat)
  }
}

biom_df <- data.frame()


pb <- txtProgressBar(min=0,max=dim(biom)[3],style=3)
for (set in 1:dim(biom)[3]){
  sub <- as.data.frame(biom[,,set])
  colnames(sub) <- c("Year", species$Species[1:dim(biom)[2]-1])
  
  
  pivot <- sub %>% pivot_longer(sub,cols = c(`Minke whales`,`Harbour Porpoise`,`White-beaked dolphin`,`Grey Seal`,`Harbour Seal`,Auks,`Fulmars and Petrels`,Gulls,Gannets,Elasmobranchs,`Cod (Adult)`,`Cod (Juvenile, 0-2; 0-40 cm)`,`Whiting (Adult)`,`Whiting (Juvenile 0-1; 0-20 cm)`,`Haddock (Adult)`,`Haddock (Juvenile 0-1; 0-20 cm)`,`Saithe (Adult)`,`Saithe (Juvenile 0-3; 0-40 cm)`,Hake,`Other piscivorous fishes`,`Other planktivorous fishes`,`Norway Pout`,`Herring (Adult)`,`Herring (Juvenile 0-1)`,`Sprat (Adult)`,`Sprat (Juvenile 0-2; 0-10 cm)`,`Horse Mackerel`,Mackerel,`Sandeel (Adult)`,`Sandeel (Juvenile 0-1; 0-10cm)`,`Other flatfishes`,Dab,Turbot,Plaice,Sole,`Larval fishes`,Cephalopods,`Sessile epifauna`,Echinoderms,`Norway Lobster`,`Other Benthic Invertebrates`,`Carnivorous zooplankton`,`Herbivorous zooplankton`,Microzooplankton,`Gelatinous zooplankton`,`Benthic crustaceans`,`Benthic meiofauna`,Phytoplankton),names_to = c("Species"))
  
  pivot$Set <- set
  biom_df <- rbind(biom_df,pivot)
  setTxtProgressBar(pb,set)
}

avg_biom <- biom_df %>% group_by(Year,Species) %>% 
  summarise(median(c(value))) %>% 
  rename(Average = `median(c(value))`)

sd_biom <- biom_df %>% group_by(Year,Species) %>% 
  summarise(sd(c(value))) %>% 
  rename(SD = `sd(c(value))`)

data <- merge(avg_biom,sd_biom,by=c("Year","Species"))

## Cod ##
dat <- data %>%
  filter(Species %in% c("Cod (Adult)","Cod (Juvenile, 0-2; 0-40 cm")) %>%
  select(c("Year","Species","Average","SD"))

agg_cod_avg <- aggregate(dat$Average,by=list(dat$Year),FUN="sum")
agg_cod_sd <- aggregate(dat$SD,by=list(dat$Year),FUN="sum")

cod <- merge(agg_cod_avg,agg_cod_sd,by="Group.1")
colnames(cod) <- c("Year","Average","SD")
cod$Species <- c("Cod")

cod <- cod[,c("Year","Species","Average","SD")]

## Whiting ##
dat <- data %>%
  filter(Species %in% c("Whiting (Adult)","Whiting (Juvenile 0-1; 0-20 cm)")) %>%
  select(c("Year","Species","Average","SD"))

agg_whit_avg <- aggregate(dat$Average,by=list(dat$Year),FUN="sum")
agg_whit_sd <- aggregate(dat$SD,by=list(dat$Year),FUN="sum")

whiting <- merge(agg_whit_avg,agg_whit_sd,by="Group.1")
colnames(whiting) <- c("Year","Average","SD")
whiting$Species <- c("Whiting")
whiting <- whiting[,c("Year","Species","Average","SD")]


## Haddock ##
dat <- data %>%
  filter(Species %in% c("Haddock (Adult)","Haddock (Juvenile 0-1; 0-20 cm)")) %>%
  select(c("Year","Species","Average","SD"))

agg_hadd_avg <- aggregate(dat$Average,by=list(dat$Year),FUN="sum")
agg_hadd_sd <- aggregate(dat$SD,by=list(dat$Year),FUN="sum")

haddock <- merge(agg_hadd_avg,agg_hadd_sd,by="Group.1")
colnames(haddock) <- c("Year","Average","SD")
haddock$Species <- c("Haddock")

haddock <- haddock[,c("Year","Species","Average","SD")]

## Herring ##
dat <- data %>%
  filter(Species %in% c("Herring (Adult)","Herring (Juvenile 0-1)")) %>%
  select(c("Year","Species","Average","SD"))

agg_herr_avg <- aggregate(dat$Average,by=list(dat$Year),FUN="sum")
agg_herr_sd <- aggregate(dat$SD,by=list(dat$Year),FUN="sum")

herring <- merge(agg_herr_avg,agg_herr_sd,by="Group.1")
colnames(herring) <- c("Year","Average","SD")
herring$Species <- c("Herring")

herring <- herring[,c("Year","Species","Average","SD")]


## Hake ##
hake <- data %>%
  filter(Species %in% c("Hake")) %>%
  select(c("Year","Species","Average","SD"))

## Turbot ##
turbot <- data %>%
  filter(Species %in% c("Turbot")) %>%
  select(c("Year","Species","Average","SD"))

## Plaice ##
plaice <- data %>%
  filter(Species %in% c("Plaice")) %>%
  select(c("Year","Species","Average","SD"))

## Sole ##
sole <- data %>%
  filter(Species %in% c("Sole")) %>%
  select(c("Year","Species","Average","SD"))

high_dat <- rbind(cod,whiting,haddock,herring,hake,turbot,plaice,sole)


for (a in 1:dim(high_dat)[1]){
  for (year in 1:dim(fmort)[1]){
    for (spec in 2:dim(fmort)[2]){
      if (fmort[year,1]==high_dat$Year[a] && colnames(fmort)[spec]==high_dat$Species[a]){
        high_dat$Fmort[a] <- fmort[year,spec]
      }
    }
  }
}

for (a in 1:dim(high_dat)[1]){
  for (spec in 1:dim(bench)[1]){
    if (bench$Species[spec]==high_dat$Species[a]){
      high_dat$BMSY[a] <- bench$BMSY[spec]
      high_dat$FMSY[a] <- bench$F.MSY[spec]
    }
  }
}

high_dat$B_BMSY <- high_dat$Average/high_dat$BMSY
high_dat$F_FMSY <- high_dat$Fmort/high_dat$FMSY

high_dat$Scenario <- c("High Predator")



## SCAN Scenario ##
setwd(SCAN_scendir)
files <- list.files()
biom <- array(0,dim=c(n_year,n_spec-1,n_distinct(files)-1))

for (file_count in 1:n_distinct(files)){
  if (files[file_count] != "mc_input"){
    setwd(paste(SCAN_scendir,"/",files[file_count],sep=""))
    dat <- read.csv("biomass_annual.csv")
    biom[,,file_count-1] <- as.matrix(dat)
  }
}

biom_df <- data.frame()


pb <- txtProgressBar(min=0,max=dim(biom)[3],style=3)
for (set in 1:dim(biom)[3]){
  sub <- as.data.frame(biom[,,set])
  colnames(sub) <- c("Year", species$Species[1:dim(biom)[2]-1])
  
  
  pivot <- sub %>% pivot_longer(sub,cols = c(`Minke whales`,`Harbour Porpoise`,`White-beaked dolphin`,`Grey Seal`,`Harbour Seal`,Auks,`Fulmars and Petrels`,Gulls,Gannets,Elasmobranchs,`Cod (Adult)`,`Cod (Juvenile, 0-2; 0-40 cm)`,`Whiting (Adult)`,`Whiting (Juvenile 0-1; 0-20 cm)`,`Haddock (Adult)`,`Haddock (Juvenile 0-1; 0-20 cm)`,`Saithe (Adult)`,`Saithe (Juvenile 0-3; 0-40 cm)`,Hake,`Other piscivorous fishes`,`Other planktivorous fishes`,`Norway Pout`,`Herring (Adult)`,`Herring (Juvenile 0-1)`,`Sprat (Adult)`,`Sprat (Juvenile 0-2; 0-10 cm)`,`Horse Mackerel`,Mackerel,`Sandeel (Adult)`,`Sandeel (Juvenile 0-1; 0-10cm)`,`Other flatfishes`,Dab,Turbot,Plaice,Sole,`Larval fishes`,Cephalopods,`Sessile epifauna`,Echinoderms,`Norway Lobster`,`Other Benthic Invertebrates`,`Carnivorous zooplankton`,`Herbivorous zooplankton`,Microzooplankton,`Gelatinous zooplankton`,`Benthic crustaceans`,`Benthic meiofauna`,Phytoplankton),names_to = c("Species"))
  
  pivot$Set <- set
  biom_df <- rbind(biom_df,pivot)
  setTxtProgressBar(pb,set)
}

avg_biom <- biom_df %>% group_by(Year,Species) %>% 
  summarise(median(c(value))) %>% 
  rename(Average = `median(c(value))`)

sd_biom <- biom_df %>% group_by(Year,Species) %>% 
  summarise(sd(c(value))) %>% 
  rename(SD = `sd(c(value))`)

data <- merge(avg_biom,sd_biom,by=c("Year","Species"))

## Cod ##
dat <- data %>%
  filter(Species %in% c("Cod (Adult)","Cod (Juvenile, 0-2; 0-40 cm")) %>%
  select(c("Year","Species","Average","SD"))

agg_cod_avg <- aggregate(dat$Average,by=list(dat$Year),FUN="sum")
agg_cod_sd <- aggregate(dat$SD,by=list(dat$Year),FUN="sum")

cod <- merge(agg_cod_avg,agg_cod_sd,by="Group.1")
colnames(cod) <- c("Year","Average","SD")
cod$Species <- c("Cod")

cod <- cod[,c("Year","Species","Average","SD")]

## Whiting ##
dat <- data %>%
  filter(Species %in% c("Whiting (Adult)","Whiting (Juvenile 0-1; 0-20 cm)")) %>%
  select(c("Year","Species","Average","SD"))

agg_whit_avg <- aggregate(dat$Average,by=list(dat$Year),FUN="sum")
agg_whit_sd <- aggregate(dat$SD,by=list(dat$Year),FUN="sum")

whiting <- merge(agg_whit_avg,agg_whit_sd,by="Group.1")
colnames(whiting) <- c("Year","Average","SD")
whiting$Species <- c("Whiting")
whiting <- whiting[,c("Year","Species","Average","SD")]


## Haddock ##
dat <- data %>%
  filter(Species %in% c("Haddock (Adult)","Haddock (Juvenile 0-1; 0-20 cm)")) %>%
  select(c("Year","Species","Average","SD"))

agg_hadd_avg <- aggregate(dat$Average,by=list(dat$Year),FUN="sum")
agg_hadd_sd <- aggregate(dat$SD,by=list(dat$Year),FUN="sum")

haddock <- merge(agg_hadd_avg,agg_hadd_sd,by="Group.1")
colnames(haddock) <- c("Year","Average","SD")
haddock$Species <- c("Haddock")

haddock <- haddock[,c("Year","Species","Average","SD")]

## Herring ##
dat <- data %>%
  filter(Species %in% c("Herring (Adult)","Herring (Juvenile 0-1)")) %>%
  select(c("Year","Species","Average","SD"))

agg_herr_avg <- aggregate(dat$Average,by=list(dat$Year),FUN="sum")
agg_herr_sd <- aggregate(dat$SD,by=list(dat$Year),FUN="sum")

herring <- merge(agg_herr_avg,agg_herr_sd,by="Group.1")
colnames(herring) <- c("Year","Average","SD")
herring$Species <- c("Herring")

herring <- herring[,c("Year","Species","Average","SD")]


## Hake ##
hake <- data %>%
  filter(Species %in% c("Hake")) %>%
  select(c("Year","Species","Average","SD"))

## Turbot ##
turbot <- data %>%
  filter(Species %in% c("Turbot")) %>%
  select(c("Year","Species","Average","SD"))

## Plaice ##
plaice <- data %>%
  filter(Species %in% c("Plaice")) %>%
  select(c("Year","Species","Average","SD"))

## Sole ##
sole <- data %>%
  filter(Species %in% c("Sole")) %>%
  select(c("Year","Species","Average","SD"))

SCAN_dat <- rbind(cod,whiting,haddock,herring,hake,turbot,plaice,sole)


for (a in 1:dim(SCAN_dat)[1]){
  for (year in 1:dim(fmort)[1]){
    for (spec in 2:dim(fmort)[2]){
      if (fmort[year,1]==SCAN_dat$Year[a] && colnames(fmort)[spec]==SCAN_dat$Species[a]){
        SCAN_dat$Fmort[a] <- fmort[year,spec]
      }
    }
  }
}

for (a in 1:dim(SCAN_dat)[1]){
  for (spec in 1:dim(bench)[1]){
    if (bench$Species[spec]==SCAN_dat$Species[a]){
      SCAN_dat$BMSY[a] <- bench$BMSY[spec]
      SCAN_dat$FMSY[a] <- bench$F.MSY[spec]
    }
  }
}

SCAN_dat$B_BMSY <- SCAN_dat$Average/SCAN_dat$BMSY
SCAN_dat$F_FMSY <- SCAN_dat$Fmort/SCAN_dat$FMSY

SCAN_dat$Scenario <- c("SCAN Predator")


all_dat <- rbind(mean_dat,low_dat,high_dat,SCAN_dat)

library(colortools)
cols<-tetradic("blue",plot=F)
cols[3] <- "firebrick"
cols[2] <- "black"
cols[4] <- "gold"

all_dat <- all_dat %>% filter(Species != c("Turbot"))
all_dat <- all_dat %>% filter(Species !=c("Plaice"))

## Make Plot ##
fig1 <- ggplot(all_dat)+
  geom_point(aes(x=F_FMSY,y=B_BMSY,shape=Scenario,colour=Scenario,alpha=Year),size=2)+scale_colour_manual(values=c(cols[c(2,1,3,4)]))+
  geom_hline(yintercept = 1,colour="white")+
  geom_vline(xintercept=1,colour="white")+
  theme_classic()+
  facet_wrap(.~Species,ncol=3)+
  scale_alpha_continuous(range = c(0.1,1),breaks=c(1990,2000,2010,2014))+
  theme(text=element_text(size=14,family="serif"),legend.text = element_text(size=12,colour="white"),panel.background = element_rect(fill="transparent",colour=NA),
        plot.background = element_rect(fill="transparent",colour=NA),
        strip.background = element_rect(fill="transparent",colour=NA),
        axis.text = element_text(colour="white"),axis.title = element_text(colour="white"),legend.background = element_rect(fill="transparent",colour=NA),
        axis.line = element_line(colour="white"),axis.ticks = element_line(colour="white"),legend.title = element_text(colour="white"),strip.text = element_text(colour="white"))+
  ylab("B/BMSY")+
  xlab("F/FMSY")+
  ylim(0,3.1)+
  xlim(0,5)

fig1

setwd(plotwd)
ggsave("BMSY and FMSY_2.png",fig1,width=9,height=4,units = c("in"))


all_dat %>% group_by(Species,Year,Scenario) %>% filter(Year == 2014) %>% filter(Species %in% "Sole") %>% data.frame()
}

## Consumption of marine mammals ----
setwd(mean_scendir)
files <- list.files()
biom <- array(0,dim=c(n_year,n_spec-1,n_distinct(files)-1))
cons <- array(0,dim=c(n_year,n_spec-1,n_distinct(files)-1))
for (file_count in 1:n_distinct(files)){
  if (files[file_count] != "mc_input"){
    setwd(paste(mean_scendir,"/",files[file_count],sep=""))
    dat <- read.csv("biomass_annual.csv")
    biom[,,file_count-1] <- as.matrix(dat)
    dat2 <-read.csv("consumption-biomass_annual.csv")
    cons[,,file_count-1] <- as.matrix(dat2)
  }
}

q <- biom*cons
q_df <- data.frame()

pb <- txtProgressBar(min=0,max=dim(q)[3],style=3)
for (set in 1:dim(biom)[3]){
  sub <- as.data.frame(q[,,set])
  colnames(sub) <- c("Year", species$Species[1:dim(q)[2]-1])
  
  
  pivot <- sub %>% pivot_longer(sub,cols = c(`Minke whales`,`Harbour Porpoise`,`White-beaked dolphin`,`Grey Seal`,`Harbour Seal`,Auks,`Fulmars and Petrels`,Gulls,Gannets,Elasmobranchs,`Cod (Adult)`,`Cod (Juvenile, 0-2; 0-40 cm)`,`Whiting (Adult)`,`Whiting (Juvenile 0-1; 0-20 cm)`,`Haddock (Adult)`,`Haddock (Juvenile 0-1; 0-20 cm)`,`Saithe (Adult)`,`Saithe (Juvenile 0-3; 0-40 cm)`,Hake,`Other piscivorous fishes`,`Other planktivorous fishes`,`Norway Pout`,`Herring (Adult)`,`Herring (Juvenile 0-1)`,`Sprat (Adult)`,`Sprat (Juvenile 0-2; 0-10 cm)`,`Horse Mackerel`,Mackerel,`Sandeel (Adult)`,`Sandeel (Juvenile 0-1; 0-10cm)`,`Other flatfishes`,Dab,Turbot,Plaice,Sole,`Larval fishes`,Cephalopods,`Sessile epifauna`,Echinoderms,`Norway Lobster`,`Other Benthic Invertebrates`,`Carnivorous zooplankton`,`Herbivorous zooplankton`,Microzooplankton,`Gelatinous zooplankton`,`Benthic crustaceans`,`Benthic meiofauna`,Phytoplankton),names_to = c("Species"))
  
  pivot$Set <- set
  q_df <- rbind(q_df,pivot)
  setTxtProgressBar(pb,set)
}

bird <- q_df %>% group_by(Year,Species) %>% summarize(mean(c(value))) %>% filter(Species %in% c("Auks","Fulmars and Petrels","Gulls","Gannets")) %>% rename(value = `mean(c(value))`)

summed <- bird %>% group_by(Year) %>% summarize(sum(value)) %>% data.frame()

(summed$sum.value./data_mean$Average)*100

mam <- q_df %>% group_by(Year,Species) %>% summarize(mean(c(value))) %>% filter(Species %in% c("Minke whales","Harbour Porpoise","White-beaked dolphin","Grey Seal","Harbour Seal")) %>% rename(value = `mean(c(value))`)
mam %>% group_by(Year) %>% summarize(sum(value)) %>% data.frame()

## Excess ----


####* Biomass ----
biom <- array(0,dim=c(n_year,n_spec+1,n_sim))

for (file_count in 1:n_sim){
  if (file_count < 10){
    foldername<-paste0(mean_scendir,"/mc_output_trial000",file_count,sep="") 
    if ((dir.exists(foldername))){
      setwd(foldername)
    }
  }
  if (file_count < 100){
    foldername<-paste0(mean_scendir,"/mc_output_trial00",file_count,sep="")
    if ((dir.exists(foldername))){
      setwd(foldername)
    }
  }
  if (file_count < 1000){
    foldername<-paste0(mean_scendir,"/mc_output_trial0",file_count,sep="")
    if ((dir.exists(foldername))){
      setwd(foldername)
    }
  }
  if (file_count < 10000){
    foldername<-paste0(mean_scendir,"/mc_output_trial",file_count,sep="")
    if ((dir.exists(foldername))){
      setwd(foldername)
    }
  }
  
  dat <- read.csv("biomass_annual.csv")
  biom[,,file_count] <- as.matrix(dat)
}

number <- n_year * n_sim * n_spec
biom_df <- data.frame(Set <- rep(0,number),
                      Year <- rep(0,number),
                      Species <- rep("",number),
                      Value <- rep(0,number))
colnames(biom_df) <- c("Set","Year","Species","Value")

count <- 1
for (set in 1:n_sim){
  for (spec in 1:n_spec){
    biom_df$Set[c(count:(count+(n_year-1)))] <- c(set)
    biom_df$Species[c(count:(count+(n_year-1)))] <- c(species[spec,2])
    biom_df$Year[c(count:(count+(n_year-1)))] <- c(biom[,1,set])
    biom_df$Value[c(count:(count+(n_year-1)))] <- c(biom[,spec+1,set])
    count <- count + n_year
  }
}


avg_biom <- aggregate(biom_df$Value,by=list(biom_df$Year,biom_df$Species),FUN="mean")
sd_biom <- aggregate(biom_df$Value,by=list(biom_df$Year,biom_df$Species),FUN="sd")

data <- cbind(avg_biom,sd_biom)

colnames(data) <- c("Year","Species","Average","Year2","Species2","SD")

head(data)

data %>% filter(Species %in% c("White-beaked dolphin")) %>%
  ggplot(aes(x=Year,y=Average,colour=Species))+
  geom_ribbon(aes(xmin=min(Year),xmax=max(Year),ymin=Average-SD,ymax=Average+SD),alpha=0.2)+
  geom_line(size=2)+theme(text = element_text(size=16,family="serif"),axis.text = element_text(size=16,family="serif"))+ylab("Biomass t/km2")+theme_classic()

#### * Biomass (End/Start) ----
biom <- array(0,dim=c(n_year,n_spec,n_sim))

for (file_count in 1:n_sim){
  if (file_count < 10){
    foldername<-paste0(mean_scendir,"/mc_output_trial000",file_count,sep="") 
    if ((dir.exists(foldername))){
      setwd(foldername)
    }
  }
  if (file_count < 100){
    foldername<-paste0(mean_scendir,"/mc_output_trial00",file_count,sep="")
    if ((dir.exists(foldername))){
      setwd(foldername)
    }
  }
  if (file_count < 1000){
    foldername<-paste0(mean_scendir,"/mc_output_trial0",file_count,sep="")
    if ((dir.exists(foldername))){
      setwd(foldername)
    }
  }
  if (file_count < 10000){
    foldername<-paste0(mean_scendir,"/mc_output_trial",file_count,sep="")
    if ((dir.exists(foldername))){
      setwd(foldername)
    }
  }
  
  dat <- read.csv("biomass_annual.csv")
  biom[,,file_count] <- as.matrix(dat[,-1])
}


number <- n_sim * n_spec
biom_es <- data.frame(Set <- rep(0,number),
                      Species <- rep("",number),
                      Macro <- rep("",number),
                      Value <- rep(0,number))
colnames(biom_es) <- c("Set","Species","Macrogroup","Value")

count <- 1
for (set in 1:n_sim){
  for (spec in 1:n_spec){
    biom_es$Set[count] <- set
    biom_es$Species[count] <- species[spec,2]
    biom_es$Macrogroup[count] <- species[spec,4]
    biom_es$Value[count] <- biom[n_year,spec,set]/biom[1,spec,set]
    count <- count + 1
  }
}



head(biom_es)
biom_es %>% filter(Macrogroup %in% c("Groundfishes")) %>%
  ggplot()+geom_hline(yintercept=1)+
  geom_boxplot(aes(x=Species,y=Value,colour=Value),outlier.alpha = 0.1)+facet_wrap(.~Macrogroup)+theme_classic()




####* TL Catch ----
cat("\nTL Catch Calculation\n")

## High Scenario ##
tlc <- array(0,dim=c(n_year,2,n_sim))

for (file_count in 1:n_sim){
  if (file_count < 10){
    foldername<-paste0(high_scendir,"/mc_output_trial000",file_count,sep="") 
    if ((dir.exists(foldername))){
      setwd(foldername)
    }
  }
  if (file_count < 100){
    foldername<-paste0(high_scendir,"/mc_output_trial00",file_count,sep="")
    if ((dir.exists(foldername))){
      setwd(foldername)
    }
  }
  if (file_count < 1000){
    foldername<-paste0(high_scendir,"/mc_output_trial0",file_count,sep="")
    if ((dir.exists(foldername))){
      setwd(foldername)
    }
  }
  if (file_count < 10000){
    foldername<-paste0(high_scendir,"/mc_output_trial",file_count,sep="")
    if ((dir.exists(foldername))){
      setwd(foldername)
    }
  }
  
  dat <- read.csv("tlc_annual.csv")
  tlc[,,file_count] <- as.matrix(dat)
}
number <- n_year * n_sim
tlc_df <- data.frame(Set <- rep(0,number),
                     Year <- rep(0,number),
                     Value <- rep(0,number))
colnames(tlc_df) <- c("Set","Year","Value")

count <- 1
set <-1
for (set in 1:n_sim){
  tlc_df$Set[c(count:(count+(n_year-1)))] <- c(set)
  tlc_df$Year[c(count:(count+(n_year-1)))] <- c(tlc[,1,set])
  tlc_df$Value[c(count:(count+(n_year-1)))] <- c(tlc[,2,set])
  count <- count + n_year
}

avg_tlc <- aggregate(tlc_df$Value,by=list(tlc_df$Year),FUN="mean")
sd_tlc <- aggregate(tlc_df$Value,by=list(tlc_df$Year),FUN="sd")

data_high <- cbind(avg_tlc,sd_tlc)
colnames(data_high) <- c("Year","Average","Year2","SD")
data_high$Scenario <- c("High Predator")



## Mean Scenario ##
tlc <- array(0,dim=c(n_year,2,n_sim))

for (file_count in 1:n_sim){
  if (file_count < 10){
    foldername<-paste0(mean_scendir,"/mc_output_trial000",file_count,sep="") 
    if ((dir.exists(foldername))){
      setwd(foldername)
    }
  }
  if (file_count < 100){
    foldername<-paste0(mean_scendir,"/mc_output_trial00",file_count,sep="")
    if ((dir.exists(foldername))){
      setwd(foldername)
    }
  }
  if (file_count < 1000){
    foldername<-paste0(mean_scendir,"/mc_output_trial0",file_count,sep="")
    if ((dir.exists(foldername))){
      setwd(foldername)
    }
  }
  if (file_count < 10000){
    foldername<-paste0(mean_scendir,"/mc_output_trial",file_count,sep="")
    if ((dir.exists(foldername))){
      setwd(foldername)
    }
  }
  
  dat <- read.csv("tlc_annual.csv")
  tlc[,,file_count] <- as.matrix(dat)
}
number <- n_year * n_sim
tlc_df <- data.frame(Set <- rep(0,number),
                     Year <- rep(0,number),
                     Value <- rep(0,number))
colnames(tlc_df) <- c("Set","Year","Value")

count <- 1
set <-1
for (set in 1:n_sim){
  tlc_df$Set[c(count:(count+(n_year-1)))] <- c(set)
  tlc_df$Year[c(count:(count+(n_year-1)))] <- c(tlc[,1,set])
  tlc_df$Value[c(count:(count+(n_year-1)))] <- c(tlc[,2,set])
  count <- count + n_year
}

avg_tlc <- aggregate(tlc_df$Value,by=list(tlc_df$Year),FUN="mean")
sd_tlc <- aggregate(tlc_df$Value,by=list(tlc_df$Year),FUN="sd")

data_mean <- cbind(avg_tlc,sd_tlc)
colnames(data_mean) <- c("Year","Average","Year2","SD")
data_mean$Scenario <- c("Mean Predator")



## Low Scenario ##
tlc <- array(0,dim=c(n_year,2,n_sim))

for (file_count in 1:n_sim){
  if (file_count < 10){
    foldername<-paste0(low_scendir,"/mc_output_trial000",file_count,sep="") 
    if ((dir.exists(foldername))){
      setwd(foldername)
    }
  }
  if (file_count < 100){
    foldername<-paste0(low_scendir,"/mc_output_trial00",file_count,sep="")
    if ((dir.exists(foldername))){
      setwd(foldername)
    }
  }
  if (file_count < 1000){
    foldername<-paste0(low_scendir,"/mc_output_trial0",file_count,sep="")
    if ((dir.exists(foldername))){
      setwd(foldername)
    }
  }
  if (file_count < 10000){
    foldername<-paste0(low_scendir,"/mc_output_trial",file_count,sep="")
    if ((dir.exists(foldername))){
      setwd(foldername)
    }
  }
  
  dat <- read.csv("tlc_annual.csv")
  tlc[,,file_count] <- as.matrix(dat)
}
number <- n_year * n_sim
tlc_df <- data.frame(Set <- rep(0,number),
                     Year <- rep(0,number),
                     Value <- rep(0,number))
colnames(tlc_df) <- c("Set","Year","Value")

count <- 1
set <-1
for (set in 1:n_sim){
  tlc_df$Set[c(count:(count+(n_year-1)))] <- c(set)
  tlc_df$Year[c(count:(count+(n_year-1)))] <- c(tlc[,1,set])
  tlc_df$Value[c(count:(count+(n_year-1)))] <- c(tlc[,2,set])
  count <- count + n_year
}

avg_tlc <- aggregate(tlc_df$Value,by=list(tlc_df$Year),FUN="mean")
sd_tlc <- aggregate(tlc_df$Value,by=list(tlc_df$Year),FUN="sd")

data_low <- cbind(avg_tlc,sd_tlc)
colnames(data_low) <- c("Year","Average","Year2","SD")
data_low$Scenario <- c("Low Predator")


## SCAN Scenario ##
tlc <- array(0,dim=c(n_year,2,n_sim))

for (file_count in 1:n_sim){
  if (file_count < 10){
    foldername<-paste0(SCAN_scendir,"/mc_output_trial000",file_count,sep="") 
    if ((dir.exists(foldername))){
      setwd(foldername)
    }
  }
  if (file_count < 100){
    foldername<-paste0(SCAN_scendir,"/mc_output_trial00",file_count,sep="")
    if ((dir.exists(foldername))){
      setwd(foldername)
    }
  }
  if (file_count < 1000){
    foldername<-paste0(SCAN_scendir,"/mc_output_trial0",file_count,sep="")
    if ((dir.exists(foldername))){
      setwd(foldername)
    }
  }
  if (file_count < 10000){
    foldername<-paste0(SCAN_scendir,"/mc_output_trial",file_count,sep="")
    if ((dir.exists(foldername))){
      setwd(foldername)
    }
  }
  
  dat <- read.csv("tlc_annual.csv")
  tlc[,,file_count] <- as.matrix(dat)
}
number <- n_year * n_sim
tlc_df <- data.frame(Set <- rep(0,number),
                     Year <- rep(0,number),
                     Value <- rep(0,number))
colnames(tlc_df) <- c("Set","Year","Value")

count <- 1
set <-1
for (set in 1:n_sim){
  tlc_df$Set[c(count:(count+(n_year-1)))] <- c(set)
  tlc_df$Year[c(count:(count+(n_year-1)))] <- c(tlc[,1,set])
  tlc_df$Value[c(count:(count+(n_year-1)))] <- c(tlc[,2,set])
  count <- count + n_year
}

avg_tlc <- aggregate(tlc_df$Value,by=list(tlc_df$Year),FUN="mean")
sd_tlc <- aggregate(tlc_df$Value,by=list(tlc_df$Year),FUN="sd")

data_SCAN <- cbind(avg_tlc,sd_tlc)
colnames(data_SCAN) <- c("Year","Average","Year2","SD")
data_SCAN$Scenario <- c("SCAN Predator")

all_dat <-rbind(data_high,data_mean,data_low,data_SCAN)
all_dat$Scenario <- factor(all_dat$Scenario,levels=rev(c("SCAN Predator","Low Predator","Mean Predator","High Predator")))


fig3.2<-ggplot(all_dat,aes(x=Year,y=Average,colour=Scenario))+scale_colour_manual(values=c(cols[1],cols[4],cols[3],cols[2]))+
  geom_ribbon(aes(xmin=min(Year),xmax=max(Year),ymin=Average-SD,ymax=Average+SD,fill=Scenario),alpha=0.2,linetype="dotted")+scale_fill_manual(values=c(cols[1],cols[4],cols[3],cols[2]))+
  geom_line(size=1.5)+theme(text = element_text(size=16,family="serif"),axis.text = element_text(size=16,family="serif"))+theme_classic()+ylab("TL Catch")+xlab("")+ylim(3,4.5)

