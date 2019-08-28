library(stringr)
library(dplyr)
setwd('~/overflow_dropbox/safety_in_numbers_power_law/england_LAs/')

if(file.exists('~/overflow_dropbox/all_england_data_2.Rds')){
  x <- readRDS('~/overflow_dropbox/all_england_data_2.Rds')
}else{
  process_for_rahul <- F
  process_for_rachel <- F
  
  x <- readRDS('~/overflow_dropbox/injury_predictions/accidents_vehicles_casualties_05-15.rds')
  if(process_for_rahul){
    x <- x[sapply(x$local_authority_.highway.,function(x)substring(as.character(x),1,1)%in%c('E','S')),]
    saveRDS(x,'~/overflow_dropbox/all_england_scotland_data.Rds')
  }else{
    x <- x[sapply(x$local_authority_.highway.,function(x)substring(as.character(x),1,1)%in%c('E')),]
    saveRDS(x,'~/overflow_dropbox/all_england_data.Rds')
  }
  x <- x[,c(1,2,8,9,10,11,14,15,16,17,18,31,34,55,56,60,66)]
  # PREPARE VARIABLES
  x <- dplyr::rename(x,  cas_severity = casualty_severity )
  x$cas_severity <- recode(x$cas_severity, '1'="Fatal",'2'="Serious", '3'="Slight")
  
  # DATE
  td <- str_split(string = x$date, pattern = "/",n = 3, simplify = TRUE)
  x$year <- td[,3]
  rm(td)
  
  # rename ROAD CLASS
  x$st_road_class <-  recode(x$x1st_road_class,'1'=1,'2' = 1, '3'=2, '4'=3,'5'=3, '6'=3) #1 unchanged
  x$roadtype <- recode(x$st_road_class, '1'="Motorway/A(M)", '2'="A", '3'="B, C, Unclassified") 
  x <- x[,-c(6,9,19)]
  
  # VEHICLE MODE 
  x$veh_mode <- recode(x$vehicle_type,'-1'=99, '1'=2, '2'=3,'3'=3, '4'=3, '5'=3, '8'=4, '9'=4,
                       '10'=6, '11'=6, '16'=99, '17'=99,'18'=99, '19'=5, '20'=7, '21'=7, '22'=99,
                       '23'=3, '90'=99, '97'=3, '98'=7 ) 
  
  
  #keep integer values for future
  x$veh_mode.int <- x$cas_mode.int <- x$veh_mode 
  x$cas_mode.int[x$casualty_class==3]   <- 1
  x$number_of_parties <- x$number_of_vehicles + as.numeric(x$cas_mode.int==1)
  cas_indices <- subset(x,cas_mode.int==1,drop=T)$accident_index
  indices <- levels(droplevels(cas_indices))
  dup_indices <- levels(droplevels(cas_indices[duplicated(cas_indices)]))
  int_indices <- intersect(indices,dup_indices)
  temp <- x[x$accident_index%in%int_indices,]
  
  
  x$cas_mode.int[ is.na(x$cas_severity)]  <- NA
  
  #creates veh_mode/cas_mode label vars
  x$veh_mode = recode(x$veh_mode.int, '1'="pedestrian",'2' ="cyclist",'3'="motorcycle",
                      '4'="car/taxi",'5'="light goods",'6'="bus",'7'="heavy goods",
                      '8' = "NOV", '99' ="other or unknown")
  
  x$cas_mode = recode(x$cas_mode.int, '1'="pedestrian",'2' ="cyclist",'3'="motorcycle",
                      '4'="car/taxi",'5'="light goods",'6'="bus",'7'="heavy goods",
                      '8' = "NOV", '99' ="other or unknown")
  
  
  
  ## NO. OF PEDESTRIANS IN ACCIDENT    
  # add "numped" column
  x <- arrange(x, accident_index)
  x$pedflag <- 0   
  x$pedflag[x$cas_mode.int==1] <- 1   #  1 if cas_mode=1 | 0: otherwise
  stopped.gr <- aggregate(x$pedflag, by =list(x$accident_index), FUN=sum, na.rm=T)
  names(stopped.gr) <- c('accident_index', 'numped')
  x <- inner_join(x, stopped.gr, by="accident_index")
  
  count_mode <- function(mode_name,mode_description){
    x_sub[[mode_name]] <- 0   
    x_sub[[mode_name]][x_sub$veh_mode==mode_description] <- 1   #  1 if cas_mode=1 | 0: otherwise
    stopped.gr <- aggregate(x_sub[[mode_name]], by =list(x_sub$accident_index), FUN=sum, na.rm=T)
    names(stopped.gr) <- c('accident_index', paste0('num',mode_name))
    x <- inner_join(x, stopped.gr, by="accident_index")
    x
  }
  acc_ind_veh <- paste0(x$accident_index,x$vehicle_reference)
  x_sub <- x[!duplicated(acc_ind_veh),]
  x <- count_mode('car','car/taxi')
  x <- count_mode('bus','bus')
  x <- count_mode('mc','motorcycle')
  x <- count_mode('other','other or unknown')
  x <- count_mode('cyc','cyclist')
  x <- count_mode('lg','light goods')
  x <- count_mode('hg','heavy goods')
  
  cyc_sub <- subset(x,(numcyc>1|numcyc==1&cas_mode!='cyclist')&cas_severity=='Fatal')
  sapply(unique(cyc_sub$cas_mode),function(y)sum(cyc_sub$cas_mode==y))
  for(y in c('motorcycle','car/taxi','heavy goods','bus' ))
    print(subset(x,accident_index%in%subset(cyc_sub,cas_mode==y&number_of_vehicles<4)$accident_index)[,c(1,2,3,4,5,17,10,12,14,18)])
  subset(cyc_sub,cas_mode=='pedestrian'&(numcar>0|numbus>0|nummc>0|numother>0|numlg>0|numhg>0))
  cyc_ped_sub <- subset(cyc_sub,cas_mode=='pedestrian'&!(numcar>0|numbus>0|nummc>0|numother>0|numlg>0|numhg>0))
  sapply(unique(cyc_ped_sub$urban_or_rural_area),function(z)sapply(unique(cyc_ped_sub$roadtype),function(y)nrow(subset(cyc_ped_sub,roadtype==y&urban_or_rural_area==z))))
  
  car_sub <- subset(x,(numcar>1|numcar==1&cas_mode!='car/taxi')&cas_severity=='Fatal')
  sapply(unique(car_sub$cas_mode),function(y)sum(car_sub$cas_mode==y))
  subset(car_sub,cas_mode=='pedestrian'&(numbus>0|numother>0|numlg>0|numhg>0))
  car_ped_sub <- subset(car_sub,cas_mode=='pedestrian'&!(numbus>0|numother>0|numlg>0|numhg>0))
  sapply(unique(car_ped_sub$urban_or_rural_area),function(z)sapply(unique(car_ped_sub$roadtype),function(y)nrow(subset(car_ped_sub,roadtype==y&urban_or_rural_area==z))))
  
  #raw_rates <- expand.grid(mode=c('cyclist','car/taxi'),road=c('rural_A','urban_A','rural_B','urban_B'))
  #raw_rates$Fatalities <- c()
  #raw_rates$role <- 'Striker'
  #for(r in c('rural_A','urban_A','rural_B','urban_B'))
  #  for(m in c('cyclist','car/taxi')){
  #    raw_rates$Fatalities[raw_rates$mode==m&raw_rates$road==r] <- nrow(subset(ss19.1,roadtype==r&strike_mode==m&cas_severity=='Fatal'))/
  #      sum(raw_road_total[[mode=m,road=r]])
  #  }
  #pdf('rawRTS.pdf'); ggplot(raw_rates, aes(x = mode, y = Fatalities, fill = role)) + 
  #  geom_bar(stat='identity',position='stack') + facet_grid(~ road) +
  #  scale_fill_manual("legend", values = c("Casualty" = "darkorange", "Striker" = "navyblue")) +
  #  theme(axis.text.x = element_text(angle = 90, hjust = 1))
  #dev.off()
  
  ## FOR RAHUL
  if(process_for_rahul){
    all_injuries <- x
    fatal <- subset(all_injuries,cas_severity=='Fatal')
    england <- readRDS('~/overflow_dropbox/accidents_vehicles_casualties_05-15.rds')
    england <- subset(england,accident_index%in%fatal$accident_index)
    england <- england[!duplicated(england$accident_index),c(1,8,9)]
    ss19.1 <- left_join(fatal,england,by='accident_index')
    ss19.1 <- ss19.1[,c(1,4,5,7,10,14,16,17,21,22,24:33)]
    saveRDS(ss19.1,'~/overflow_dropbox/england_scotland_fatalities_all_veh_long_lat.Rds')
    write.csv(ss19.1,'~/overflow_dropbox/england_scotland_fatalities_all_veh_long_lat.csv')
  }
  ##
  
  ## FOR RACHEL
  if(process_for_rachel){
    nrow(subset(ss19.1,sapply(local_authority_.highway.,function(x)substring(as.character(x),1,1)%in%c('E'))))
    nrow(subset(ss19.1,roadtype!='Motorway/A(M)'&sapply(local_authority_.highway.,function(x)substring(as.character(x),1,1)%in%c('E'))))
    nrow(subset(ss19.1,number_of_parties>2&roadtype!='Motorway/A(M)'&sapply(local_authority_.highway.,function(x)substring(as.character(x),1,1)%in%c('E'))))
    nrow(subset(ss19.1,number_of_vehicles>2&roadtype!='Motorway/A(M)'&sapply(local_authority_.highway.,function(x)substring(as.character(x),1,1)%in%c('E'))))
    nrow(subset(ss19.1,number_of_vehicles-numother>2&roadtype!='Motorway/A(M)'&sapply(local_authority_.highway.,function(x)substring(as.character(x),1,1)%in%c('E'))))
  }
  ##
  
  #x <- subset(x,!is.na(x$cas_severity))
  saveRDS(x,'~/overflow_dropbox/all_england_data_2.Rds')
}


library(matrixStats)
library(latex2exp)
library(readstata13)
library(xlsx)
library(readODS)
library(tensorA)
library(dplyr)
library(tidyr)
library(MASS); 
library(splines)
#loc <- readRDS('~/overflow_dropbox/HPC/locality.RData')
all_Data <- x
#########################################################################################################################
## code name map
lacodes <- read.ods('~/Dropbox/ITHIM/InjuryModel/spatial/UKLAcodes.ods')[[1]][-1,]
lacodes10 <- read.ods('~/Dropbox/ITHIM/InjuryModel/spatial/UKLAcodes10.ods')[[1]]
injury_codes <- unique(all_Data$local_authority_.highway.)
#########################################################################################################################
## combine
lacodes <- lacodes[lacodes$A%in%injury_codes,]
#lacodes <- lacodes[lacodes$C%in%las,]
lacodes10 <- lacodes10[lacodes10$B%in%injury_codes,]
#lacodes10 <- lacodes10[lacodes10$A%in%las,]
#########################################################################################################################
code_to_la <- c(lapply(lacodes10$B,function(x)lacodes10$A[which(lacodes10$B==x)]),lapply(lacodes$A,function(x)lacodes$C[which(lacodes$A==x)]))
names(code_to_la) <- c(lacodes10$B,lacodes$A)
la_to_code <- c(lapply(lacodes10$A,function(x)lacodes10$B[which(lacodes10$A==x)]),lapply(lacodes$C,function(x)lacodes$A[which(lacodes$C==x)]))
names(la_to_code) <- c(lacodes10$A,lacodes$C)
#la_map <- lapply(names(la_to_code),function(x)raw_la[which(raw_la[,3]==x)[1],2])
#names(la_map) <- names(la_to_code)
#la_incomplete <- sapply(names(la_to_code),function(x)sum(raw_la[,3]==x))
#missing_places <- names(la_to_code)[la_incomplete<11]

#la_to_code <- la_to_code[la_incomplete==11]
#code_to_la <- code_to_la[la_incomplete==11]
#raw_la <- raw_la[!raw_la[,3]%in%missing_places,]
#region_list <- lapply(regions,function(x)unique(raw_la[which(raw_la[,2]==x),3]))
#names(region_list) <- regions

#########################################################################################################################
## population numbers
pop_la <- read.ods('~/Dropbox/ITHIM/InjuryModel/spatial/populationUK.ods')[[2]][-c(1:15),]
pop_la <- pop_la[,-5]
names(pop_la) <- c('code','region','ua','la','number','area','density')
pop_la$number<-as.numeric(gsub(',','',pop_la$number))
la_pop_map <- lapply(names(code_to_la),function(x)pop_la$number[which(pop_la$code==x)])
la_den_map <- lapply(names(code_to_la),function(x)as.numeric(pop_la$density[which(pop_la$code==x)]))
names(la_pop_map) <- names(code_to_la)
names(la_den_map) <- names(code_to_la)

summary_counts <- expand.grid(LA_Name=names(la_to_code),Year=2005:2015)
summary_counts$code <- unlist(la_to_code)
summary_counts$population <- unlist(sapply(summary_counts$LA_Name,function(x)la_pop_map[[la_to_code[[as.character(x)]]]]))
#summary_counts$count <- apply(summary_counts,1,function(x) nrow(subset(all_Data,year==as.numeric(x[2])&local_authority_.highway.==la_to_code[[as.character(x[1])]])))
summary_counts$dens <- unlist(sapply(summary_counts$LA_Name,function(x)la_den_map[[la_to_code[[as.character(x)]]]]))

add_count_to_summary <- function(read_column,write_column){
  stopped.gr <- aggregate(all_injuries[[read_column]], by =list(all_injuries$local_authority_.highway.,all_injuries$year), FUN=sum, na.rm=T)
  names(stopped.gr) <- c('code','Year', write_column)
  stopped.gr$Year <- as.integer(stopped.gr$Year)
  summary_counts <- inner_join(summary_counts, stopped.gr, by=c("code",'Year'))
  summary_counts
}
all_injuries <- x
all_injuries$unit <- 1
all_injuries$whw <- all_injuries$nov <- 0
all_injuries$whw[all_injuries$number_of_parties>1] <- 1
all_injuries$nov[all_injuries$number_of_parties==1] <- 1
summary_counts <- add_count_to_summary('whw','whw')
summary_counts <- add_count_to_summary('nov','nov')
summary_counts$count <- summary_counts$whw + summary_counts$nov
all_injuries$whw_ksi <- all_injuries$nov_ksi <- 0
all_injuries$whw_ksi[all_injuries$number_of_parties>1&all_injuries$cas_severity!='Slight'] <- 1
all_injuries$nov_ksi[all_injuries$number_of_parties==1&all_injuries$cas_severity!='Slight'] <- 1
summary_counts <- add_count_to_summary('whw_ksi','whw_ksi')
summary_counts <- add_count_to_summary('nov_ksi','nov_ksi')
all_injuries$whw_fatal <- all_injuries$nov_fatal <- 0
all_injuries$whw_fatal[all_injuries$number_of_parties>1&all_injuries$cas_severity=='Fatal'] <- 1
all_injuries$nov_fatal[all_injuries$number_of_parties==1&all_injuries$cas_severity=='Fatal'] <- 1
summary_counts <- add_count_to_summary('whw_fatal','whw_fatal')
summary_counts <- add_count_to_summary('nov_fatal','nov_fatal')

all_injuries$whw_bike <- all_injuries$nov_bike <- 0
all_injuries$whw_bike[all_injuries$number_of_parties>1&all_injuries$cas_mode.int==2] <- 1
all_injuries$nov_bike[all_injuries$number_of_parties==1&all_injuries$cas_mode.int==2] <- 1
summary_counts <- add_count_to_summary('whw_bike','whw_bike')
summary_counts <- add_count_to_summary('nov_bike','nov_bike')
all_injuries$whw_ksi_bike <- all_injuries$nov_ksi_bike <- 0
all_injuries$whw_ksi_bike[all_injuries$number_of_parties>1&all_injuries$cas_severity!='Slight'&all_injuries$cas_mode.int==2] <- 1
all_injuries$nov_ksi_bike[all_injuries$number_of_parties==1&all_injuries$cas_severity!='Slight'&all_injuries$cas_mode.int==2] <- 1
summary_counts <- add_count_to_summary('whw_ksi_bike','whw_ksi_bike')
summary_counts <- add_count_to_summary('nov_ksi_bike','nov_ksi_bike')
all_injuries$whw_fatal_bike <- all_injuries$nov_fatal_bike <- 0
all_injuries$whw_fatal_bike[all_injuries$number_of_parties>1&all_injuries$cas_severity=='Fatal'&all_injuries$cas_mode.int==2] <- 1
all_injuries$nov_fatal_bike[all_injuries$number_of_parties==1&all_injuries$cas_severity=='Fatal'&all_injuries$cas_mode.int==2] <- 1
summary_counts <- add_count_to_summary('whw_fatal_bike','whw_fatal_bike')
summary_counts <- add_count_to_summary('nov_fatal_bike','nov_fatal_bike')

saveRDS(summary_counts,'data/england_power_law_data.Rds')
#########################################################################################################################
## road distance name
#raw_road <- read.xlsx('~/overflow_dropbox/injury_predictions/VehicleTypeRoadTypeRegionalLevel.xlsx',sheetIndex=1,rowIndex=6:402,colClasses=c('numeric',rep('character',2),rep('numeric',7)))
raw_la <- read.xlsx('~/overflow_dropbox/injury_predictions/VehicleType_LALevel.xlsx',sheetIndex=1,rowIndex=6:1670,colClasses=c('numeric',rep('character',2),rep('numeric',7)))
#regions <- levels(raw_road[,2])
#las <- unique(raw_la[,3])

summary_counts <- left_join(summary_counts,raw_la,by=c('LA_Name','Year'))

summary_counts$total_travel <- summary_counts$Pedal.Cycles + summary_counts$All.Motor.Vehicles

summary_counts <- subset(summary_counts,!is.na(total_travel))



la_road_length_sheets <- read.ods('~/overflow_dropbox/ITHIM/InjuryModel/spatial/rdl0202.ods')
keep_sheets <- c(5,7,9,11,13,15,17,19,21,23,24)
la_road_by_year <- list()
for(i in 1:11){
  la_road_by_year[[i]] <- la_road_length_sheets[[keep_sheets[12-i]]][7:159,c(1,2,3,7,13,ifelse(i%in%c(9,10),19,21))]
  names(la_road_by_year[[i]]) <- c('code','region','LA','Motorway','A','B')
  la_road_by_year[[i]] <- la_road_by_year[[i]][-1,]
  la_road_by_year[[i]]$code[which(la_road_by_year[[i]]=='E08000037')] <- 'E08000020'
  la_road_by_year[[i]]$code[which(la_road_by_year[[i]]=='E06000057')] <- 'E06000048'
  for(j in 4:6) la_road_by_year[[i]][[j]] <- as.numeric(gsub(',','',la_road_by_year[[i]][[j]]))
  print(la_road_by_year[[i]]$LA[!la_road_by_year[[i]]$code %in% names(code_to_la)])
  la_road_by_year[[i]] <- la_road_by_year[[i]][la_road_by_year[[i]]$code %in% names(code_to_la),]
  la_road_by_year[[i]]$Year <- i+2004
}

road_data <- do.call(rbind,la_road_by_year)

summary_counts$code <- sapply(summary_counts$LA_Name,function(x)la_to_code[[as.character(x)]])
superset <- left_join(summary_counts,road_data,by=c('code','Year'))
superset$total_road <- superset$Motorway + superset$A + superset$B
superset$AB <- superset$A + superset$B
summary(glm(log(total_road)~log(population),data=superset,offset=log(population)))
superset$cyclist_density <- superset$Pedal.Cycles/superset$AB
superset$car_density <- superset$Car/superset$AB
superset$motor_density <- superset$All.Motor.Vehicles/superset$AB



saveRDS(superset,'data/england_power_law_data.Rds')



summary_counts <- readRDS('data/england_power_law_data.Rds')

model <- glm(count~log(I(dens*population/total_travel)),data=subset(summary_counts,Year==2011),family=poisson(),offset=log(I(dens*population/total_travel)))
summary(model)
model <- glm(count~log(population),data=subset(summary_counts,Year==2011),family=poisson(),offset=log(population))
summary(model)
model <- glm(nov~log(population),data=subset(summary_counts,Year==2011),family=poisson(),offset=log(population))
summary(model)
model <- glm(count~log(total_travel),data=summary_counts,family=poisson(),offset=log(total_travel))
summary(model)
model <- glm(whw~log(total_travel),data=summary_counts,family=poisson(),offset=log(total_travel))
summary(model)
model <- glm(nov~log(total_travel),data=summary_counts,family=poisson(),offset=log(total_travel))
summary(model)
model <- glm(count~log(All.Motor.Vehicles),data=summary_counts,family=poisson(),offset=log(All.Motor.Vehicles))
summary(model)
model <- glm(count~log(Pedal.Cycles),data=summary_counts,family=poisson(),offset=log(Pedal.Cycles))
summary(model)
model <- glm(total_travel~log(population),data=subset(summary_counts,Year==2011),family=gaussian(link=log),offset=log(population))
summary(model)

{x11(height=10); par(mfrow=c(6,2),mar=c(5,5,1,1))
  xvals <- c(7,24)
  ylabels <- c(', OV',', NOV')
  xlabels <- c('Travel','Population')
  extension <- c('','_ksi','_fatal')
  lab_extension <- c('Total','KSI','Fatal')
  for(j in 1:2)
    for(k in 1:3)
      for(i in 1:2){
        type <- paste0(c('whw','nov')[i],extension[k])
        off <- c('total_travel','population')[j]
        form <- paste0(type,'~log(',off,')')
        subsum <- summary_counts
        if(off=='population') subsum <- subset(summary_counts,Year==2011)
        plot(log(subsum[[off]]),log(subsum[[type]]),pch=16,col='grey',
             frame=F,xlab=xlabels[j],ylab=paste0(lab_extension[k],ylabels[i],sep=' '),cex.lab=1.5,cex.axis=1.5)
        model <- glm(as.formula(form),data=subsum,family=poisson(),offset=log(subsum[[off]]))
        lines(xvals,model$coefficients[1]+(1+model$coefficients[2])*xvals,col='navyblue',lwd=2)
        lines(xvals,model$coefficients[1]+(1+model$coefficients[2]+coef(summary(model))[2,2])*xvals,col='navyblue',lwd=2,lty=2)
        lines(xvals,model$coefficients[1]+(1+model$coefficients[2]-coef(summary(model))[2,2])*xvals,col='navyblue',lwd=2,lty=2)
        print(c(type,off,coef(summary(model))[2,4]))
        text(x=1.1*min(log(subsum[[off]])),y=0.9*max(log(subsum[[type]])),round(1+model$coefficients[2],2),col='navyblue',cex=1.5)
      }
}

x11(); {par(mar=c(5,5,2,2)); 
  plot(log(subset(summary_counts,Year==2015&LA_Name!='Isles of Scilly'&Region_Name!='London')$Car),log(subset(summary_counts,Year==2015&Region_Name!='London'&LA_Name!='Isles of Scilly')$Pedal.Cycles),pch=16,col='grey',
     frame=F,xlab='Cars',ylab='Bikes',cex.lab=1.5,cex.axis=1.5,xlim=c(19,23),ylim=c(14,19))
model <- glm(log(Pedal.Cycles)~log(Car),data=subset(summary_counts,Year==2015&LA_Name!='Isles of Scilly'&Region_Name!='London'),family=gaussian(),offset=log(Car))
xval <- c(22,22.5)
yval <- model$coefficients[1]+(1+model$coefficients[2])*xval
arrows(x0=xval[1],x1=xval[2],y0=yval[1],y1=yval[2],col='navyblue',lwd=2)
xval <- c(21,20.5)
yval <- model$coefficients[1]+(1+model$coefficients[2])*xval
arrows(x0=xval[1],x1=xval[2],y0=yval[1],y1=yval[2],col='navyblue',lwd=2)
arrows(x0=21.5,x1=21,y0=17,y1=18,col='navyblue',lwd=2)
}

{x11(height=10); par(mfrow=c(6,2),mar=c(5,5,1,1))
  xvals <- c(7,24)
  ylabels <- c(', OV',', NOV')
  xlabels <- c('Bike travel','Travel','Population')
  extension <- c('','_ksi','_fatal')
  lab_extension <- c('Total','KSI','Fatal')
  for(j in c(1,3))
    for(k in 1:3)
      for(i in 1:2){
        type <- paste0(c('whw','nov')[i],extension[k],'_bike')
        off <- c('Pedal.Cycles','total_travel','population')[j]
        form <- paste0(type,'~log(',off,')')
        subsum <- summary_counts
        if(off=='population') subsum <- subset(summary_counts,Year==2011)
        plot(log(subsum[[off]]),log(subsum[[type]]),pch=16,col='grey',
             frame=F,xlab=xlabels[j],ylab=paste0(lab_extension[k],ylabels[i],sep=' '),cex.lab=1.5,cex.axis=1.5)
        model <- glm(as.formula(form),data=subsum,family=poisson(),offset=log(subsum[[off]]))
        lines(xvals,model$coefficients[1]+(1+model$coefficients[2])*xvals,col='navyblue',lwd=2)
        lines(xvals,model$coefficients[1]+(1+model$coefficients[2]+coef(summary(model))[2,2])*xvals,col='navyblue',lwd=2,lty=2)
        lines(xvals,model$coefficients[1]+(1+model$coefficients[2]-coef(summary(model))[2,2])*xvals,col='navyblue',lwd=2,lty=2)
        print(c(type,off,coef(summary(model))[2,4]))
        text(x=1.1*min(log(subsum[[off]])),y=0.9*max(log(subsum[[type]])),round(1+model$coefficients[2],2),col='navyblue',cex=1.5)
      }
}

summary_counts$PD <- summary_counts$Region_Name!='London'
j <- k <- i <- 1
{x11(height=3,width=8); par(mfrow=c(1,3),mar=c(5,5,2,2))
  lab_extension <- c('log(injuries)','log(KSI)','log(fatalities)')
  for(k in 1:3){
    type <- paste0(c('whw','nov')[i],extension[k],'_bike')
    off <- c('Pedal.Cycles','total_travel','population')[j]
    form <- paste0(type,'~log(',off,')')
    subsum <- subset(summary_counts,!PD)
    if(off=='population') subsum <- subset(summary_counts,Year==2011)
    plot(log(subsum[[off]]),log(subsum[[type]]),pch=16,col='grey',
         frame=F,xlab='log(bike distance)',ylab=lab_extension[k],cex.lab=1.5,cex.axis=1.5)
    model <- glm(as.formula(form),data=subsum,family=poisson(),offset=log(subsum[[off]]))
    lines(xvals,model$coefficients[1]+(1+model$coefficients[2])*xvals,col='navyblue',lwd=2)
    lines(xvals,model$coefficients[1]+(1+model$coefficients[2]+coef(summary(model))[2,2])*xvals,col='navyblue',lwd=2,lty=2)
    lines(xvals,model$coefficients[1]+(1+model$coefficients[2]-coef(summary(model))[2,2])*xvals,col='navyblue',lwd=2,lty=2)
    print(c(type,off,coef(summary(model))[2,4]))
    text(x=1.05*min(log(subsum[[off]])),y=0.9*max(log(subsum[[type]])),round(1+model$coefficients[2],2),col='navyblue',cex=1.5)
  }}
i <- 2
{x11(height=3,width=8); par(mfrow=c(1,3),mar=c(5,5,2,2))
  lab_extension <- c('log(injuries)','log(KSI)','log(fatalities)')
  for(k in 1:3){
    type <- paste0(c('whw','nov')[i],extension[k],'_bike')
    off <- c('Pedal.Cycles','total_travel','population')[j]
    form <- paste0(type,'~log(',off,')')
    subsum <- subset(summary_counts,!PD)
    if(off=='population') subsum <- subset(summary_counts,Year==2011)
    plot(log(subsum[[off]]),log(subsum[[type]]),pch=16,col='grey',
         frame=F,xlab='log(bike distance)',ylab=lab_extension[k],cex.lab=1.5,cex.axis=1.5)
    model <- glm(as.formula(form),data=subsum,family=poisson(),offset=log(subsum[[off]]))
    lines(xvals,model$coefficients[1]+(1+model$coefficients[2])*xvals,col='navyblue',lwd=2)
    lines(xvals,model$coefficients[1]+(1+model$coefficients[2]+coef(summary(model))[2,2])*xvals,col='navyblue',lwd=2,lty=2)
    lines(xvals,model$coefficients[1]+(1+model$coefficients[2]-coef(summary(model))[2,2])*xvals,col='navyblue',lwd=2,lty=2)
    print(c(type,off,coef(summary(model))[2,4]))
    text(x=1.05*min(log(subsum[[off]])),y=0.9*max(log(subsum[[type]])),round(1+model$coefficients[2],2),col='navyblue',cex=1.5)
  }}

non_london <- subset(summary_counts,PD&LA_Name!='Isles of Scilly'&Year==2011)
car_mod <- glm(log(Car)~log(Pedal.Cycles),data=non_london)
non_london$logcarpred <- car_mod$fitted.values
non_london$logcarresidual <- log(non_london$Car) - non_london$logcarpred
bike_mod <- glm(log(Pedal.Cycles)~log(Car),data=non_london)
non_london$logbikepred <- bike_mod$fitted.values
non_london$logbikeresidual <- log(non_london$Pedal.Cycles) - non_london$logbikepred
par(mfrow=c(2,2))
plot(log(non_london$Pedal.Cycles),log(non_london$Car),pch=16,col='grey',
     frame=F,xlab='log(bike distance)',ylab='log(car distance)',cex.lab=1.5,cex.axis=1.5)
plot(log(non_london$Pedal.Cycles),non_london$logcarresidual,pch=16,col='grey',
     frame=F,xlab='log(bike distance)',ylab='Car residual',cex.lab=1.5,cex.axis=1.5)
model <- glm(whw_bike~log(Pedal.Cycles)+log(Car),family=poisson,data=non_london,offset=log(Car))
model <- glm(whw_bike~log(Car)+logbikeresidual,family=poisson,data=non_london,offset=log(Car))
plot(log(non_london$Pedal.Cycles),log(non_london$whw_ksi_bike),pch=16,col='grey',
     frame=F,xlab='log(bike distance)',ylab='Injuries',cex.lab=1.5,cex.axis=1.5)
plot(log(model$fitted.values),log(non_london$whw_ksi_bike),pch=16,col='grey',
     frame=F,xlab='Fitted injuries',ylab='Injuries',cex.lab=1.5,cex.axis=1.5)

par(mfrow=c(2,2))
plot(log(summary_counts$Pedal.Cycles),log(summary_counts$count))
plot(log(summary_counts$All.Motor.Vehicles),log(summary_counts$count))
plot(log(summary_counts$total_travel),log(summary_counts$count))
plot(log(summary_counts$population),log(summary_counts$count))

car_mod <- glm(log(Car)~log(Pedal.Cycles),data=summary_counts)
summary_counts$logcarpred <- car_mod$fitted.values
summary_counts$logcarresidual <- log(summary_counts$Car) - summary_counts$logcarpred
bike_mod <- glm(log(Pedal.Cycles)~log(Car),data=summary_counts)
summary_counts$logbikepred <- bike_mod$fitted.values
summary_counts$logbikeresidual <- log(summary_counts$Pedal.Cycles) - summary_counts$logbikepred

non_london <- subset(summary_counts,PD&LA_Name!='Isles of Scilly')

ext <- c('','_ksi','_fatal')
cov <- c('','+LA_Name','+LA_Name+logcarresidual','+logcarresidual')
betas <- pvals <- matrix(0,nrow=3,ncol=4)
for(i in 1:3) for(j in 1:4){
  term <- 'log(Pedal.Cycles)'#ns(Pedal.Cycles, df = 4)'
  form <- paste0('whw',ext[i],'_bike~',term,cov[j])
  model <- glm(as.formula(form),data=subset(non_london,count>0),family=poisson())
  betas[i,j] <- model$coefficients[2]
  pvals[i,j] <- coef(summary(model))[2,4]
  plot.Gam(model,terms=term)
}
summary(model)

model <- glm(whw_ksi_bike~log(Pedal.Cycles),data=subset(summary_counts,count>10),family=poisson())
summary(model)
model <- glm(whw_ksi_bike~log(Pedal.Cycles)+logcarresidual,data=non_london,family=poisson(),offset=log(Pedal.Cycles))
summary(model)
plot(log(non_london$whw_ksi_bike),log(model$fitted.values))
plot((non_london$whw_ksi_bike),(model$fitted.values))
plot(log(non_london$Pedal.Cycles),log(non_london$whw_bike/non_london$Pedal.Cycles))
plot(log(non_london$Pedal.Cycles),log(non_london$whw_ksi_bike/non_london$Pedal.Cycles))
plot(log(non_london$Pedal.Cycles),log(non_london$whw_fatal_bike/non_london$Pedal.Cycles))
model <- glm(whw_ksi_bike~log(Pedal.Cycles)+LA_Name,data=non_london,family=poisson(),offset=log(Pedal.Cycles))
summary(model)
plot(log(non_london$whw_ksi_bike),log(model$fitted.values))

long_form <- list()
long_form[[1]] <- summary_counts[,c(1,2,4,13,20,22,30,32)]
long_form[[2]] <- summary_counts[,c(1,2,4,15,20,22,30,32)]
long_form[[3]] <- summary_counts[,c(1,2,4,17,20,22,30,32)]
for(i in 1:3) names(long_form[[i]])[4] <- 'injuries'
long_form[[1]]$severity <- 'slight'
long_form[[2]]$severity <- 'serious'
long_form[[3]]$severity <- 'fatal'
long_form[[1]]$injuries <- long_form[[1]]$injuries - long_form[[2]]$injuries
long_form[[2]]$injuries <- long_form[[2]]$injuries - long_form[[3]]$injuries
intable <- do.call('rbind',long_form)
intable$rate <- intable$injuries/intable$Pedal.Cycles
for(sev in c('slight','serious')){#,'fatal')){
  model <- glm(injuries~log(Pedal.Cycles),data=subset(intable,severity%in%sev&Year==2011),family=poisson(),offset=log(Pedal.Cycles))
  model2 <- glm(injuries~1,data=subset(intable,severity%in%sev&Year==2011),family=poisson(),offset=log(Pedal.Cycles))
  plot(log(model$data$Pedal.Cycles),log(model$data$injuries),frame=F,ylab='Injuries',xlab='Cycling',
       cex.axis=1.5,cex.lab=1.5,pch=16,col='grey')
  lines(xvals,model$coefficients[1]+(1+model$coefficients[2])*xvals,col='navyblue',lwd=2)
  lines(xvals,model2$coefficients[1]+xvals,col='navyblue',lwd=2,lty=2)
  #lines(xvals,model$coefficients[1]+(1+model$coefficients[2]+coef(summary(model))[2,2])*xvals,col='navyblue',lwd=2,lty=2)
  #lines(xvals,model$coefficients[1]+(1+model$coefficients[2]-coef(summary(model))[2,2])*xvals,col='navyblue',lwd=2,lty=2)
  plot(log(model$data$Pedal.Cycles),log(model$data$injuries/model$data$Pedal.Cycles),frame=F,ylab='Injuries per cyclist',xlab='Cycling',
       cex.axis=1.5,cex.lab=1.5,pch=16,col='grey')
  lines(xvals,model$coefficients[1]+(1+model$coefficients[2])*xvals-xvals,col='navyblue',lwd=2)
  lines(xvals,model2$coefficients[1]+xvals-xvals,col='navyblue',lwd=2,lty=2)
  #lines(xvals,model$coefficients[1]+(1+model$coefficients[2]+coef(summary(model))[2,2])*xvals-xvals,col='navyblue',lwd=2,lty=2)
  #lines(xvals,model$coefficients[1]+(1+model$coefficients[2]-coef(summary(model))[2,2])*xvals-xvals,col='navyblue',lwd=2,lty=2)
  print(summary(model))
}
plot(log(intable$Pedal.Cycles),log(intable$injuries))

model <- glm(count~log(total_travel),data=subset(summary_counts,count>10),family=poisson(),offset=log(total_travel))
summary(model)
model <- glm(count~log(All.Motor.Vehicles),data=subset(summary_counts,count>10),family=poisson(),offset=log(All.Motor.Vehicles))
summary(model)
model <- glm(count~log(Pedal.Cycles),data=subset(summary_counts,count>10),family=poisson(),offset=log(Pedal.Cycles))
summary(model)

{par(mfrow=c(2,2),mar=c(5,5,2,2))
  plot(log(subset(summary_counts,count>10&Year==2011)$All.Motor.Vehicles),log(subset(summary_counts,count>10&Year==2011)$count),frame=F,cex.lab=1.5,cex.axis=1.5,pch=15,xlab='Total distance',ylab='Count')
  points(log(subset(summary_counts,count>10&Year==2011&PD)$All.Motor.Vehicles),
         log(subset(summary_counts,count>10&Year==2011&PD)$count),pch=15,col='hotpink')
  plot(log(subset(summary_counts,count>10&Year==2011)$population),log(subset(summary_counts,count>10&Year==2011)$All.Motor.Vehicles),pch=15,frame=F,cex.lab=1.5,cex.axis=1.5,ylab='Total distance',xlab='Population')
  points(log(subset(summary_counts,count>10&Year==2011&PD)$population),
         log(subset(summary_counts,count>10&Year==2011&PD)$All.Motor.Vehicles),pch=15,col='hotpink')
  plot(log(subset(summary_counts,count>10&Year==2011)$population),log(subset(summary_counts,count>10&Year==2011)$count),pch=15,frame=F,cex.lab=1.5,cex.axis=1.5,ylab='Count',xlab='Population')
  points(log(subset(summary_counts,count>10&Year==2011&PD)$population),
         log(subset(summary_counts,count>10&Year==2011&PD)$count),pch=15,col='hotpink')
  plot(log(subset(summary_counts,count>10&Year==2011)$dens),log(subset(summary_counts,count>10&Year==2011)$count),pch=15,frame=F,cex.lab=1.5,cex.axis=1.5,xlab='Population density',ylab='Count')
  points(log(subset(summary_counts,count>10&Year==2011&PD)$dens),
         log(subset(summary_counts,count>10&Year==2011&PD)$count),pch=15,col='hotpink')
}
{par(mfrow=c(2,2))
  plot(log(subset(summary_counts,count>10&Year==2011)$Pedal.Cycles),log(subset(summary_counts,count>10&Year==2011)$whw_fatal),pch=15,xlab='Cycle distance',ylab='Count')
  points(log(subset(summary_counts,count>10&Year==2011&PD)$Pedal.Cycles),
         log(subset(summary_counts,count>10&Year==2011&PD)$whw_fatal),pch=15,col='hotpink')
  
  plot(log(subset(summary_counts,count>10&Year==2011)$All.Motor.Vehicles),log(subset(summary_counts,count>10&Year==2011)$whw_fatal),pch=15,xlab='Total distance',ylab='Count')
  points(log(subset(summary_counts,count>10&Year==2011&PD)$All.Motor.Vehicles),
         log(subset(summary_counts,count>10&Year==2011&PD)$whw_fatal),pch=15,col='hotpink')
  
  plot(log(subset(summary_counts,count>10&Year==2011)$population),log(subset(summary_counts,count>10&Year==2011)$All.Motor.Vehicles),pch=15,ylab='Total distance',xlab='Population')
  points(log(subset(summary_counts,count>10&Year==2011&PD)$population),
         log(subset(summary_counts,count>10&Year==2011&PD)$All.Motor.Vehicles),pch=15,col='hotpink')
  
  plot(log(subset(summary_counts,count>10&Year==2011)$population),log(subset(summary_counts,count>10&Year==2011)$whw_fatal),pch=15,ylab='Count',xlab='Population')
  points(log(subset(summary_counts,count>10&Year==2011&PD)$population),
         log(subset(summary_counts,count>10&Year==2011&PD)$whw_fatal),pch=15,col='hotpink')
}



x11(); plot(log(subset(summary_counts,Year==2011)$population),log(subset(summary_counts,Year==2011)$count))
text(subset(summary_counts,Year==2011&LA_Name=='Cambridgeshire')$LA_Name,
     x=log(subset(summary_counts,Year==2011&LA_Name=='Cambridgeshire')$population),
     y=log(subset(summary_counts,Year==2011&LA_Name=='Cambridgeshire')$count))





la_road_length_sheets <- read.ods('~/overflow_dropbox/ITHIM/InjuryModel/spatial/rdl0202.ods')
keep_sheets <- c(5,7,9,11,13,15,17,19,21,23,24)
la_road_by_year <- list()
for(i in 1:11){
  la_road_by_year[[i]] <- la_road_length_sheets[[keep_sheets[12-i]]][7:159,c(1,2,3,7,13,ifelse(i%in%c(9,10),19,21))]
  names(la_road_by_year[[i]]) <- c('code','region','LA','Motorway','A','B')
  la_road_by_year[[i]] <- la_road_by_year[[i]][-1,]
  la_road_by_year[[i]]$code[which(la_road_by_year[[i]]=='E08000037')] <- 'E08000020'
  la_road_by_year[[i]]$code[which(la_road_by_year[[i]]=='E06000057')] <- 'E06000048'
  for(j in 4:6) la_road_by_year[[i]][[j]] <- as.numeric(gsub(',','',la_road_by_year[[i]][[j]]))
  print(la_road_by_year[[i]]$LA[!la_road_by_year[[i]]$code %in% names(code_to_la)])
  la_road_by_year[[i]] <- la_road_by_year[[i]][la_road_by_year[[i]]$code %in% names(code_to_la),]
  la_road_by_year[[i]]$Year <- i+2004
}

road_data <- do.call(rbind,la_road_by_year)

summary_counts$code <- sapply(summary_counts$LA_Name,function(x)la_to_code[[as.character(x)]])
superset <- left_join(summary_counts,road_data,by=c('code','Year'))
superset$total_road <- superset$Motorway + superset$A + superset$B
superset$AB <- superset$A + superset$B
summary(glm(log(total_road)~log(population),data=superset,offset=log(population)))
superset$cyclist_density <- superset$Pedal.Cycles/superset$AB

plot(log(subset(superset,Year==2011)$cyclist_density),log(subset(superset,Year==2011)$whw_bike))
plot(log(london$Pedal.Cycles),log(london$whw_bike)-log(london$Pedal.Cycles),
     frame=F,ylab='Injuries per cyclist',xlab='Cycling',cex.axis=1.5,cex.lab=1.5,pch=16,col='grey')
text('Injuries',x=15,y=-12.5,col='navyblue',cex=1.5)
plot(log(london$cyclist_density),log(london$whw_bike)-log(subset(london,Year==2011)$Pedal.Cycles),
     frame=F,ylab='Injuries per cyclist',xlab='Cycling density',cex.axis=1.5,cex.lab=1.5,pch=16,col='grey')
text('Injuries',x=9,y=-12.5,col='navyblue',cex=1.5)
plot(log(london$Pedal.Cycles),log(london$whw_ksi_bike)-log(london$Pedal.Cycles),
     frame=F,ylab='Injuries per cyclist',xlab='Cycling',cex.axis=1.5,cex.lab=1.5,pch=16,col='grey')
text('KSI',x=15,y=-15.5,col='navyblue',cex=1.5)
plot(log(london$cyclist_density),log(london$whw_ksi_bike)-log(london$Pedal.Cycles),
     frame=F,ylab='Injuries per cyclist',xlab='Cycling density',cex.axis=1.5,cex.lab=1.5,pch=16,col='grey')
text('KSI',x=9,y=-15.5,col='navyblue',cex=1.5)


london$relCar <- sapply(1:nrow(london),function(x)london$Car[x]/london$Car[london$LA_Name==london$LA_Name[x]&london$Year==2005])
london$relBike <- sapply(1:nrow(london),function(x)london$Pedal.Cycles[x]/london$Pedal.Cycles[london$LA_Name==london$LA_Name[x]&london$Year==2005])


od_data <- readRDS('~/overflow_dropbox/ITHIM/InjuryModel/od_data.Rds')
commute_prop <- c(0.0462,0.1178)
urbanicity <- read_ods('~/overflow_dropbox/ITHIM/InjuryModel/urban_2011.ods',col_names=T)
urbanicity2 <- read.csv('~/overflow_dropbox/ITHIM/InjuryModel/urban_county.csv')

superset <- superset[superset$code%in%c(urbanicity$LAD11CD,as.character(urbanicity2$CTY11CD)),]
superset$ruralpercent <- 0
for(i in 1:length(unique(superset$code))){
  cd <- unique(superset$code)[i]
  if(cd%in%urbanicity$LAD11CD){
    pc <- urbanicity[which(urbanicity$LAD11CD==cd),9]
  }else{
    pc <- urbanicity2[which(urbanicity2$CTY11CD==cd),11]
  }
  superset$ruralpercent[superset$code==cd] <- pc/100
}
  
lad_to_county <- read.csv('~/overflow_dropbox/ITHIM/InjuryModel/LAD_to_county.csv')

  
superset$Walk <- 0
for(i in 1:length(unique(superset$code))){
  cd <- unique(superset$code)[i]
  if(!cd%in%unique(od_data[[2]]$lad11cd1)){
    cd <- lad_to_county$LAD16CD[lad_to_county$CTY16CD==cd]
  }
  walkset <- subset(od_data[[2]],lad11cd1%in%cd)
  dists <- sum(walkset$foot*walkset$rf_dist_km)*5*(52-3)*2
  superset$Walk[superset$code==unique(superset$code)[i]] <- dists/(superset$ruralpercent[i]*commute_prop[1]+(1-superset$ruralpercent[i])*commute_prop[2])
}
  
superset$total_travel_plus_Walk <- superset$total_travel + superset$Walk

{par(mfrow=c(2,2))
  plot(log(subset(superset,count>10&Year==2011)$dens),log(subset(superset,count>10&Year==2011)$count),pch=15,xlab='Population density',ylab='Count')
  points(log(subset(superset,count>10&Year==2011&PD)$dens),
         log(subset(superset,count>10&Year==2011&PD)$count),pch=15,col='hotpink')
  
  plot(log(subset(superset,count>10&Year==2011)$total_travel_plus_Walk),log(subset(superset,count>10&Year==2011)$count),pch=15,xlab='Total distance',ylab='Count')
  points(log(subset(superset,count>10&Year==2011&PD)$total_travel_plus_Walk),
         log(subset(superset,count>10&Year==2011&PD)$count),pch=15,col='hotpink')
  
  plot(log(subset(superset,count>10&Year==2011)$population),log(subset(superset,count>10&Year==2011)$total_travel_plus_Walk),pch=15,ylab='Total distance',xlab='Population')
  points(log(subset(superset,count>10&Year==2011&PD)$population),
         log(subset(superset,count>10&Year==2011&PD)$total_travel_plus_Walk),pch=15,col='hotpink')
  
  plot(log(subset(superset,count>10&Year==2011)$population),log(subset(superset,count>10&Year==2011)$count),pch=15,ylab='Count',xlab='Population')
  points(log(subset(superset,count>10&Year==2011&PD)$population),
         log(subset(superset,count>10&Year==2011&PD)$count),pch=15,col='hotpink')
}  
{par(mfrow=c(2,2))
  plot(log(subset(superset,count>10&Year==2011)$AB),log(subset(superset,count>10&Year==2011)$count),pch=15,xlab='Road length',ylab='Count')
  points(log(subset(superset,count>10&Year==2011&PD)$AB),
         log(subset(superset,count>10&Year==2011&PD)$count),pch=15,col='hotpink')
  
  plot(log(subset(superset,count>10&Year==2011)$total_travel_plus_Walk),log(subset(superset,count>10&Year==2011)$count),pch=15,xlab='Total distance',ylab='Count')
  points(log(subset(superset,count>10&Year==2011&PD)$total_travel_plus_Walk),
         log(subset(superset,count>10&Year==2011&PD)$count),pch=15,col='hotpink')
  
  plot(log(subset(superset,count>10&Year==2011)$total_travel_plus_Walk/subset(superset,count>10&Year==2011)$AB),log(subset(superset,count>10&Year==2011)$count),
       pch=15,xlab='Road density',ylab='Count')
  points(log(subset(superset,count>10&Year==2011&PD)$total_travel_plus_Walk/subset(superset,count>10&Year==2011&PD)$AB),
         log(subset(superset,count>10&Year==2011&PD)$count),pch=15,col='hotpink')
  
  plot(log(subset(superset,count>10&Year==2011)$population),log(subset(superset,count>10&Year==2011)$count),pch=15,ylab='Count',xlab='Population')
  points(log(subset(superset,count>10&Year==2011&PD)$population),
         log(subset(superset,count>10&Year==2011&PD)$count),pch=15,col='hotpink')
}  

xvals<-1:30
{x11(height=5,width=5); par(mfrow=c(2,2),mar=c(5,5,2,2))
  plot(log(london$Car),log(london$Pedal.Cycles),pch=16,col='grey',
       frame=F,xlab='log(car distance)',ylab='log(bike distance)',cex.lab=1.5,cex.axis=1.5)
  model <- glm(Pedal.Cycles~log(london$Car),data=london,family=poisson())
  lines(xvals,model$coefficients[1]+(model$coefficients[2])*xvals,col='navyblue',lwd=2)
  print(model$coefficients[2])
  plot(log(london$Pedal.Cycles+london$Car),log(london$Car),pch=16,col='grey',
       frame=F,xlab='log(total distance)',ylab='log(car distance)',cex.lab=1.5,cex.axis=1.5)
  model <- glm(Car~log(london$Pedal.Cycles+london$Car),data=london,family=poisson())
  lines(xvals,model$coefficients[1]+(model$coefficients[2])*xvals,col='navyblue',lwd=2)
  print(model$coefficients[2])
  plot(log(london$Car),log(london$whw_bike),pch=16,col='grey',
       frame=F,xlab='log(car distance)',ylab='log(bike injuries)',cex.lab=1.5,cex.axis=1.5)
  model <- glm(whw_bike~log(london$Car),data=london,family=poisson())
  lines(xvals,model$coefficients[1]+(model$coefficients[2])*xvals,col='navyblue',lwd=2)
  print(model$coefficients[2])
  plot(log(london$Pedal.Cycles),log(london$whw_bike),pch=16,col='grey',
       frame=F,xlab='log(bike distance)',ylab='log(bike injuries)',cex.lab=1.5,cex.axis=1.5)
  model <- glm(whw_bike~log(Pedal.Cycles),data=london,family=poisson())
  lines(xvals,model$coefficients[1]+(model$coefficients[2])*xvals,col='navyblue',lwd=2)
  print(model$coefficients[2])
  #text(x=1.05*min(log(subsum[[off]])),y=0.9*max(log(subsum[[type]])),round(1+model$coefficients[2],2),col='navyblue',cex=1.5)
}
{x11(height=5,width=5); par(mfrow=c(2,2),mar=c(5,5,2,2))
  plot(log(london$Car),log(london$Pedal.Cycles),pch=16,col='grey',
       frame=F,xlab='log(car distance)',ylab='log(bike distance)',cex.lab=1.5,cex.axis=1.5)
  model <- glm(Pedal.Cycles~log(london$Car),data=london,family=poisson())
  lines(xvals,model$coefficients[1]+(model$coefficients[2])*xvals,col='navyblue',lwd=2)
  print(model$coefficients[2])
  plot(log(london$Pedal.Cycles+london$Car),log(london$Car),pch=16,col='grey',
       frame=F,xlab='log(total distance)',ylab='log(car distance)',cex.lab=1.5,cex.axis=1.5)
  model <- glm(Car~log(london$Pedal.Cycles+london$Car),data=london,family=poisson())
  lines(xvals,model$coefficients[1]+(model$coefficients[2])*xvals,col='navyblue',lwd=2)
  print(model$coefficients[2])
  plot(log(london$Car),log(london$whw_ksi_bike),pch=16,col='grey',
       frame=F,xlab='log(car distance)',ylab='log(bike injuries)',cex.lab=1.5,cex.axis=1.5)
  model <- glm(whw_ksi_bike~log(london$Car),data=london,family=poisson())
  lines(xvals,model$coefficients[1]+(model$coefficients[2])*xvals,col='navyblue',lwd=2)
  print(model$coefficients[2])
  plot(log(london$Pedal.Cycles+london$Car),log(london$whw_ksi_bike),pch=16,col='grey',
       frame=F,xlab='log(bike distance)',ylab='log(bike injuries)',cex.lab=1.5,cex.axis=1.5)
  model <- glm(whw_ksi_bike~log(Pedal.Cycles+london$Car),data=london,family=poisson())
  lines(xvals,model$coefficients[1]+(model$coefficients[2])*xvals,col='navyblue',lwd=2)
  print(model$coefficients[2])
  #text(x=1.05*min(log(subsum[[off]])),y=0.9*max(log(subsum[[type]])),round(1+model$coefficients[2],2),col='navyblue',cex=1.5)
}
{x11(height=2.5); par(mfrow=c(1,3),mar=c(5,5,2,2))
  plot(log(subset(non_london,Year==2011)$population),log(subset(non_london,Year==2011)$whw_ksi_bike),pch=16,col='grey',
       frame=F,xlab='log(population)',ylab='log(bike KSI)',cex.lab=1.5,cex.axis=1.5)
  plot(log(subset(non_london,Year==2011)$population),log(subset(non_london,Year==2011)$Car),pch=16,col='grey',
       frame=F,xlab='log(population)',ylab='log(car distance)',cex.lab=1.5,cex.axis=1.5)
  plot(log(subset(non_london,Year==2011)$population),log(subset(non_london,Year==2011)$Pedal.Cycles),pch=16,col='grey',
       frame=F,xlab='log(population)',ylab='log(bike distance)',cex.lab=1.5,cex.axis=1.5)
}
glm(whw_ksi_bike~log(population),family=poisson,data=subset(non_london,Year==2011))
glm(whw_ksi_bike~log(Pedal.Cycles)+log(Car),family=poisson,data=subset(non_london,Year==2011))

library(latex2exp)
beta_1<-c(0.52,0.76,0.29,0.36,0.4,0.73,0.5,0.27,0.32,0.62,0.55,0.64,0.58,0.35); 
beta_2<-c(0.65,0.35,0.09,0.2,.044,0.48,0.56,0.34,0.39,0.26,0.44,0.53,0.65,0.67)
bmean <- mean(beta_1+beta_2)
bmedian <- median(beta_1+beta_2)
{x11(); par(mar=c(5,5,2,2))
plot(density(beta_1+beta_2),main='',xlab=TeX('$\\beta_1+\\beta_2$'),frame=F,cex.lab=1.5,cex.axis=1.5,lwd=2,col='navyblue')
abline(v=bmean,col='darkorange2',lwd=2)
abline(v=bmedian,col='turquoise',lwd=2)
text('mean',x=bmean-0.15,y=0.1,col='darkorange2',cex=1.5)
text('median',x=bmedian+0.18,y=0.1,col='turquoise',cex=1.5)

}

mod<-glm(whw_bike~log(cyclist_density)+I(log(Car/AB)),family=poisson(),offset=log(Pedal.Cycles)+log(Car),data=london)
plot(london$whw_bike/london$Pedal.Cycles,mod$fitted.values/london$Pedal.Cycles)
mod<-glm(whw_ksi_bike~log(cyclist_density)+I(log(Car/AB)),family=poisson(),offset=log(Pedal.Cycles)+log(Car),data=london)
plot(london$whw_ksi_bike/london$Pedal.Cycles,mod$fitted.values/london$Pedal.Cycles)


{par(mfrow=c(2,2))
  plot(log(subset(superset,count>10&Year==2011)$Pedal.Cycles),log(subset(superset,count>10&Year==2011)$whw_bike),col='navyblue',pch=15,xlab='Cycle distance',ylab='Count')
  points(log(subset(superset,count>10&Year==2011&!PD)$Pedal.Cycles),
         log(subset(superset,count>10&Year==2011&!PD)$whw_bike),pch=15,col='hotpink')
  
  plot(log(subset(superset,count>10&Year==2011)$All.Motor.Vehicles),log(subset(superset,count>10&Year==2011)$whw_bike),col='navyblue',pch=15,xlab='Total distance',ylab='Count')
  points(log(subset(superset,count>10&Year==2011&!PD)$All.Motor.Vehicles),
         log(subset(superset,count>10&Year==2011&!PD)$whw_bike),pch=15,col='hotpink')
  
  plot(log(subset(superset,count>10&Year==2011)$Pedal.Cycles/subset(superset,count>10&Year==2011)$AB),
       log(subset(superset,count>10&Year==2011)$whw_bike),col='navyblue',pch=15,xlab='Cycle density',ylab='Count')
  points(log(subset(superset,count>10&Year==2011&!PD)$Pedal.Cycles/subset(superset,count>10&Year==2011&!PD)$AB),
         log(subset(superset,count>10&Year==2011&!PD)$whw_bike),pch=15,col='hotpink')
  
  plot(log(subset(superset,count>10&Year==2011&!PD)$Pedal.Cycles/subset(superset,count>10&Year==2011&!PD)$AB),
       log(subset(superset,count>10&Year==2011&!PD)$whw_bike/subset(superset,count>10&Year==2011&!PD)$Pedal.Cycles),col='hotpink',pch=15,
       xlab='Cycle density',ylab='Count per cyclist')
  
}
