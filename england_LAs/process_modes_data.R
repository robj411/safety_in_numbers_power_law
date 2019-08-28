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
loc <- readRDS('~/overflow_dropbox/HPC/locality.RData')

#########################################################################################################################
## code name map
lacodes <- read.ods('~/Dropbox/ITHIM/InjuryModel/spatial/UKLAcodes.ods')[[1]][-1,]
lacodes10 <- read.ods('~/Dropbox/ITHIM/InjuryModel/spatial/UKLAcodes10.ods')[[1]]
#########################################################################################################################
## road distance name
raw_road <- read.xlsx('~/overflow_dropbox/injury_predictions/VehicleTypeRoadTypeRegionalLevel.xlsx',sheetIndex=1,rowIndex=6:402,colClasses=c('numeric',rep('character',2),rep('numeric',7)))
raw_la <- read.xlsx('~/overflow_dropbox/injury_predictions/VehicleType_LALevel.xlsx',sheetIndex=1,rowIndex=6:1670,colClasses=c('numeric',rep('character',2),rep('numeric',7)))
regions <- levels(raw_road[,2])
las <- unique(raw_la[,3])

#########################################################################################################################
## injury code
load('~/overflow_dropbox/ITHIM/InjuryModel/Stats19.rda')
ss19<-tbl_df(ss19)
ss19<-ss19[complete.cases(ss19),]
ss19$district <- loc$local_authority_.highway.[match(ss19$accident_index,loc$accident_index)]
injury_codes <- unique(ss19$district)
#########################################################################################################################
## combine
lacodes <- lacodes[lacodes$A%in%injury_codes,]
lacodes <- lacodes[lacodes$C%in%las,]
lacodes10 <- lacodes10[lacodes10$B%in%injury_codes,]
lacodes10 <- lacodes10[lacodes10$A%in%las,]
#########################################################################################################################
code_to_la <- c(lapply(lacodes10$B,function(x)lacodes10$A[which(lacodes10$B==x)]),lapply(lacodes$A,function(x)lacodes$C[which(lacodes$A==x)]))
names(code_to_la) <- c(lacodes10$B,lacodes$A)
la_to_code <- c(lapply(lacodes10$A,function(x)lacodes10$B[which(lacodes10$A==x)]),lapply(lacodes$C,function(x)lacodes$A[which(lacodes$C==x)]))
names(la_to_code) <- c(lacodes10$A,lacodes$C)
la_map <- lapply(names(la_to_code),function(x)raw_la[which(raw_la[,3]==x)[1],2])
names(la_map) <- names(la_to_code)
saveRDS(la_map,'data/la_map.Rds')
la_incomplete <- sapply(names(la_to_code),function(x)sum(raw_la[,3]==x))
missing_places <- names(la_to_code)[la_incomplete<11]

la_to_code <- la_to_code[la_incomplete==11]
code_to_la <- code_to_la[la_incomplete==11]
raw_la <- raw_la[!raw_la[,3]%in%missing_places,]
region_list <- lapply(regions,function(x)unique(raw_la[which(raw_la[,2]==x),3]))
names(region_list) <- regions

#########################################################################################################################
## road length name
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
}
#########################################################################################################################
## population numbers
pop_la <- read.ods('~/Dropbox/ITHIM/InjuryModel/spatial/populationUK.ods')[[2]][-c(1:15),]
pop_la <- pop_la[,-5]
names(pop_la) <- c('code','region','ua','la','number','area','density')
pop_la$number<-as.numeric(gsub(',','',pop_la$number))
la_pop_map <- lapply(names(code_to_la),function(x)pop_la$number[which(pop_la$code==x)])
names(la_pop_map) <- names(code_to_la)

#########################################################################################################################
ss19 <- ss19 %>% droplevels() %>%
  filter(!strike_mode%in%c('pedestrian','other or unknown')) %>%
  filter(cas_mode=='cyclist') %>% #filter(!cas_mode%in%c('pedestrian','other or unknown')) %>% #
  filter(district%in%names(code_to_la))
ss19$region <- unlist(sapply(ss19$district,function(x)la_map[[code_to_la[[as.character(x)]]]]))
ss19$population <- unlist(sapply(ss19$district,function(x)la_pop_map[[as.character(x)]]))

ssg_total <-
  group_by(ss19,year,district) %>% 
  summarise(count=dplyr::n()) %>% 
  droplevels() %>% 
  as.data.frame() %>%    # remove "grouped" class, which breaks filling with zeroes
  complete(year,district,fill=list(count=0))
ssg_total$la_name <- unlist(sapply(ssg_total$district,function(x)code_to_la[[as.character(x)]]))
ssg_total$population <- unlist(sapply(ssg_total$district,function(x)la_pop_map[[as.character(x)]]))
ssg_total$road_length <- apply(ssg_total,1,function(x) sum(as.numeric(subset(la_road_by_year[[as.numeric(x[1])-2004]],code==as.character(x[2]))[5:6]),na.rm=T))
ssg_total$m_length <- apply(ssg_total,1,function(x) sum(as.numeric(subset(la_road_by_year[[as.numeric(x[1])-2004]],code==as.character(x[2]))[4]),na.rm=T))
plot(log(ssg_total$population),log(ssg_total$road_length))
fit1 <- glm(log(road_length)~log(population),data=ssg_total)
ssg_total$population[ssg_total$la_name=='City of London'] <- 500000
ssg_plot <- subset(ssg_total,count>0&year==2011)
plot(log(ssg_plot$population),log(ssg_plot$count))
points(log(ssg_plot$population[ssg_plot$la_name%in%c('Cambridgeshire')]),log(ssg_plot$count[ssg_plot$la_name%in%c('Cambridgeshire')]),col='red')
fit1 <- glm(count~log(population),family=poisson,data=ssg_total)

ssg_la <-
  group_by(ss19,cas_severity,strike_mode,year,district) %>% 
  summarise(count=dplyr::n()) %>% 
  droplevels() %>% 
  as.data.frame() %>%    # remove "grouped" class, which breaks filling with zeroes
  complete(cas_severity,strike_mode,year,district,fill=list(count=0))
ssg_la$la_name <- unlist(sapply(ssg_la$district,function(x)code_to_la[[as.character(x)]]))
ssg_la$population <- unlist(sapply(ssg_la$district,function(x)la_pop_map[[as.character(x)]]))

ssg_la_road <-
  group_by(ss19,cas_severity,strike_mode,roadtype,year,district) %>% 
  summarise(count=dplyr::n()) %>% 
  droplevels() %>% 
  as.data.frame() %>%    # remove "grouped" class, which breaks filling with zeroes
  complete(cas_severity,strike_mode,roadtype,year,district,fill=list(count=0))
ssg_la_road$la_name <- unlist(sapply(ssg_la_road$district,function(x)code_to_la[[as.character(x)]]))
ssg_la_road$population <- unlist(sapply(ssg_la_road$district,function(x)la_pop_map[[as.character(x)]]))

ssg_reg <-
  group_by(ss19,cas_severity,strike_mode,year,region) %>% 
  summarise(count=dplyr::n()) %>% 
  droplevels() %>% 
  as.data.frame() %>%    # remove "grouped" class, which breaks filling with zeroes
  complete(cas_severity,strike_mode,year,region,fill=list(count=0))

ssg_reg_road <-
  group_by(ss19,cas_severity,strike_mode,roadtype,year,region) %>% 
  summarise(count=dplyr::n()) %>% 
  droplevels() %>% 
  as.data.frame() %>%    # remove "grouped" class, which breaks filling with zeroes
  complete(cas_severity,strike_mode,roadtype,year,region,fill=list(count=0))

ssg_la$region <- sapply(ssg_la$la_name,function(x)la_map[[x]])
ssg_la_road$region <- sapply(ssg_la_road$la_name,function(x)la_map[[x]])
ssg_reg$population <- sapply(ssg_reg$region,function(x)sum(unlist(la_pop_map)[sapply(names(la_pop_map),function(y)la_map[[code_to_la[[y]]]]==x)] ))
la_names <- names(la_to_code)
reg_names <- unique(ssg_reg$region)

######################################################################################
modes <- levels(ssg_la$strike_mode)
roads <- levels(ssg_reg_road$roadtype)
roadmap <- list()
tab_modes <- names(raw_road)[4:9]
roadmap[['cyclist']] <- 'Pedal.Cycles'
roadmap[['motorcycle']] <- 'Two.Wheeled.Motor.Vehicles'
roadmap[['car/taxi']] <- 'Car'
roadmap[['light goods']] <- 'LGV'
roadmap[['bus']] <- 'Bus'
roadmap[['heavy goods']] <- 'HGV'
roadtypes <- list()
roadtypes[["Motorway/A(M)"]] <- 'Motorway'
roadtypes[["A"]] <- c('A-Road','A')
roadtypes[["B, C, Unclassified"]] <- c('B-Road','C or Unclassified','B')

la_length <- to.tensor(0,dims=c(year=11,road=length(roads),district=length(la_names)),
                       ndimnames=list(2005:2015,roads,la_names))
region_length <- to.tensor(0,dims=c(year=11,road=length(roads),region=length(regions)),
                           ndimnames=list(2005:2015,roads,regions))
for(i in 1:11)
  for(j in 1:length(roads)){
    la_length[[year=i,road=j]] <- la_road_by_year[[i]][match(names(code_to_la),la_road_by_year[[i]]$code),j+3]
  }
for(i in 1:11)
  for(k in 1:length(regions)){
    region_length[[year=i,region=k]] <- colSums(la_road_by_year[[i]][
      match(names(code_to_la)[sapply(names(la_to_code),function(y)la_map[[y]]==regions[k])],la_road_by_year[[i]]$code),
      4:6])
  }

raw_la_total <- to.tensor(0,dims=c(mode=6,year=11,district=length(la_names)),
                          ndimnames=list(modes,2005:2015,la_names))
temproad <- c()
for(i in 1:11)
  for(j in 1:length(modes)){
    for(l in 1:length(la_names))
      temproad[l] <- raw_la[[roadmap[[j]]]][raw_la$Year==i+2004&raw_la$LA_Name==la_names[l]]
    raw_la_total[[mode=j,year=i]] <- temproad*1.6
  }
raw_reg_total <- to.tensor(0,dims=c(mode=6,year=11,region=length(regions)),
                           ndimnames=list(modes,2005:2015,regions))
temproad <- c()
for(i in 1:11)
  for(j in 1:length(modes)){
    for(l in 1:length(regions))
      temproad[l] <- sum(raw_la[[roadmap[[j]]]][raw_la$Year==i+2004&raw_la$LA_Name%in%region_list[[l]]])
    raw_reg_total[[mode=j,year=i]] <- temproad*1.6
  }
raw_road_total <- to.tensor(0,dims=c(mode=6,year=11,road=length(roads),region=length(regions)),
                            ndimnames=list(modes,2005:2015,roads,regions))
temproad <- c()
for(i in 1:11)
  for(j in 1:length(modes))
    for(k in 1:length(regions)){
      for(l in 1:length(roads))
        temproad[l] <- sum(raw_road[[roadmap[[j]]]][raw_road$Year==i+2004&raw_road$RoadType%in%roadtypes[[l]]&raw_road$Region_Name==regions[k]])
      raw_road_total[[mode=j,year=i,region=k]] <- temproad*1.6
    }

la_road_total <- to.tensor(0,dims=c(mode=6,year=11,road=length(roads),district=length(la_names)),
                           ndimnames=list(modes,2005:2015,roads,la_names))
# D_l,t = D_l L_l,t D_r,t / L_r,t / sum_t(L_l,t D_r,t / L_r,t)
for(j in 1:length(modes)){
  for(i in 1:dim(raw_la_total)[3]){
    reg <- as.character(la_map[[dimnames(la_length)[[3]][i]]])
    la_length2 <- la_length
    la_length2[[district=which(sapply(dimnames(la_road_total)[[4]],function(x)la_map[[x]]==reg)),road=1]] <- as.numeric(la_length[[district=which(sapply(dimnames(la_road_total)[[4]],function(x)la_map[[x]]==reg)),road=1]]>0)
    producti <- la_length2[[district=i,road=1]]*raw_la_total[[mode=j,district=i]]
    product <- la_length2[[district=which(sapply(dimnames(la_road_total)[[4]],function(x)la_map[[x]]==reg)),road=1]]*raw_la_total[[mode=j,district=which(sapply(dimnames(la_road_total)[[4]],function(x)la_map[[x]]==reg))]]
    Mdist <- raw_road_total[[mode=j,region=reg,road=1]]*producti/margin.tensor(product,by=1)
    LDL <- la_length[[district=i,road=c(2,3)]]*raw_road_total[[mode=j,region=reg,road=c(2,3)]]/region_length[[region=reg,road=c(2,3)]]
    la_road_total[[mode=j,district=i,road=c(2,3)]] <- (raw_la_total[[mode=j,district=i]]-Mdist)*LDL/margin.tensor(LDL,by=1)
    la_road_total[[mode=j,district=i,road=1]] <- Mdist
  }
}

##check la road totals add up to region road totals, esp motorway
for(i in 1:3){
  print(raw_road_total[[mode=3,road=i,region=1]])
  print(margin.tensor(la_road_total[[mode=3,road=i,district=which(sapply(dimnames(la_road_total)[[4]],function(x)la_map[[x]]==regions[1]))]],by=1))
}
######################################################################################

ssg.la.base <- ssg_la %>% 
  mutate(
    cas_distance = apply(cbind(factor(ssg_la$year),ssg_la$la_name),1,function(x)as.numeric(raw_la_total[1,as.numeric(x[1]),which(dimnames(raw_la_total)[[3]]==x[2])])),
    strike_distance = apply(cbind(ssg_la$strike_mode,factor(ssg_la$year),ssg_la$la_name),1,function(x)raw_la_total[as.numeric(x[1]),as.numeric(x[2]),which(dimnames(raw_la_total)[[3]]==x[3])])
  )
ssg.la.road.base <- ssg_la_road %>% 
  mutate(
    cas_distance = apply(cbind(factor(ssg_la_road$year),ssg_la_road$roadtype,ssg_la_road$la_name),1,function(x)as.numeric(la_road_total[1,as.numeric(x[1]),as.numeric(x[2]),which(dimnames(la_road_total)[[4]]==x[3])])),
    strike_distance = apply(cbind(ssg_la_road$strike_mode,factor(ssg_la_road$year),ssg_la_road$roadtype,ssg_la_road$la_name),1,function(x)la_road_total[as.numeric(x[1]),as.numeric(x[2]),as.numeric(x[3]),which(dimnames(la_road_total)[[4]]==x[4])])
  )
ssg.reg.base <- ssg_reg %>% 
  mutate(
    cas_distance = apply(cbind(factor(ssg_reg$year),ssg_reg$region),1,function(x)raw_reg_total[1,x[1],x[2]]),
    strike_distance = apply(cbind(ssg_reg$strike_mode,factor(ssg_reg$year),ssg_reg$region),1,function(x)raw_reg_total[x[1],x[2],x[3]])
  )
ssg.total.base <- ssg_total %>% 
  mutate(
    cas_distance = apply(cbind(factor(ssg_total$year),ssg_total$la_name),1,function(x)as.numeric(raw_la_total[1,as.numeric(x[1]),which(dimnames(raw_la_total)[[3]]==x[2])]))
  )
ssg.road.base <- ssg_reg_road %>% 
  mutate(
    cas_distance = apply(cbind(factor(ssg_reg_road$year),ssg_reg_road$roadtype,ssg_reg_road$region),1,function(x)raw_road_total[1,x[1],x[2],x[3]]),
    strike_distance = apply(cbind(ssg_reg_road$strike_mode,factor(ssg_reg_road$year),ssg_reg_road$roadtype,ssg_reg_road$region),1,function(x)raw_road_total[x[1],x[2],x[3],x[4]]),
    cas_distance_sum = apply(cbind(factor(ssg_reg_road$year),ssg_reg_road$region),1,function(x)sum(raw_road_total[1,x[1],,x[2]])),
    strike_distance_sum = apply(cbind(ssg_reg_road$strike_mode,factor(ssg_reg_road$year),ssg_reg_road$region),1,function(x)sum(raw_road_total[x[1],x[2],,x[3]]))
  )
saveRDS(list(ssg.la.base,ssg.la.road.base,ssg.reg.base,ssg.road.base,ssg.total.base),'~/overflow_dropbox/safety_in_numbers_power_law/england_LAs/data/england_data.Rds')
datasets <- readRDS('~/overflow_dropbox/england_data.Rds')
ssg.la.base <- datasets[[1]]
ssg.la.road.base <- datasets[[2]]
ssg.reg.base <- datasets[[3]]
ssg.road.base <- datasets[[4]]

######################################################################################
ssg.total.base0 <- ssg.total.base
ssg.total.base <- subset(ssg.total.base0,!la_name%in%c('Isles of Scilly','Rutland','City of London'))
plot(log(ssg.total.base$population),log(ssg.total.base$road_length))
text(ssg.total.base$la_name,x=log(ssg.total.base$population),y=log(ssg.total.base$road_length))
#ssg.total.base$population[ssg.total.base$la_name=='City of London'] <- 500000
plot(log(ssg.total.base$population),log(ssg.total.base$cas_distance))
fit1 <- glm(log(road_length)~log(population),data=ssg.total.base)
fit1 <- glm(log(cas_distance)~log(population),data=ssg.total.base)
fit1 <- glm(count~log(population),family=poisson,data=ssg.total.base)
(fit1)
fit1 <- glm(count~log(cas_distance)+offset(log(population)),family=poisson,data=ssg.total.base)
(fit1)
fit1 <- glm(count~log(population)+offset(log(cas_distance)),family=poisson,data=ssg.total.base)
(fit1)
fit1 <- glm(count~log(cas_distance),family=poisson,data=ssg.total.base)
(fit1)
fit1 <- glm(count~log(population)+(log(cas_distance)+log(road_length)),family=poisson,data=ssg.total.base)
(fit1)
fit1 <- glm(count~log(population)+offset(log(cas_distance)-log(road_length)),family=poisson,data=ssg.total.base)
(fit1)
fit1 <- glm(count~log(population)+offset(log(cas_distance)-log(road_length)),family=poisson,data=subset(ssg.total.base,la_name!='Isles of Scilly'))
(fit1)
fit1 <- glm(count~I(log(cas_distance)-log(road_length)),family=poisson,data=ssg.total.base)
(fit1)

ssg_plot <- subset(ssg.total.base,count>0)#&la_name%in%c('City of London','Westminster','Cambridgeshire'))
plot(log(ssg_plot$cas_distance),log(ssg_plot$count))
plot(log(ssg_plot$cas_distance),log(ssg_plot$road_length))
plot(log(ssg_plot$cas_distance/ssg_plot$road_length),log(ssg_plot$count))
plot(log(ssg_plot$population),log(ssg_plot$count))
plot(log(ssg_plot$population),log(ssg_plot$count/ssg_plot$cas_distance*ssg_plot$road_length))
text(ssg_plot$la_name,x=log(ssg_plot$population),y=log(ssg_plot$count/ssg_plot$cas_distance*ssg_plot$road_length))
points(log(ssg_plot$population[ssg_plot$la_name%in%c('Cambridgeshire')]),log(ssg_plot$count[ssg_plot$la_name%in%c('Cambridgeshire')]),col='red')
fit1 <- glm(count~log(population),family=poisson,data=ssg.total.base)

######################################################################################

sev <- c('Fatal','Serious')
modes <- unique(ssg.la.base$strike_mode)
vehicle <- 2
for(vehicle in 2:length(modes)){
  veh <- modes[vehicle]
  dataset <- subset(ssg.la.base,cas_severity%in%sev&strike_mode==veh)#ssg.la.base
  fit <- glm(count~log(cas_distance)+log(strike_distance),data=dataset,family=poisson())
  print(c(veh,coef(fit)[2],coef(fit)[3],sum(dataset$strike_distance)/sum(dataset$cas_distance)))
}
saved_models <- list()
saved_models[[1]] <- coef(fit)
fit2 <- glm(count~log(cas_distance)+log(strike_distance)+offset(0.5*log(cas_distance)+0.5*log(strike_distance)),
            data=dataset,family=poisson())
summary(fit2)
fit3 <- glm(count~log(cas_distance)+log(strike_distance)+offset(log(cas_distance)+log(strike_distance)),
            data=dataset,family=poisson())
summary(fit3)
saved_models[[2]] <- coef(fit2)
aics <-matrix(0,nrow=20,ncol=5)
for(i in 1:20){
  newdata <- dataset[sample(1:nrow(dataset),size=nrow(dataset),replace=T),]
  fit <- glm(count~offset(saved_models[[1]][2]*log(cas_distance)+saved_models[[1]][3]*log(strike_distance)),
             data=newdata,
             family=poisson())
  aics[i,1] <- AIC(fit)
  fit2 <- glm(count~offset(0.5*log(cas_distance)+0.5*log(strike_distance)),data=newdata,family=poisson())
  aics[i,2] <- AIC(fit2)
  fit2 <- glm(count~offset(0.6*log(cas_distance)+0.4*log(strike_distance)),data=newdata,family=poisson())
  aics[i,3] <- AIC(fit2)
  fit2 <- glm(count~offset(0.7*log(cas_distance)+0.3*log(strike_distance)),data=newdata,family=poisson())
  aics[i,4] <- AIC(fit2)
  fit2 <- glm(count~offset(0.8*log(cas_distance)+0.2*log(strike_distance)),data=newdata,family=poisson())
  aics[i,5] <- AIC(fit2)
}
colMeans(aics)

