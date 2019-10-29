library(rstan)
library(parallel)
library(tableone)
setwd('~/overflow_dropbox/safety_in_numbers_power_law/england_LAs/')

summary_counts <- readRDS('data/england_models_all_data.Rds')
rural_percent_threshold <- 0.02
urban <- subset(summary_counts,ruralpercent<rural_percent_threshold)
rural <- subset(summary_counts,ruralpercent>=rural_percent_threshold)
datasets <- list(all=summary_counts,urban=urban,rural=rural)

summary(summary_counts)


## Load model
fileName <- "./density_regression.stan"
stan_code <- readChar(fileName, file.info(fileName)$size)
options(mc.cores = parallel::detectCores())

results_list <- list()
for(set in c(1,2,3)){
  results_list[[set]] <- list()
  for(lab in c('','_ksi','_fatal')){
    ## Run glm
    glm1 <- glm(formula = whw_ksi_bike_car ~ log(Pedal.Cycles)+log(Car) + offset(-log(AB)),
                family  = poisson(link = "log"),
                data    = datasets[[set]])
    summary(glm1)
    
    ## Get model matrix
    modMat <- as.data.frame(model.matrix(glm1))
    modMat$offset <- -log(datasets[[set]]$AB)
    names(modMat) <- c("intercept", "cycle", "motor", "offset")
    
    dat   <- as.list(modMat)
    dat$N <- nrow(modMat)
    dat$p <- ncol(modMat) - 1
    
    dat$y <- datasets[[set]][[paste0('whw',lab,'_bike_car')]]
    
    ## Run stan
    resStan <- stan(model_code = stan_code, data = dat,
                    chains = 3, iter = 4000, warmup = 1000, thin = 10)
    
    ## Show traceplot
    traceplot(resStan, pars = c("beta"), inc_warmup = TRUE)
    
    ## Frequentist
    tableone::ShowRegTable(glm1, exp = FALSE)
    
    print(c(names(datasets)[set],lab))
    ## Bayesian
    print(resStan, pars = c("beta","beta_sum"))
    results_list[[set]][[paste0('whw',lab,'_bike')]] <- summary(resStan, pars = c("beta","beta_sum"), probs=c(0.025,0.975))$summary
  }
}

rowlabels <- c('All','Urban','Rural')
for(row in c(4,2,3,1))
  for(set in c(1,2,3)){
    cat(paste0(rowlabels[set],' areas & '))
    for(lab in c('','_ksi','_fatal')){
      toprint <- results_list[[set]][[paste0('whw',lab,'_bike')]]
      cat(paste0(sprintf('%.2f',toprint[row,4]),'--{}',sprintf('%.2f',toprint[row,5])))
      if(lab=='_fatal'){
        cat(' \\\\ \n')
      }else{
        cat(' & ')
      }
    }
  }
          

#####################################################

long_form <- list()
keep_names <- c('LA_Name','Year','population','Pedal.Cycles','Car','AB')
long_form[[1]] <- summary_counts[,colnames(summary_counts)%in%c(keep_names,'whw_bike_car')]
long_form[[2]] <- summary_counts[,colnames(summary_counts)%in%c(keep_names,'whw_ksi_bike_car')]
long_form[[3]] <- summary_counts[,colnames(summary_counts)%in%c(keep_names,'whw_fatal_bike_car')]
for(i in 1:3) names(long_form[[i]])[which(!colnames(long_form[[i]])%in%keep_names)] <- 'injuries'
long_form[[1]]$severity <- 'slight'
long_form[[2]]$severity <- 'serious'
long_form[[3]]$severity <- 'fatal'
long_form[[1]]$injuries <- long_form[[1]]$injuries - long_form[[2]]$injuries
long_form[[2]]$injuries <- long_form[[2]]$injuries - long_form[[3]]$injuries
all_data <- do.call('rbind',long_form)
all_data$urban <- all_data$ruralpercent < 0.01
## Run glm
glm1 <- glm(formula = count ~ log(Pedal.Cycles)+log(Car) + offset(-log(AB)) + severity + urban,
            family  = poisson(link = "log"),
            data    = all_data)
summary(glm1)

## Get model matrix
modMat <- as.data.frame(model.matrix(glm1))
modMat$offset <- -log(all_data$AB)
names(modMat) <- c("intercept", "cycle", "motor", "serious", "slight", "urban", "offset")

dat   <- as.list(modMat)
dat$N <- nrow(modMat)
dat$p <- ncol(modMat) - 1

dat$y <- all_data$count

## Load model
fileName <- "./density_regression_long.stan"
stan_code <- readChar(fileName, file.info(fileName)$size)

## Run stan
resStan <- stan(model_code = stan_code, data = dat,
                chains = 3, iter = 4000, warmup = 1000, thin = 10)

## Show traceplot
traceplot(resStan, pars = c("beta"), inc_warmup = TRUE)

## Frequentist
tableone::ShowRegTable(glm1, exp = FALSE)

## Bayesian
print(resStan, pars = c("beta","beta_sum"))
