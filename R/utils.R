#.libPaths("G:/Delivery/Shared/ACAD/JonesDC/_Support/RWD")

library(tidyverse); theme_set(theme_bw())
library(WWSP)
library(lubridate)
library(tidyr)

data("hrt")
load("./data/sw.RData")
load("./data/results.RData")

if(exists("results")) {tmp <- results}

# Load from model ---------------------------------------------------------

load_icm <- function(filename) {
  mtime <- file.mtime(filename)
  
  # Import Files from Model
  data <- read.csv(filename, skip=0, header=T)
  colnames(data) <- c("cmms", paste0(0:23))
  
  
  tidy <- data %>% 
    tidyr::gather("time", "rt", -cmms) %>%
    dplyr::mutate(hour=as.numeric(time)) %>%
    dplyr::group_by(cmms, hour) %>%
    dplyr::summarize(hrt=sum(rt))
  
  # tidy %>% 
  #   filter(cmms==unique(tidy$cmms)[1]) %>%
  #   ggplot(aes(x=hour, y=hrt)) +
  #   geom_line(lwd=1)
  
  tmp <- tidy %>% 
    tidyr::spread("cmms", "hrt")
  
  clean_tmp <- matrix(rep(NA,nrow(tmp)*ncol(tmp)), nrow=nrow(tmp))
  clean_tmp <- as.data.frame(clean_tmp)
  clean_tmp[1,] <- as.data.frame(tmp[1,])
  
  for (i in 2:nrow(tmp)) {
    clean_tmp[i,] <- tmp[i,]-tmp[i-1,]
  }
  
  clean_tmp[,1] <- 0:23
  colnames(clean_tmp) <- colnames(tmp)
  
  use <- clean_tmp %>% 
    tidyr::gather("cmms", "hrt", -hour)
  
  # use %>%
  #   filter(cmms==unique(tidy$cmms)[9]) %>%
  #   ggplot(aes(x=hour, y=hrt)) +
  #   geom_line(lwd=1)
  
  use$model_run <- mtime
  
  return(use)
}

if(!exists("tmp")) {
  tmp <- hrt %>% 
    dplyr::filter(lubridate::wday(datetime) %in% 2:6) %>%
    dplyr::mutate(hour=hour(datetime)) %>% 
    dplyr::group_by(cmms, hour) %>% 
    dplyr::summarize(mu=mean(runtime), sig=sd(runtime), q1=quantile(runtime, probs=0.25), q3=quantile(runtime, probs=0.75)) %>%
    dplyr::inner_join(use, by=c("cmms", "hour")) %>%
    dplyr::mutate(z=(hrt-mu)/sig) %>%
    dplyr::group_by(cmms, hour)
}

pumpstation_names <- hrt %>%
  dplyr::group_by(grid, cmms, address, tag) %>%
  dplyr::select(cmms, address,tag) %>%
  dplyr::distinct()


if(!exists("sw")) {
  use <- load_icm("./dev/model.csv")
  
  sw <- pumpstation_names %>% 
    dplyr::inner_join(use, by="cmms") %>% 
    dplyr::select(cmms, address, tag) %>% 
    dplyr::distinct() %>%
    dplyr::mutate(approved="-", comment="-")
}


#calibration_events <- use %>% select(model_run) %>% distinct() %>% unname()
calibration_events <- c("2019-11-12 16:40")

comment_options <- c(
    "-"
  , "New Development"
  , "Investigate - Pump"
  , "Investigate - Basin Flow"
  , "Investigate - As-Builts"
  , "Investigate - Valve Position"
  , "Field Verification Needed"
)

filter_options <- c(
    "All"
  , "Approved"
  , "Not Approved"
  , "Investigate"
)

wastewater_plants <- c(
    "I - Buckman"
  , "II - Cedar Bay"
  , "III - Southwest"
)

error <- tmp %>% 
  dplyr::group_by(cmms) %>% 
  dplyr::summarize(avg=mean(z), sd=sd(z)) %>% 
  dplyr::arrange(desc(avg))

error <- hrt %>%
  dplyr::group_by(grid, cmms, address) %>%
  dplyr::summarize(RT=mean(runtime)) %>%
  dplyr::select(cmms, address, RT) %>%
  dplyr::distinct() %>%
  dplyr::right_join(error, by="cmms")

error$RT <- round(error$RT, 2)
error$avg <- round(error$avg, 2)
error$sd <- round(error$sd, 2)


# tmp %>%
#   dplyr::group_by(cmms, hour) %>%
#   dplyr::summarize(
#     e = (mu - hrt),
#     se = e ^ 2,
#     mu = mu,
#     avg = mean(mu),
#     btm = (mu - avg)
#   ) %>%
#   dplyr::group_by(cmms) %>%
#   dplyr::summarize(mse = mean(se),
#                    sd = sum(btm),
#                    nse = (1 - mse / (sd ^ 2)))
# 
# tmp %>%
#   filter(cmms == "LS-004034") %>%
#   ggplot(aes(x = hrt, ymin = q1, ymax = q3)) +
#   geom_linerange() +
#   ylim(c(0, 60)) +
#   xlim(c(0, 60)) +
#   geom_abline(slope = 1)

                   