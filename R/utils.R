#.libPaths("G:/Delivery/Shared/ACAD/JonesDC/_Support/RWD")

library(tidyverse); theme_set(theme_bw())
library(WWSP)
library(lubridate)
library(tidyr)

data("hrt")


# Load from model ---------------------------------------------------------

load_icm <- function(filename) {
  mtime <- file.mtime(filename)
  
  # Import Files from Model
  data <- read.csv(filename, skip=0, header=T)
  colnames(data) <- c("cmms", paste0(0:23))
  
  
  tidy <- data %>% 
    gather("time", "rt", -cmms) %>%
    mutate(hour=as.numeric(time)) %>%
    group_by(cmms, hour) %>%
    dplyr::summarize(hrt=sum(rt))
  
  # tidy %>% 
  #   filter(cmms==unique(tidy$cmms)[1]) %>%
  #   ggplot(aes(x=hour, y=hrt)) +
  #   geom_line(lwd=1)
  
  tmp <- tidy %>% 
    spread("cmms", "hrt")
  
  clean_tmp <- matrix(rep(NA,nrow(tmp)*ncol(tmp)), nrow=nrow(tmp))
  clean_tmp <- as.data.frame(clean_tmp)
  clean_tmp[1,] <- as.data.frame(tmp[1,])
  
  for (i in 2:nrow(tmp)) {
    clean_tmp[i,] <- tmp[i,]-tmp[i-1,]
  }
  
  clean_tmp[,1] <- 0:23
  colnames(clean_tmp) <- colnames(tmp)
  
  use <- clean_tmp %>% 
    gather("cmms", "hrt", -hour)
  
  # use %>%
  #   filter(cmms==unique(tidy$cmms)[9]) %>%
  #   ggplot(aes(x=hour, y=hrt)) +
  #   geom_line(lwd=1)
  
  use$model_run <- mtime
  
  return(use)
}

use <- load_icm("./dev/model.csv")

tmp <- hrt %>% 
  filter(wday(datetime) %in% 2:6) %>%
  mutate(hour=hour(datetime)) %>% 
  group_by(cmms, hour) %>% 
  dplyr::summarize(mu=mean(runtime), sig=sd(runtime), q1=quantile(runtime, probs=0.25), q3=quantile(runtime, probs=0.75)) %>%
  dplyr::inner_join(use, by=c("cmms", "hour")) %>%
  mutate(z=(hrt-mu)/sig) %>%
  group_by(cmms, hour)

pumpstation_names <- hrt %>%
  group_by(grid, cmms, address, tag) %>%
  select(cmms, address,tag) %>%
  distinct()


sw <- pumpstation_names %>% 
  inner_join(use, by="cmms") %>% 
  select(cmms, address, tag) %>% 
  distinct()


#calibration_events <- use %>% select(model_run) %>% distinct() %>% unname()
calibration_events <- c("2019-11-12 16:40")

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
  group_by(cmms) %>% 
  summarize(avg=mean(z), sd=sd(z)) %>% 
  arrange(desc(avg))

error <- hrt %>%
  group_by(grid, cmms, address, tag) %>%
  select(cmms, address,tag) %>%
  distinct() %>%
  right_join(error, by="cmms")
