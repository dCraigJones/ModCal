#.libPaths("G:/Delivery/Shared/ACAD/JonesDC/_Support/RWD")

library(tidyverse); theme_set(theme_bw())
library(WWSP)
library(lubridate)
library(tidyr)
library(DT)

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
    summarize(hrt=sum(rt))
  
  tidy %>% 
    filter(cmms==unique(tidy$cmms)[1]) %>%
    ggplot(aes(x=hour, y=hrt)) +
       geom_line(lwd=1)
  
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
  
  use %>%
    filter(cmms==unique(tidy$cmms)[9]) %>%
    ggplot(aes(x=hour, y=hrt)) +
    geom_line(lwd=1)
  
  use$model_run <- mtime
  
  return(use)
}

use <- load_icm("./dev/model.csv")

tmp <- hrt %>% 
  filter(wday(datetime) %in% 2:6) %>%
  mutate(hour=hour(datetime)) %>% 
  group_by(cmms, hour) %>% 
  summarize(mu=mean(runtime), sig=sd(runtime), q1=quantile(runtime, probs=0.25), q3=quantile(runtime, probs=0.75)) %>%
  inner_join(use, by=c("cmms", "hour")) %>%
  mutate(z=(hrt-mu)/sig) %>%
  group_by(cmms, hour)

error <- tmp %>% 
  group_by(cmms) %>% 
  summarize(avg=mean(z), sd=sd(z)) %>% 
  arrange(desc(avg))

error <- hrt %>%
  group_by(grid, cmms, address, tag) %>%
  select(cmms, address,tag) %>%
  distinct() %>%
  right_join(error, by="cmms")


i <- 60

tmp %>% 
  filter(cmms==sw$cmms[i]) %>% 
  ggplot(aes(x=hour, y=mu)) + 
  geom_line(col="grey50") + 
  #geom_errorbar(aes(ymin=mu-sig, ymax=mu+sig)) + 
  geom_errorbar(aes(ymin=q1, ymax=q3), col="grey50") + 
  geom_line(aes(y=hrt), lwd=1) + 
  geom_point(aes(y=hrt), size=3) + 
  labs(title=error$address[i]
    #, tag=error$grid[i]
    , subtitle=error$cmms[i]
    , caption=error$tag[i]
  )

tmp %>% filter(cmms==sw$cmms[i]) %>%
  ggplot(aes(x=hour, y=z)) +
  geom_line(lwd=1) + 
  geom_point(size=3) + 
  geom_hline(yintercept=c(0), col=rgb(0,0,1,0.5), lwd=1) +
  #geom_hline(yintercept=-3:5, col=rgb(1,0,0,0.25), lwd=0.5) +
  geom_hline(yintercept=c(-1,1), col=rgb(1,0,0,0.25), lwd=0.5) +
  geom_hline(yintercept=c(-1.96,1.96), col=rgb(1,0,0,0.5), lwd=0.5) +
  geom_hline(yintercept=c(-2.54,2.54), col=rgb(1,0,0,1), lty=2) +
  ylab("z")
# 
# 
# p <- hrt %>% 
#   filter(cmms==error$cmms[4]) %>%
#   mutate(hour=hour(datetime)) %>%
#   ggplot(aes(x=hour, y=runtime)) +
#   geom_point() 
#   
# use %>% 
#   filter(cmms==error$cmms[40]) %>% 
#   ggplot(aes(x=hour, y=hrt)) +
#   geom_line()
# 
# 
# a <- p %>% 
#   ggplot(aes(x=hour)) +
#   geom_point(aes(y=runtime))
#   #ylim(c(0,20))
#   
# p %>% group_by(hour) %>% 
#   summarize(
#     q1=(quantile(runtime, probs=0.050)), 
#     q3=(quantile(runtime, probs=0.950))
#   ) %>% 
#   ggplot(aes(x=hour)) + 
#   geom_line(aes(y=q1)) + 
#   geom_line(aes(y=q3)) + a
# 
# 
# p %>% 
#   group_by(hour) %>% 
#   summarize(
#     q1=(quantile(runtime, probs=0.050)), 
#     q3=(quantile(runtime, probs=0.950))
#     ) %>% 
#   ggplot(aes(x=hour)) + 
#   geom_line(aes(y=q1)) + 
#   geom_line(aes(y=q3))



# General Match -----------------------------------------------------------
# 
# 
# 
# hrt %>% 
#   filter(wday(datetime) %in% 2:6) %>%
#   mutate(hour=hour(datetime)) %>% 
#   group_by(cmms, hour) %>% 
#   summarize(mu=mean(runtime), sig=sd(runtime)) %>%
#   inner_join(use, by=c("cmms", "hour")) %>%
#   mutate(z=(hrt-mu)/sig) %>%
#   group_by(cmms, hour) %>%
#   summarize(mu=mean(z)
#       , sd=sd(z)
#       , min=min(z)
#       , q1=quantile(z, probs=0.25)
#       , md=quantile(z, probs=0.5)
#       , q3=quantile(z, probs=0.75)
#       , max=max(z)
#   ) %>%
#   ggplot(aes(x=hour, y=md)) +
#     geom_line(lwd=1) + 
#     geom_point(size=3) + 
#     geom_hline(yintercept=c(0), col=rgb(0,0,1,0.5), lwd=1) +
#     #geom_hline(yintercept=-3:5, col=rgb(1,0,0,0.25), lwd=0.5) +
#     geom_hline(yintercept=c(-1,1), col=rgb(1,0,0,0.25), lwd=0.5) +
#     geom_hline(yintercept=c(-1.96,1.96), col=rgb(1,0,0,0.5), lwd=0.5) +
#     geom_hline(yintercept=c(-2.54,2.54), col=rgb(1,0,0,1), lty=2) +
#     ylab("z")
