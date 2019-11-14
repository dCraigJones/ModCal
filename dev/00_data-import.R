.libPaths("G:/Delivery/Shared/ACAD/JonesDC/_Support/RWD")

library(tidyverse); theme_set(theme_bw())
library(WWSP)
library(lubridate)

data("hrt")


# Load from model ---------------------------------------------------------

data <- read.csv("./data-model/hrt_4.csv", skip=0, header=T)

colnames(data) <- c("cmms", paste0(0:23))

sw <- data %>% 
  gather("time", "rt", -cmms) %>%
  mutate(hour=as.numeric(time)) %>%
  group_by(cmms, hour) %>%
  summarize(hrt=sum(rt))

sw %>% 
  filter(cmms==unique(sw$cmms)[1]) %>%
  ggplot(aes(x=hour, y=hrt)) +
     geom_line(lwd=1)

library(tidyr)

tmp <- sw %>% 
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
  filter(cmms==unique(sw$cmms)[9]) %>%
  ggplot(aes(x=hour, y=hrt)) +
  geom_line(lwd=1)



# Load from SCADA ---------------------------------------------------------


tmp <- hrt %>%
  filter(date(datetime)>=mdy("10/15/2018")) %>%
  filter(date(datetime)<mdy("10/20/2018")) %>%
  mutate(hour=hour(datetime)) %>%
  inner_join(use, by=c("cmms", "hour")) %>%
  select(datetime, grid, cmms, address, tag, field=runtime, model=hrt)

tmp %>% 
  filter(cmms==unique(sw$cmms)[1]) %>% 
  ggplot(aes(x=model, y=field)) + 
  geom_point() + xlim(c(0,20)) + ylim(c(0,20))

tmp %>% 
  filter(cmms==unique(sw$cmms)[80]) %>% 
  #filter(cmms=="LS-000717") %>%
  ggplot(aes(x=hour, y=runtime)) + 
  geom_point() + 
  geom_smooth() +
  geom_line(aes(x=hour, y=hrt), col="red", lwd=1, lty=2)

NSE <- function(mod, obs) {
  # 1.00 to 0.50  Excellent - Final Design
  # 0.49 to 0.40  Very Good - Final Design
  # 0.39 to 0.30  Good - Prelim Design
  # 0.29 to 0.20  Fair - Planning
  # 0.21 or less  Poor - Screening
  #mod <- df[,model]
  #obs <- df[,field]
  
  SE <- sum((obs-mod)^2)
  MSE <- SE/length(obs)
  sig <- sd(obs)
  sig2 <- sig^2
  
  NSE <- 1 - MSE/sig2
  
  return(NSE)
}

a <- tmp %>% filter(cmms==error$cmms[100])

1-(sum(a$z^2)/24)/sd(a$z)^2



ISE <- function(mod, obs) {
  #  0.0 to  3.0  Excellent - Final Design
  #  3.1 to  6.0  Very Good - Final Design
  #  6.1 to 10.0  Good - Prelim Design
  # 10.1 to 25.0  Fair - Planning
  # 25.1 or more  Poor - Screening
  
  SE <- sum((obs-mod)^2)
  MSE <- SE/length(obs)
  S <- sum(obs)
  
  ISE <- sqrt(MSE)/S * 100
  
  return(ISE)
}


hrt %>% 
  filter(cmms==unique(sw$cmms)[8]) %>% 
  mutate(hour=hour(datetime)) %>% 
  group_by(cmms, hour) %>% 
  summarize(mu=mean(runtime), sig=sd(runtime)) %>%
  inner_join(use, by=c("cmms", "hour")) %>%
    mutate(z=(hrt-mu)/sig) %>%
    ggplot(aes(x=hour, y=z)) +
      geom_line(lwd=1) + 
      geom_point(size=3) + 
      geom_hline(yintercept=c(0), col=rgb(0,0,1,0.5), lwd=1) +
      geom_hline(yintercept=c(-1,1), col=rgb(1,0,0,0.25), lwd=0.5) +
      geom_hline(yintercept=c(-2,2), col=rgb(1,0,0,0.5), lwd=0.5) +
      geom_hline(yintercept=c(-3,3), col=rgb(1,0,0,1))


# Get Unique --------------------------------------------------------------

# hrt %>%
#   right_join(sw, by="cmms") %>%
#   select(cmms, grid, address, tag) %>% 
#   group_by(cmms) %>%
#   unique()

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
  summarize(avg=abs(mean(z))) %>% 
  arrange(desc(avg))

error <- hrt %>%
  group_by(grid, cmms, address, tag) %>%
  select(cmms, address,tag) %>%
  distinct() %>%
  right_join(error, by="cmms")


i <- 60

tmp %>% 
  filter(cmms==error$cmms[i]) %>% 
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

tmp %>% filter(cmms==error$cmms[i]) %>%
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



hrt %>% 
  filter(wday(datetime) %in% 2:6) %>%
  mutate(hour=hour(datetime)) %>% 
  group_by(cmms, hour) %>% 
  summarize(mu=mean(runtime), sig=sd(runtime)) %>%
  inner_join(use, by=c("cmms", "hour")) %>%
  mutate(z=(hrt-mu)/sig) %>%
  group_by(cmms, hour) %>%
  summarize(mu=mean(z)
      , sd=sd(z)
      , min=min(z)
      , q1=quantile(z, probs=0.25)
      , md=quantile(z, probs=0.5)
      , q3=quantile(z, probs=0.75)
      , max=max(z)
  ) %>%
  ggplot(aes(x=hour, y=md)) +
    geom_line(lwd=1) + 
    geom_point(size=3) + 
    geom_hline(yintercept=c(0), col=rgb(0,0,1,0.5), lwd=1) +
    #geom_hline(yintercept=-3:5, col=rgb(1,0,0,0.25), lwd=0.5) +
    geom_hline(yintercept=c(-1,1), col=rgb(1,0,0,0.25), lwd=0.5) +
    geom_hline(yintercept=c(-1.96,1.96), col=rgb(1,0,0,0.5), lwd=0.5) +
    geom_hline(yintercept=c(-2.54,2.54), col=rgb(1,0,0,1), lty=2) +
    ylab("z")
