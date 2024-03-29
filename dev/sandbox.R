if(!exists("tbl_ts")) {
  use <- load_icm("./dev/model.csv")
  
  tbl_ts <- hrt %>%
    filter(wday(datetime) %in% 2:6) %>%
    mutate(hour=hour(datetime)) %>%
    group_by(cmms, address, hour) %>%
    summarize(
        avg_runtime=mean(runtime)
      , sd_runtime=sd(runtime)
      , q1=quantile(runtime, probs=0.25)
      , q2=quantile(runtime, probs=0.5)
      , q3=quantile(runtime, probs=0.75)
    ) %>%
    inner_join(use, by=c("cmms", "hour")) %>%
    mutate(
        z=(hrt-avg_runtime)/sd_runtime
      , diff=hrt-avg_runtime
      , error=diff/avg_runtime
    )
  
  save(tbl_ts, file="./data/ts.RData")
}

if(!exists("tbl_info")) {
  tbl_info <- tbl_ts %>%
    ungroup() %>%
    mutate(
        cm=cumsum(hrt)
      , c1=cumsum(q1)
      , c2=cumsum(q2)
      , c3=cumsum(q3)
    ) %>% 
    group_by(cmms, address) %>%
    summarize(
        RT=mean(avg_runtime)*24
      , MPE=mean(error)
      , RMS=sqrt(sum(error^2))
      , mu_z=mean(z)
      , sd_z=sd(z)
      , beta=coef(lm(c1~cm))[2]
      , SSR=sum(diff^2)
      , SST=sum((hrt-mean(avg_runtime))^2)
      , NSE=1-(SSR/SST)
    ) %>%
    dplyr::mutate(approved=FALSE, comment="-", action="") %>%
    select(cmms, address, RT, MPE, RMS, mu_z, sd_z, beta, NSE, approved, comment, action)
  
  tbl_info$RT <- round(tbl_info$RT, 0)
  tbl_info[,4:9] <- round(tbl_info[,4:9],2)
  
  save(tbl_info, file="./data/info.RData")
}