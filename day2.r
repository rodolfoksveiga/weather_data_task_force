# code info
# file name: demonstration of bias correction for the city of montreal is
# writer: Abhishek Gaur
# institution: National Research Council Canada
# email: Abhishek.Gaur@nrc-cnrc.gc.ca
# phone: +1-613-998-9799

# data required
# 1. observational data
# 2. rcm data
  # a) observational time-period
  # b) rcm: 2001-2020, 2041-2060, 2081-2100

# load libraries
library(ggplot2)
library(lubridate)
library(MBC)

# pre-process data ####
# load files
obs_cal = read.csv("./obs_montreal-airport_1998-2017.csv")
rcm_cal = read.csv("./rcm_montreal-airport_1998-2017.csv")
rcm_2010s = read.csv("./rcm_montreal-airport_2001-2020.csv")
rcm_2050s = read.csv("./rcm_montreal-airport_2041-2060.csv")
rcm_2090s = read.csv("./rcm_montreal-airport_2081-2100.csv")
# reframe data for bias correction
obs_cal = cbind(obs_cal$RAIN_mm,obs_cal$WDIR_deg,
                obs_cal$WSP_mpers,obs_cal$DRYBTEMP_degc,
                obs_cal$GHI_kjperm2,obs_cal$STNPR_pa,obs_cal$RHUM_per,
                obs_cal$TCC_percent,obs_cal$SNOWD_cm)
rcm_cal = cbind(rcm_cal$rain_mm,rcm_cal$wdr_deg,
                rcm_cal$wsp_mpers,rcm_cal$tas_degc,
                rcm_cal$rsds_kjperm2,rcm_cal$ps_pa,rcm_cal$hurs_per,
                rcm_cal$clt_per,rcm_cal$snd_cm)
rcm_2010s = cbind(rcm_2010s$rain_mm,rcm_2010s$wdr_deg,
                  rcm_2010s$wsp_mpers,rcm_2010s$tas_degc,
                  rcm_2010s$rsds_kjperm2,rcm_2010s$ps_pa,rcm_2010s$hurs_per,
                  rcm_2010s$clt_per,rcm_2010s$snd_cm)
rcm_2050s = cbind(rcm_2050s$rain_mm,rcm_2050s$wdr_deg,
                  rcm_2050s$wsp_mpers,rcm_2050s$tas_degc,
                  rcm_2050s$rsds_kjperm2,rcm_2050s$ps_pa,rcm_2050s$hurs_per,
                  rcm_2050s$clt_per,rcm_2050s$snd_cm)
rcm_2090s = cbind(rcm_2090s$rain_mm,rcm_2090s$wdr_deg,
                  rcm_2090s$wsp_mpers,rcm_2090s$tas_degc,
                  rcm_2090s$rsds_kjperm2,rcm_2090s$ps_pa,rcm_2090s$hurs_per,
                  rcm_2090s$clt_per,rcm_2090s$snd_cm)
# define a dataframe of ratios and traces
ratio_trace_df = data.frame(varnames = c("rain_mm", "wdr_deg", "wsp_mpers", "tas_degc",
                                         "rsds_kjperm2", "ps_pa", "hurs_per", "clt_per", "snd_cm"),
                            ratio.seq = c("TRUE", "TRUE", "TRUE", "FALSE", "FALSE",
                                          "TRUE", "TRUE", "TRUE", "TRUE"),
                            trace = c(0.1, 10, 0.1, Inf, Inf, 10, 0, 10, 1))

# apply the mbcn methods ####
set.seed(1)
MBCout_2010s = MBCn(o.c = obs_cal, m.c = rcm_cal, m.p = rcm_2010s,
                    ratio.seq = ratio_trace_df$ratio.seq, trace = ratio_trace_df$trace)
set.seed(1)
MBCout_2050s = MBCn(o.c = obs_cal, m.c = rcm_cal, m.p = rcm_2050s,
                    ratio.seq = ratio_trace_df$ratio.seq, trace = ratio_trace_df$trace)
set.seed(1)
MBCout_2090s = MBCn(o.c = obs_cal, m.c = rcm_cal, m.p = rcm_2090s,
                    ratio.seq = ratio_trace_df$ratio.seq, trace = ratio_trace_df$trace)

# post-processing ####
# ensure that all variables are within the expected range (no checks for tas)
rcm_bc_cal = data.frame(MBCout_2010s[[1]])
colnames(rcm_bc_cal) = ratio_trace_df$varnames
# rain should be >= 0.1
if(any(rcm_bc_cal$rain_mm < 0.1)) {
  rcm_bc_cal$rain_mm[which(rcm_bc_cal$rain_mm < 0.1)] = 0
}
# wdr should be between 0 and 360
if(any(rcm_bc_cal$wdr_deg < 0)) {
  rcm_bc_cal$wdr_deg[which(rcm_bc_cal$wdr_deg < 0)] =
    360 + rcm_bc_cal$wdr_deg[which(rcm_bc_cal$wdr_deg < 0)]
}
if(any(rcm_bc_cal$wdr_deg >= 360)) {
  rcm_bc_cal$wdr_deg[which(rcm_bc_cal$wdr_deg>=360)]=rcm_bc_cal$wdr_deg[which(rcm_bc_cal$wdr_deg>=360)]-360
}
# wsp should be >=0
if(any(rcm_bc_cal$wsp_mpers < 0)) {
  rcm_bc_cal$wsp_mpers[which(rcm_bc_cal$wsp_mpers < 0)] = 0
}
# rsds should be >= 0
if(any(rcm_bc_cal$rsds_kjperm2 < 0)) {
  rcm_bc_cal$rsds_kjperm2[which(rcm_bc_cal$rsds_kjperm2<0)] = 0
}
# ps should be >= 0
if(any(rcm_bc_cal$ps_pa<0)) {
  rcm_bc_cal$ps_pa[which(rcm_bc_cal$ps_pa<0)]=0
}
# hurs should be between 0 and 100
if(any(rcm_bc_cal$hurs_per < 0)) {
  rcm_bc_cal$hurs_per[which(rcm_bc_cal$hurs_per<0)] = 0
}
if(any(rcm_bc_cal$hurs_per > 100)) {
  rcm_bc_cal$hurs_per[which(rcm_bc_cal$hurs_per>100)] = 100
}
# clt should be between 0 and 100
if(any(rcm_bc_cal$clt_per < 0)) {
  rcm_bc_cal$clt_per[which(rcm_bc_cal$clt_per<0)] = 0
}
if(any(rcm_bc_cal$clt_per > 100)) {
  rcm_bc_cal$clt_per[which(rcm_bc_cal$clt_per>100)] = 100
}
# snd should be >= 1
if(any(rcm_bc_cal$snd_cm < 1)) {
  rcm_bc_cal$snd_cm[which(rcm_bc_cal$snd_cm<1)] = 0
}
# 2010s
rcm_bc_2010s = data.frame(MBCout_2010s[[2]])
colnames(rcm_bc_2010s) = ratio_trace_df$varnames
# rain should be >= 0.1
if(any(rcm_bc_2010s$rain_mm < 0.1)) {
  rcm_bc_2010s$rain_mm[which(rcm_bc_2010s$rain_mm < 0.1)] = 0
}
# wdr should be 0-360
if(any(rcm_bc_2010s$wdr_deg < 0)) {
  rcm_bc_2010s$wdr_deg[which(rcm_bc_2010s$wdr_deg<0)] =
    360 + rcm_bc_2010s$wdr_deg[which(rcm_bc_2010s$wdr_deg < 0)]
}
if(any(rcm_bc_2010s$wdr_deg >= 360)) {
  rcm_bc_2010s$wdr_deg[which(rcm_bc_2010s$wdr_deg >= 360)] =
    rcm_bc_2010s$wdr_deg[which(rcm_bc_2010s$wdr_deg>=360)] - 360
}
# wsp should be >= 0
if(any(rcm_bc_2010s$wsp_mpers < 0)) {
  rcm_bc_2010s$wsp_mpers[which(rcm_bc_2010s$wsp_mpers < 0)] = 0
}
# rsds should be >=0
if(any(rcm_bc_2010s$rsds_kjperm2 < 0))
  rcm_bc_2010s$rsds_kjperm2[which(rcm_bc_2010s$rsds_kjperm2 < 0)]=0
# ps should be >=0
if(any(rcm_bc_2010s$ps_pa<0))
  rcm_bc_2010s$ps_pa[which(rcm_bc_2010s$ps_pa < 0)] = 0
# hurs should be 0-100
if(any(rcm_bc_2010s$hurs_per < 0)) {
  rcm_bc_2010s$hurs_per[which(rcm_bc_2010s$hurs_per < 0)] = 0
}
if(any(rcm_bc_2010s$hurs_per > 100)) {
  rcm_bc_2010s$hurs_per[which(rcm_bc_2010s$hurs_per > 100)] = 100
}
# clt should be 0-100
if(any(rcm_bc_2010s$clt_per < 0)) {
  rcm_bc_2010s$clt_per[which(rcm_bc_2010s$clt_per < 0)] = 0
}
if(any(rcm_bc_2010s$clt_per > 100)) {
  rcm_bc_2010s$clt_per[which(rcm_bc_2010s$clt_per > 100)] = 100
}
# snd should be >= 1
if(any(rcm_bc_2010s$snd_cm < 1)) {
  rcm_bc_2010s$snd_cm[which(rcm_bc_2010s$snd_cm < 1)] = 0
}
# 2050s
rcm_bc_2050s = data.frame(MBCout_2050s[[2]]) {
  colnames(rcm_bc_2050s) = ratio_trace_df$varnames
}
# rain should be >=0.1
if(any(rcm_bc_2050s$rain_mm < 0.1)) {
  rcm_bc_2050s$rain_mm[which(rcm_bc_2050s$rain_mm < 0.1)] = 0
}
# wdr should be between 0 and 360
if(any(rcm_bc_2050s$wdr_deg < 0)) {
  rcm_bc_2050s$wdr_deg[which(rcm_bc_2050s$wdr_deg < 0)] =
    360+rcm_bc_2050s$wdr_deg[which(rcm_bc_2050s$wdr_deg < 0)]
}
if(any(rcm_bc_2050s$wdr_deg >= 360)) {
  rcm_bc_2050s$wdr_deg[which(rcm_bc_2050s$wdr_deg >= 360)] =
    rcm_bc_2050s$wdr_deg[which(rcm_bc_2050s$wdr_deg >= 360)] - 360
}
# wsp should be >=0
if(any(rcm_bc_2050s$wsp_mpers<0)) {
  rcm_bc_2050s$wsp_mpers[which(rcm_bc_2050s$wsp_mpers<0)]=0
}
# rsds should be >=0
if(any(rcm_bc_2050s$rsds_kjperm2 < 0)) {
  rcm_bc_2050s$rsds_kjperm2[which(rcm_bc_2050s$rsds_kjperm2 < 0)] = 0
}
# ps should be >=0
if(any(rcm_bc_2050s$ps_pa < 0)) {
  rcm_bc_2050s$ps_pa[which(rcm_bc_2050s$ps_pa < 0)] = 0
}
# hurs should be between 0 and100
if(any(rcm_bc_2050s$hurs_per < 0)) {
  rcm_bc_2050s$hurs_per[which(rcm_bc_2050s$hurs_per < 0)] = 0
}
if(any(rcm_bc_2050s$hurs_per > 100)) {
  rcm_bc_2050s$hurs_per[which(rcm_bc_2050s$hurs_per > 100)] = 100
}
# clt should be 0-100
if(any(rcm_bc_2050s$clt_per < 0)) {
  rcm_bc_2050s$clt_per[which(rcm_bc_2050s$clt_per < 0)] = 0
}
if(any(rcm_bc_2050s$clt_per > 100)) {
  rcm_bc_2050s$clt_per[which(rcm_bc_2050s$clt_per > 100)] = 100
}
# snd should be >=1
if(any(rcm_bc_2050s$snd_cm < 1)) {
  rcm_bc_2050s$snd_cm[which(rcm_bc_2050s$snd_cm < 1)] = 0
}
# 2090s
rcm_bc_2090s = data.frame(MBCout_2090s[[2]])
colnames(rcm_bc_2090s) = ratio_trace_df$varnames
# rain should be >= 0.1
if(any(rcm_bc_2090s$rain_mm < 0.1)) {
  rcm_bc_2090s$rain_mm[which(rcm_bc_2090s$rain_mm < 0.1)] = 0
}
# wdr should be 0-360
if(any(rcm_bc_2090s$wdr_deg < 0)) {
  rcm_bc_2090s$wdr_deg[which(rcm_bc_2090s$wdr_deg < 0)]=
    360 + rcm_bc_2090s$wdr_deg[which(rcm_bc_2090s$wdr_deg < 0)]
}
if(any(rcm_bc_2090s$wdr_deg >= 360)) {
  rcm_bc_2090s$wdr_deg[which(rcm_bc_2090s$wdr_deg >= 360)] =
    rcm_bc_2090s$wdr_deg[which(rcm_bc_2090s$wdr_deg >= 360)] - 360
}
# wsp should be >=0
if(any(rcm_bc_2090s$wsp_mpers < 0)) {
  rcm_bc_2090s$wsp_mpers[which(rcm_bc_2090s$wsp_mpers < 0)] = 0
}
# rsds should be >=0
if(any(rcm_bc_2090s$rsds_kjperm2 < 0)) {
  rcm_bc_2090s$rsds_kjperm2[which(rcm_bc_2090s$rsds_kjperm2 < 0)] = 0
}
# ps should be >=0
if(any(rcm_bc_2090s$ps_pa < 0)) {
  rcm_bc_2090s$ps_pa[which(rcm_bc_2090s$ps_pa < 0)] = 0
}
# hurs should be 0-100
if(any(rcm_bc_2090s$hurs_per < 0)) {
  rcm_bc_2090s$hurs_per[which(rcm_bc_2090s$hurs_per < 0)] = 0
}
if(any(rcm_bc_2090s$hurs_per > 100)) {
  rcm_bc_2090s$hurs_per[which(rcm_bc_2090s$hurs_per > 100)] = 100
}
# clt should be 0-100
if(any(rcm_bc_2090s$clt_per < 0)) {
  rcm_bc_2090s$clt_per[which(rcm_bc_2090s$clt_per < 0)] = 0
}
if(any(rcm_bc_2090s$clt_per > 100)) {
  rcm_bc_2090s$clt_per[which(rcm_bc_2090s$clt_per > 100)] = 100
}
# snd should be >=1
if(any(rcm_bc_2090s$snd_cm < 1)) {
  rcm_bc_2090s$snd_cm[which(rcm_bc_2090s$snd_cm < 1)] = 0
}

# check out if bias correction has worked
obs_cal = data.frame(obs_cal)
colnames(obs_cal) = ratio_trace_df$varnames
rcm_cal = data.frame(rcm_cal)
colnames(rcm_cal) = ratio_trace_df$varnames
# summarize data
summary(obs_cal)
summary(rcm_cal)
summary(rcm_bc_cal)

# data comparison plots ####
ggplot()+
  geom_density(data = obs_cal,aes(x = tas_degc), color = "grey50", size = 6) +
  geom_density(data = rcm_cal,aes(x = tas_degc), color = "blue", size = 2) +
  geom_density(data = rcm_bc_cal, aes(x = tas_degc), color = "red", size = 2) +
  xlab("Temperature (°C)") +
  ylab("Density") +
  theme_bw() +
  theme(legend.position = "none") +
  theme(axis.title.x = element_text(size = 21, face = "bold", vjust = 1),
        axis.title.y = element_text(size = 21, face = "bold"),
        axis.text.x = element_text(size = 21),
        axis.text.y = element_text(size = 21),
        legend.title = element_text(size = 21, face = "bold"),
        legend.text = element_text(size=21), text = element_text(size = 21))
ggsave("./outputs/tas_degc_obs_rcm_bcrcm.png", width = 12, height = 10)

ggplot()+
  geom_density(data = obs_cal, aes(x = wsp_mpers), color = "grey50", size = 6) +
  geom_density(data = rcm_cal, aes(x = wsp_mpers), color = "blue", size = 2) +
  geom_density(data = rcm_bc_cal, aes(x = wsp_mpers), color = "red", size = 2) +
  xlab("Temperature (?C)") +
  ylab("Density") +
  theme_bw() +
  theme(legend.position = "none")+
  theme(axis.title.x = element_text(size = 21, face = "bold", vjust = 1),
        axis.title.y = element_text(size = 21, face = "bold"),
        axis.text.x = element_text(size = 21),
        axis.text.y = element_text(size = 21),
        legend.title = element_text(size = 21, face = "bold"),
        legend.text = element_text(size = 21), text = element_text(size = 21))
ggsave("./outputs/wsp_mpers_obs_rcm_bcrcm.png", width = 12, height = 10)

# impact on projected changes
summary(rcm_bc_2010s)
summary(rcm_bc_2050s)
summary(rcm_bc_2090s)

ggplot() +
  geom_density(data = rcm_bc_2010s, aes(x = tas_degc), color = "black", size = 2) +
  geom_density(data = rcm_bc_2050s, aes(x = tas_degc), color = "blue", size = 2) +
  geom_density(data = rcm_bc_2090s, aes(x = tas_degc), color = "red", size = 2) +
  xlab("Temperature (°C)") +
  ylab("Density") +
  theme_bw() +
  theme(legend.position = "none")+
  theme(axis.title.x = element_text(size = 21, face = "bold", vjust = 1),
        axis.title.y = element_text(size = 21, face = "bold"),
        axis.text.x = element_text(size = 21),
        axis.text.y = element_text(size = 21),
        legend.title = element_text(size = 21, face = "bold"),
        legend.text = element_text(size = 21), text = element_text(size = 21))
ggsave("./outputs/tas_degc_2010s_2050s_2090s.png", width = 12, height = 10)

ggplot()+
  geom_density(data = rcm_bc_2010s, aes(x = wsp_mpers), color = "black", size = 2) +
  geom_density(data = rcm_bc_2050s, aes(x = wsp_mpers), color = "blue", size = 2) +
  geom_density(data = rcm_bc_2090s, aes(x = wsp_mpers), color = "red", size = 2) +
  xlab("Temperature (°C)") +
  ylab("Density") +
  theme_bw() +
  theme(legend.position = "none") +
  theme(axis.title.x = element_text(size = 21, face = "bold", vjust = 1),
        axis.title.y = element_text(size = 21, face = "bold"),
        axis.text.x = element_text(size = 21),
        axis.text.y = element_text(size = 21),
        legend.title = element_text(size = 21, face = "bold"),
        legend.text = element_text(size = 21), text = element_text(size = 21))
ggsave("./outputs/wsp_mpers_2010s_2050s_2090s.png", width = 12, height = 10)
