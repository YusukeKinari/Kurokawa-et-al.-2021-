setwd("C:/Users/Hirofumi Kurokawa/Box/201710_オキシトシン実験/Paper/TrustGame/Submission/FNeuroendocrine/R2/DataCode") 
setwd("C:/Users/Kurokawa/Box Sync/201710_オキシトシン実験/Paper/TrustGame/Submission/FNeuroendocrine/R2/DataCode") 

if(!require("ggplot2")){
  install.packages("ggplot2")
}
if(!require("ggsgnif")){
  install.packages("ggsignif")
}
if(!require("Hmisc")){
  install.packages("Hmisc")
}
library(ggplot2)
library(ggsignif)
library(Hmisc)

if(!require("devtools")){
  install.packages("devtools")
}
devtools::install_github("trinker/plotflow")
library("plotflow")
library("gridExtra")


if(!require("ggthemes")){
  install.packages("ggthemes")
}
library(ggthemes)
library(tidyverse)

library(haven)
data_oxt <- read_dta("Data/Data.dta")

# Figure 1 Transfer ----------------------------------------------OK
Fig1_transfer_trust<-
  ggplot(data_oxt, aes(x = oxt, y = transfer_ave)) +
  geom_boxplot()+
  geom_jitter(, width=0.25)+
  theme_classic()+
  labs(title="(a) Trust experiment", y="Transfer (MU)", x="Group")+
  geom_signif(stat = "identity",
              data = data.frame(x = c(1.3),
                                xend = c(1.8),
                                y = c(12.5),
                                annotation = c("t = 1.40, p = 0.08")),
              aes(x = x, xend = xend, y = y, yend = y, annotation = annotation),
              col = "black")

Fig1_transfer_risk<-
 ggplot(data_oxt, aes(x = oxt, y = transfer_risk_ave)) +
  geom_boxplot()+
  geom_jitter(, width=0.25)+
  theme_classic()+
  labs(title="(b) Risk experiment", y="Transfer (MU)", x="Group")+
  geom_signif(stat = "identity",
            data = data.frame(x = c(1.3),
                              xend = c(1.8),
                              y = c(12.5),
                              annotation = c("t = 0.41, p = 0.34")),
            aes(x = x, xend = xend, y = y, yend = y, annotation = annotation),
            col = "black")


Fig1 <- gridExtra::grid.arrange(Fig1_transfer_trust, Fig1_transfer_risk, nrow = 1)
ggsave(file="Figures/Fig1_transfer.tiff", Fig1)

# Figure 2 Transfer: Relative frequency ----------------------------------------OK
data_freq <-
  read.csv("Data/Data_freq.csv", header = TRUE, fileEncoding="UTF-8-BOM")
data_freq <- 
  transform(data_freq, Group= factor(Group, levels = c("Placebo", "Oxytocin")))

data_freq_trust <-
  data_freq %>%
  filter(Game== "Trust")

Fig2_transfer_trust<-
  ggplot(data_freq_trust, aes(x =  transfer, y = freqency, fill = Group))+
  geom_bar(stat = "identity", position = "dodge")+
  scale_y_continuous(limits = c(0, 0.65))+
  scale_x_continuous(breaks=seq(0,12,by=1),limits=c(0,12.5))+
  labs(title="(a) Trust game", y="Relative frequency", x="Average transfer per investor (MU)")+  
  theme_classic()+
  scale_fill_grey()+
  annotate("text", x=12,   y=0.48, label="Fisher, p = 0.04")+
  annotate("segment", x=11.5,xend=12.5, y=0.45,yend=0.45,
             colour="black", size=0.5)


data_freq_risk <-
  data_freq %>%
  filter(Game== "Risk")
Fig2_transfer_risk<-
  ggplot(data_freq_risk, aes(x =  transfer, y = freqency, fill = Group))+
  geom_bar(stat = "identity", position = "dodge")+
  scale_y_continuous(limits = c(0, 0.65))+
  scale_x_continuous(breaks=seq(0,12,by=1),limits=c(0,12.5))+
  labs(title="(b) Risk game", y="Relative frequency", x="Average transfer per investor (MU)")+  
  theme_classic()+
  scale_fill_grey()+
  annotate("text", x=12,   y=0.63, label="Fisher, p = 0.24")+
  annotate("segment", x=11.5,xend=12.5, y=0.6,yend=0.6,
           colour="black", size=0.5)

Fig2 <- gridExtra::grid.arrange(Fig2_transfer_trust, Fig2_transfer_risk, nrow = 2)
ggsave(file="Figures/Fig2_transfer_RF.tiff", Fig2)


# Figure 3 Back Transfer ----------------------------------------------OK
data_oxt_recieve <- read_dta("Data/Data_receive.dta")
labeli <- as_labeller(c(`0` = "Transfer = 0",
                        `4` = "Transfer = 4",
                        `8` = "Transfer = 8",
                        `12` = "Transfer = 12"))

data_oxt_recieve0 <-
  data_oxt_recieve %>%
  filter(receive== 0)

Fig3_recieve0<-
ggplot(data_oxt_recieve0, aes(x = oxt, y = return)) +
  geom_boxplot()+
  geom_jitter(, width=0.25)+
  scale_y_continuous(limits = c(0, 50))+
  labs(title="Transfer=0", y="Back transfer from the trustee (MU)", x="Group")+
  theme_classic()+
  annotate("text", x=1.45,   y=17, label="t = -1.82, p = 0.04", size=2.5)+
  annotate("segment", x=1.2,xend=1.7, y=15,yend=15,
           colour="black", size=0.5)

data_oxt_recieve4 <-
  data_oxt_recieve %>%
  filter(receive== 4)

Fig3_recieve4<-
  ggplot(data_oxt_recieve4, aes(x = oxt, y = return)) +
  geom_boxplot()+
  geom_jitter(, width=0.25)+
  scale_y_continuous(limits = c(0, 50))+
  labs(title="Transfer=4", y="Back transfer from the trustee (MU)", x="Group")+
  theme_classic()+
  annotate("text", x=1.45,   y=17, label="t = -1.06, p = 0.85", size=2.5)+
  annotate("segment", x=1.2,xend=1.7, y=15,yend=15,
           colour="black", size=0.5)

data_oxt_recieve8 <-
  data_oxt_recieve %>%
  filter(receive== 8)

Fig3_recieve8<-
  ggplot(data_oxt_recieve8, aes(x = oxt, y = return)) +
  geom_boxplot()+
  geom_jitter(, width=0.25)+
  scale_y_continuous(limits = c(0, 50))+
  labs(title="Transfer=8", y="Back transfer from the trustee (MU)", x="Group")+
  theme_classic()+
  annotate("text", x=1.45,   y=22, label="t = -0.34, p = 0.63", size=2.5)+
  annotate("segment", x=1.2,xend=1.7, y=20,yend=20,
           colour="black", size=0.5)

data_oxt_recieve12 <-
  data_oxt_recieve %>%
  filter(receive== 12)

Fig3_recieve12<-
  ggplot(data_oxt_recieve12, aes(x = oxt, y = return)) +
  geom_boxplot()+
  geom_jitter(, width=0.25)+
  scale_y_continuous(limits = c(0, 50))+
  labs(title="Transfer=12", y="Back transfer from the trustee (MU)", x="Group")+
  theme_classic()+
  annotate("text", x=1.45,   y=30, label="t = -0.34, p = 0.63", size=2.5)+
  annotate("segment", x=1.2,xend=1.7, y=28,yend=28,
           colour="black", size=0.5)

Fig3 <- gridExtra::grid.arrange(Fig3_recieve0,  Fig3_recieve4, Fig3_recieve8, Fig3_recieve12, ncol = 4)
ggsave(file="Figures/Fig3_back_transfer.tiff", Fig3)


# Figure 4 Administration-induced increases in the concentrations of oxytocin ----記載不要？
ggplot(data_oxt, aes(x = oxt, y = lnoxt_diff31)) +
  geom_boxplot()+
  geom_jitter(, width=0.25)+
  theme_classic()+
  labs(title="Administration-induced increases in the concentrations of oxytocin", subtitle="After RE (72 minutes) - Before administration",y="Logarithmic oxytocin level (pg/ml)", x="Group")+
  ggsave(file="Figures/Fig4_OTdiff.tiff")

# Figure 5 Transfer: Relative frequency by OT Sensitivity ----------------------------------------OK
data_freq_ot <-
  read.csv("Data/Data_freq_ot.csv", header = TRUE, fileEncoding="UTF-8-BOM")
data_freq_ot <- 
  transform(data_freq_ot, Group= factor(Group, levels = c("Placebo", "Oxytocin: low sensitivity", "Oxytocin: high sensitivity")))

data_freq_ot_trust <-
  data_freq_ot %>%
  filter(Game== "Trust")
Fig5_transfer_trust<-
  ggplot(data_freq_ot_trust, aes(x =  transfer, y = freqency, fill = Group))+
  geom_bar(stat = "identity", position = "dodge")+
  scale_y_continuous(limits = c(0, 0.85))+
  scale_x_continuous(breaks=seq(0,12,by=1),limits=c(0,12.55))+
  labs(title="(a) Trust game", y="Relative frequency", x="Average transfer per investor (MU)")+  
  theme_classic()+
  scale_fill_grey()+
  annotate("text", x=12,   y=0.48, label="Fisher, p = 0.03", size=2.5)+
  annotate("segment", x=11.5,xend=12.5, y=0.44,yend=0.44,
           colour="black", size=0.5)+
  annotate("text", x=11,   y=0.42, label="Fisher, p = 0.34", size=2.5)+
  annotate("segment", x=11.5,xend=11.9, y=0.39,yend=0.39,
           colour="black", size=0.5)

data_freq_ot_risk <-
  data_freq_ot %>%
  filter(Game== "Risk")
Fig5_transfer_risk<-
  ggplot(data_freq_ot_risk, aes(x =  transfer, y = freqency, fill = Group))+
  geom_bar(stat = "identity", position = "dodge")+
  scale_y_continuous(limits = c(0, 0.85))+
  scale_x_continuous(breaks=seq(0,12,by=1),limits=c(0,12.5))+
  labs(title="(b) Risk game", y="Relative frequency", x="Average transfer per investor (MU)")+
  theme_classic()+
  scale_fill_grey()+
  annotate("text", x=12,   y=0.77, label="Fisher, p = 0.41", size=2.5)+
  annotate("segment", x=11.5,xend=12.5, y=0.74,yend=0.74,
           colour="black", size=0.5)+
  annotate("text", x=11,   y=0.72, label="Fisher, p = 0.11", size=2.5)+
  annotate("segment", x=11.5,xend=11.9, y=0.69,yend=0.69,
           colour="black", size=0.5)


Fig5 <- gridExtra::grid.arrange(Fig5_transfer_trust, Fig5_transfer_risk, nrow = 2)
ggsave(file="Figures/Fig5_transfer_RF.tiff", Fig5)

# Figure 6 Transfer by OT Sensitivity ----------------------------------------------OK
Fig6_transfer_trust<-
  ggplot(data_oxt, aes(x = oxt_sensitivity, y = transfer_ave)) +
  geom_boxplot()+
  geom_jitter(, width=0.25)+
  theme_classic()+
  labs(title="(a) Trust experiment", y="Transfer (MU)", x="Group")+
  geom_signif(stat = "identity",
              data = data.frame(x = c(1.2, 2.0),
                                xend = c(2.8, 2.8),
                                y = c(14, 12.5),
                                annotation = c("t = 1.55, p = 0.06", "t = 0.36, p =0.36")),
              aes(x = x, xend = xend, y = y, yend = y, annotation = annotation),
              col = "black")

Fig6_transfer_risk<-
  ggplot(data_oxt, aes(x = oxt_sensitivity, y = transfer_risk_ave)) +
  geom_boxplot()+
  geom_jitter(, width=0.25)+
  theme_classic()+
  labs(title="(b) Risk experiment", y="Transfer (MU)", x="Group")+
  geom_signif(stat = "identity",
              data = data.frame(x = c(1.2, 2.0),
                                xend = c(2.8, 2.8),
                                y = c(14, 12.5),
                                annotation = c("t = 0.13, p = 0.45", "t = 0.82, p =0.21")),
              aes(x = x, xend = xend, y = y, yend = y, annotation = annotation),
              col = "black")

Fig6 <- gridExtra::grid.arrange(Fig6_transfer_trust, Fig6_transfer_risk, nrow = 1)
ggsave(file="Figures/Fig6_transfer_OTSensitivity.tiff", Fig6)



# Figure 7 Oxtocin Time Course ----------------------------------------------OK
Fig7_before_ad<-
  ggplot(data_oxt, aes(x = oxt, y = oxytocin1_log)) +
  geom_boxplot()+
  geom_jitter(, width=0.25)+
  scale_y_continuous(limits = c(2, 8))+
  theme_classic()+
  labs(title="Before administration", subtitle="",y="Logarithmic oxytocin level (pg/ml)", x="Group")+
  geom_signif(stat = "identity",
              data = data.frame(x = c(1.2),
                                xend = c(1.8),
                                y = c(8),
                                annotation = c("t = 1.17, p = 0.12")),
              aes(x = x, xend = xend, y = y, yend = y, annotation = annotation),
              col = "black")

Fig7_before_tg<-
  ggplot(data_oxt, aes(x = oxt, y = oxytocin2_log)) +
  geom_boxplot()+
  geom_jitter(, width=0.25)+
  scale_y_continuous(limits = c(2, 8))+
  theme_classic()+
  labs(title="Before TG", subtitle="60 minutes after administration", y="Logarithmic oxytocin level (pg/ml)", x="Group")+
  geom_signif(stat = "identity",
              data = data.frame(x = c(1.2),
                                xend = c(1.8),
                                y = c(8),
                                annotation = c("t = 2.27, p = 0.02")),
              aes(x = x, xend = xend, y = y, yend = y, annotation = annotation),
              col = "black")

Fig7_after_rg<-
  ggplot(data_oxt, aes(x = oxt, y = oxytocin3_log)) +
  geom_boxplot()+
  geom_jitter(, width=0.25)+
  scale_y_continuous(limits = c(2, 8))+
  theme_classic()+
  labs(title="After RE", subtitle="72 minutes after administration", y="Logarithmic oxytocin level (pg/ml)", x="Group")+
  geom_signif(stat = "identity",
              data = data.frame(x = c(1.2),
                                xend = c(1.8),
                                y = c(8),
                                annotation = c("t = 8.90, p = 0.00")),
              aes(x = x, xend = xend, y = y, yend = y, annotation = annotation),
              col = "black")

Fig7 <- gridExtra::grid.arrange(Fig7_before_ad, Fig7_before_tg, Fig7_after_rg, nrow = 1)
ggsave(file="Figures/Fig7_OT_courese.tiff", Fig7)



# Figure 8 Oxytocin Time Course by AQ ----------------------------------------------推定結果の記載なし
data_oxt_aq_ot <- read_dta("Data/Data_aq_ot.dta")

ggplot(data_oxt_aq_ot, aes(x = group, y = lnoxt_diff31)) +
  geom_boxplot()+
  geom_jitter(, width=0.25)+
  scale_x_discrete(labels=c(expression(Oxytocin: AQ<26), expression(Oxytocin: AQ>=26), expression(Placebo: AQ<26), expression(Placebo: AQ>=26)))+
  labs(title="Administration-induced increases in the concentrations of oxytocin", 
       subtitle="After RE (72 minutes) - Before administration",
       y="Logarithmic oxytocin level (pg/ml)",
       x="Group",
       labeller = label_parsed)+
  theme_classic()+
  ggsave(file="Figures/Fig8_OTcourse_AQ.tiff")


