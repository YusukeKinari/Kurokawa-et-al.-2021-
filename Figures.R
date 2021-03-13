setwd("C:/Users/Kurokawa/Box Sync/201710_オキシトシン実験/Paper/TrustGame/Submission/FNeuroendocrine/Data_Code") 

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

# Figure 1 Transfer ----------------------------------------------
Fig1_transfer_trust<-ggplot(data_oxt, aes(x = oxt, y = transfer_ave)) +
  geom_boxplot()+
  geom_jitter(, width=0.25)+
  theme_classic()+
  labs(title="(a) Trust experiment", y="Transfer (MU)", x="Group")

Fig1_transfer_risk<-
ggplot(data_oxt, aes(x = oxt, y = transfer_risk_ave)) +
  geom_boxplot()+
  geom_jitter(, width=0.25)+
  theme_classic()+
  labs(title="(b) Risk experiment", y="Transfer (MU)", x="Group")


Fig1 <- gridExtra::grid.arrange(Fig1_transfer_trust, Fig1_transfer_risk, nrow = 1)
ggsave(file="Figures/Fig1_transfer.tiff", Fig1)

# Figure 2 Transfer: Relative frequency ----------------------------------------
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
  scale_y_continuous(limits = c(0, 0.6))+
  scale_x_continuous(breaks=seq(0,12,by=1),limits=c(0,12))+
  labs(title="(a) Trust game", y="Relative frequency", x="Average transfer per investor (MU)")+  
  theme_classic()+
  scale_fill_grey()

data_freq_risk <-
  data_freq %>%
  filter(Game== "Risk")
Fig2_transfer_risk<-
  ggplot(data_freq_risk, aes(x =  transfer, y = freqency, fill = Group))+
  geom_bar(stat = "identity", position = "dodge")+
  scale_y_continuous(limits = c(0, 0.6))+
  scale_x_continuous(breaks=seq(0,12,by=1),limits=c(0,12))+
  labs(title="(b) Risk game", y="Relative frequency", x="Average transfer per investor (MU)")+  
  theme_classic()+
  scale_fill_grey()

Fig2 <- gridExtra::grid.arrange(Fig2_transfer_trust, Fig2_transfer_risk, nrow = 2)
ggsave(file="Figures/Fig2_transfer_RF.tiff", Fig2)


# Figure 3 Back Transfer ----------------------------------------------
data_oxt_recieve <- read_dta("Data/Data_receive.dta")
labeli <- as_labeller(c(`0` = "Transfer = 0",
                        `4` = "Transfer = 4",
                        `8` = "Transfer = 8",
                        `12` = "Transfer = 12"))

ggplot(data_oxt_recieve, aes(x = oxt, y = return)) +
  geom_boxplot()+
  geom_jitter(, width=0.25)+
  facet_wrap(~receive, nrow = 1, labeller = labeli)+
  labs(title="Trust experiment", y="Back transfer from the trustee (MU)", x="Group")+
  theme_classic()+
  ggsave(file="Figures/Fig3_back_transfer.tiff")


# Figure 4 Administration-induced increases in the concentrations of oxytocin ----
ggplot(data_oxt, aes(x = oxt, y = lnoxt_diff31)) +
  geom_boxplot()+
  geom_jitter(, width=0.25)+
  theme_classic()+
  labs(title="Administration-induced increases in the concentrations of oxytocin", subtitle="After RE (72 minutes) - Before administration",y="Logarithmic oxytocin level (pg/ml)", x="Group")+
  ggsave(file="Figures/Fig4_OTdiff.tiff")

# Figure 5 Transfer: Relative frequency by OT Sensitivity ----------------------------------------
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
  scale_y_continuous(limits = c(0, 0.8))+
  scale_x_continuous(breaks=seq(0,12,by=1),limits=c(0,12))+
  labs(title="(a) Trust game", y="Relative frequency", x="Average transfer per investor (MU)")+  
  theme_classic()+
  scale_fill_grey()

data_freq_ot_risk <-
  data_freq_ot %>%
  filter(Game== "Risk")
Fig5_transfer_risk<-
  ggplot(data_freq_ot_risk, aes(x =  transfer, y = freqency, fill = Group))+
  geom_bar(stat = "identity", position = "dodge")+
  scale_y_continuous(limits = c(0, 0.8))+
  scale_x_continuous(breaks=seq(0,12,by=1),limits=c(0,12))+
  labs(title="(b) Risk game", y="Relative frequency", x="Average transfer per investor (MU)")+
  theme_classic()+
  scale_fill_grey()

Fig5 <- gridExtra::grid.arrange(Fig5_transfer_trust, Fig5_transfer_risk, nrow = 2)
ggsave(file="Figures/Fig5_transfer_RF.tiff", Fig5)

# Figure 6 Transfer by OT Sensitivity ----------------------------------------------
Fig6_transfer_trust<-
  ggplot(data_oxt, aes(x = oxt_sensitivity, y = transfer_ave)) +
  geom_boxplot()+
  geom_jitter(, width=0.25)+
  theme_classic()+
  labs(title="(a) Trust experiment", y="Transfer (MU)", x="Group")+
  theme(axis.text.x = element_text(angle = 25))

Fig6_transfer_risk<-
  ggplot(data_oxt, aes(x = oxt_sensitivity, y = transfer_risk_ave)) +
  geom_boxplot()+
  geom_jitter(, width=0.25)+
  theme_classic()+
  labs(title="(b) Risk experiment", y="Transfer (MU)", x="Group")+
  theme(axis.text.x = element_text(angle = 25))

Fig6 <- gridExtra::grid.arrange(Fig6_transfer_trust, Fig6_transfer_risk, nrow = 1)
ggsave(file="Figures/Fig6_transfer_OTSensitivity.tiff", Fig6)



# Figure 7 Oxtocin Time Course ----------------------------------------------
Fig7_before_ad<-
  ggplot(data_oxt, aes(x = oxt, y = oxytocin1_log)) +
  geom_boxplot()+
  geom_jitter(, width=0.25)+
  scale_y_continuous(limits = c(2, 8))+
  theme_classic()+
  labs(title="Before administration", subtitle="",y="Logarithmic oxytocin level (pg/ml)", x="Group")

Fig7_before_tg<-
  ggplot(data_oxt, aes(x = oxt, y = oxytocin2_log)) +
  geom_boxplot()+
  geom_jitter(, width=0.25)+
  scale_y_continuous(limits = c(2, 8))+
  theme_classic()+
  labs(title="Before TG", subtitle="60 minutes after administration", y="Logarithmic oxytocin level (pg/ml)", x="Group")

Fig7_after_rg<-
  ggplot(data_oxt, aes(x = oxt, y = oxytocin3_log)) +
  geom_boxplot()+
  geom_jitter(, width=0.25)+
  scale_y_continuous(limits = c(2, 8))+
  theme_classic()+
  labs(title="After RE", subtitle="72 minutes after administration", y="Logarithmic oxytocin level (pg/ml)", x="Group")

Fig7 <- gridExtra::grid.arrange(Fig7_before_ad, Fig7_before_tg, Fig7_after_rg, nrow = 1)
ggsave(file="Figures/Fig7_OT_courese.tiff", Fig7)



# Figure 8 Oxytocin Time Course by AQ ----------------------------------------------
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


