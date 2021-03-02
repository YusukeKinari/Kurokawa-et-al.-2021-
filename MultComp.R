setwd("C:/Users/Hirofumi Kurokawa/Box/201710_オキシトシン実験/Paper/TrustGame/Submission/FNeuroendocrine/Data_Code") 

#install.packages("mixOmics")
#if (!requireNamespace("BiocManager", quietly = TRUE))
#install.packages("BiocManager")
#BiocManager::install("mixOmics")
#install.packages("RVAideMemoire")
library("RVAideMemoire")


# Multiple Comparison: Fisher ---------------------------------------------
# AQ
group_aq <- matrix(c(22,19,30,8,6,1,7,3),nrow=4, ncol=2,
                    dimnames=list( c("OT-AQ(L)","P-AQ(L)","OT-AQ(H)", "P-AQ(H)"), c("Not All Investment","All Investment")),byrow=TRUE)
group_aq
fisher.multcomp(group_aq, p.method="hochberg")


# OT Sensitivity
group_ot <- matrix(c(8,4,37,11,20,16,37,11),nrow=4, ncol=2,
                   dimnames=list( c("OT-Sensitivity(L)","P-Sensitivity(L)","OT-Sensitivity(H)", "P-Sensitivity(H)"), c("Not All Investment","All Investment")),byrow=TRUE)
group_ot
fisher.multcomp(group_ot, p.method="hochberg")

