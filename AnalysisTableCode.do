clear

cd "C:\Users\Hirofumi Kurokawa\Box\201710_オキシトシン実験\Paper\TrustGame\Submission\FNeuroendocrine\Data_Code"

use Data\Data.dta, replace

****************************************
**3.1 MU transfers following oxytocin or placebo: Trust game and Risk game
*t-test
ttest  transfer_ave, by(oxt) //  OXT vs Placebo
ttest  transfer_risk_ave, by(oxt) //  OXT vs Placebo

**Fisher exact test
tab transfer_ave_all group_a, all exact
tab transfer_risk_ave_all group_a, all exact

*t-test: Trust vs Risk
ttest dif_transfer, by(oxt)

*t-test
use Data\Data_receive.dta, replace
ttest  return if receive==0, by(oxt) //  OXT vs Placebo
ttest  return if receive==4, by(oxt) //  OXT vs Placebo
ttest  return if receive==8, by(oxt) //  OXT vs Placebo
ttest  return if receive==12, by(oxt) //  OXT vs Placebo

*****************************************
use Data\Data.dta, replace

**3.2 Variance in sensitivity to oxytocin administration
*Fisher
tab transfer_ave_all group_a if subg_sensitive_dif==1, all exact // High sensitive VS Placebo
tab transfer_ave_all group_a if subg_sensitive_dif2==1, all exact // Low sensitive VS Placebo 

*t-test
ttest transfer_ave if subg_sensitive_dif==1, by(oxt) // High sensitive VS Placebo
ttest transfer_ave if subg_sensitive_dif2==1, by(oxt) // Low sensitive VS Placebo 

ttest transfer_risk_ave if subg_sensitive_dif==1, by(oxt) // High sensitive VS Placebo
ttest transfer_risk_ave if subg_sensitive_dif2==1, by(oxt) // Low sensitive VS Placebo 

*Multiple comparison
pwmean transfer_ave, over(group_a subg_sensitive_dif) mcompare(bon) effect
pwmean transfer_ave, over(group_a subg_sensitive_dif2) mcompare(bon) effect


****************************************
**3.3 Variance in baseline oxytocin levels
** Figure 4: Initial level of oxytocin
ttest oxytocin1_log, by(group_a)
ksmirnov oxytocin1_log, by(group_a)

ttest oxytocin2_log, by(group_a)
ttest oxytocin3_log, by(group_a)


****************************************
**3.4 Oxytocin time course
**Before administration VS After RG
ttest oxytocin1_log=oxytocin3_log if group_a==0
ttest oxytocin1_log=oxytocin3_log if group_a==1

**Before TG VS After RG
ttest oxytocin2_log=oxytocin3_log if group_a==0
ttest oxytocin2_log=oxytocin3_log if group_a==1

**Before administration VS Before TG
ttest oxytocin1_log=oxytocin2_log if group_a==0
ttest oxytocin1_log=oxytocin2_log if group_a==1

****************************************
**3.5 Oxytocin and trust behavior in relation to AQ score
*t-test
ttest aq, by(oxt) // Difference of AQ between OXT and Placebo

*t-test
ttest  transfer_ave if as26==0, by(oxt) // Lower AQ (AQ<26):  OXT vs Placebo
ttest  transfer_ave if as26==1, by(oxt) // Higher AQ (AQ>=26) : OXT vs Placebo

**Fisher exact test
tab transfer_ave_all group_a if as26==0, all exact
tab transfer_ave_all group_a if as26==1, all exact

*t-test
ttest  transfer_ave if oxt=="Oxytocin", by(as26) // Oxytocin group: Lower AQ vs Higher AQ
ttest  transfer_ave if oxt=="Placebo", by(as26) // Placebo group: Lower AQ vs Higher AQ

*Multiple compatison
pwmean transfer_ave, over(group_a as26) mcompare(bon) effect

**Oxytocin and trust behavior in relation to EQ score
*t-test
ttest eq, by(oxt) // Difference of EQ between OXT and Placebo

*t-test
ttest  transfer_ave if eq_h==0, by(oxt) // Lower EQ (EQ<16):  OXT vs Placebo
ttest  transfer_ave if eq_h==1, by(oxt) // Higher EQ (EQ>=16) : OXT vs Placebo

**Fisher exact test
tab transfer_ave_all group_a if eq_h==0, all exact
tab transfer_ave_all group_a if eq_h==1, all exact

*t-test
ttest  transfer_ave if oxt=="Oxytocin", by(eq_h) // Oxytocin group: Lower EQ vs Higher EQ
ttest  transfer_ave if oxt=="Placebo", by(eq_h) // Placebo group: Lower EQ vs Higher EQ


**Oxytocin and trust behavior in relation to SQ score
*t-test
ttest sq, by(oxt) // Difference of SQ between OXT and Placebo

*t-test
ttest  transfer_ave if sq_h==0, by(oxt) // Lower SQ (SQ<16):  OXT vs Placebo
ttest  transfer_ave if sq_h==1, by(oxt) // Higher SQ (SQ>=16) : OXT vs Placebo

**Fisher exact test
tab transfer_ave_all group_a if sq_h==0, all exact
tab transfer_ave_all group_a if sq_h==1, all exact

*t-test
ttest  transfer_ave if oxt=="Oxytocin", by(sq_h) // Oxytocin group: Lower SQ vs Higher SQ
ttest  transfer_ave if oxt=="Placebo", by(sq_h) // Placebo group: Lower SQ vs Higher SQ

****************************************
**Table 1: Mean and median transfer behaviour of investors
sum  transfer_ave transfer_risk_ave if oxt=="Oxytocin"
sum  transfer_ave transfer_risk_ave if oxt=="Placebo"

sum  transfer_ave transfer_risk_ave if oxt=="Oxytocin", detail
sum  transfer_ave transfer_risk_ave if oxt=="Placebo", detail

**Table 2: Mean and median transfer behaviour of investors by AQ
**Lower AQ (AQ<26)
sum  transfer_ave transfer_risk_ave if oxt=="Oxytocin" & as26==0
sum  transfer_ave transfer_risk_ave if oxt=="Placebo" & as26==0

sum  transfer_ave transfer_risk_ave if oxt=="Oxytocin" & as26==0, detail
sum  transfer_ave transfer_risk_ave if oxt=="Placebo" & as26==0, detail

**Higher AQ (AQ>=26)
sum  transfer_ave transfer_risk_ave if oxt=="Oxytocin" & as26==1
sum  transfer_ave transfer_risk_ave if oxt=="Placebo" & as26==1

sum  transfer_ave transfer_risk_ave if oxt=="Oxytocin" & as26==1, detail
sum  transfer_ave transfer_risk_ave if oxt=="Placebo" & as26==1, detail



********************************************************************************
**Equivalent test
*All
tostt transfer_ave, by(oxt)

*OT Seinsitivity
tostt transfer_ave if subg_sensitive_dif==1, by(oxt) // High sensitive VS Placebo
tostt transfer_ave if subg_sensitive_dif2==1, by(oxt) // Low sensitive VS Placebo 

*AQ
tostt  transfer_ave if as26==0, by(oxt) // Lower AQ (AQ<26):  OXT vs Placebo
tostt  transfer_ave if as26==1, by(oxt) // Higher AQ (AQ>=26) : OXT vs Placebo


*******************************************************************************
**Effect size
*All
esize twosample transfer_ave, by(oxt)
bootstrap r(d) r(g), reps(200) nowarn seed(111):esize twosample transfer_ave, by(oxt)


*OT Seinsitivity
esize twosample transfer_ave if subg_sensitive_dif==1, by(oxt) // High sensitive VS Placebo
bootstrap r(d) r(g), reps(200) nowarn seed(111):esize twosample transfer_ave if subg_sensitive_dif==1, by(oxt)
esize twosample transfer_ave if subg_sensitive_dif2==1, by(oxt) // Low sensitive VS Placebo 
bootstrap r(d) r(g), reps(200) nowarn seed(111):esize twosample transfer_ave if subg_sensitive_dif2==1, by(oxt) 

*AQ
esize twosample transfer_ave if as26==0, by(oxt) // Lower AQ (AQ<26):  OXT vs Placebo
bootstrap r(d) r(g), reps(200) nowarn seed(111):esize twosample transfer_ave if as26==0, by(oxt)
esize twosample transfer_ave if as26==1, by(oxt) // Higher AQ (AQ>=26) : OXT vs Placebo
bootstrap r(d) r(g), reps(200) nowarn seed(111):esize twosample transfer_ave if as26==1, by(oxt)

*******************************************************************************
**Minimum detective effect size
*All
power twomeans 6.919667, n(96) power(0.8) direction(upper)

*OT Seinsitivity
power twomeans 6.919667, n1(48) n2(36) power(0.8) direction(upper) // High sensitive VS Placebo
power twomeans 6.919667, n1(48) n2(12) power(0.8) direction(upper) // Low sensitive VS Placebo 

*AQ
power twomeans 6.947368, n1(38) n2(41) power(0.8) direction(upper) // Lower AQ > (AQ<26):  OXT vs Placebo
power twomeans 6.8, n1(7) n2(10) power(0.8) direction(upper) // Higher AQ (AQ>=> 26) : OXT vs Placebo

*******************************************************************************
**Power calc
*All
ttest transfer_ave, by(oxt)
power twomeans 6.916667 8.125, n(96) sd1(4.276772) sd2(4.169928)

*OT Seinsitivity
ttest transfer_ave if subg_sensitive_dif==1, by(oxt) // High sensitive VS Placebo
power twomeans 6.916667 8.36111, n(84) sd1(4.276772) sd2(4.15522)

ttest transfer_ave if subg_sensitive_dif2==1, by(oxt) // Low sensitive VS Placebo
power twomeans 6.916667 7.416667, n(60) sd1(4.276772) sd2(4.316108)

*AQ
ttest transfer_ave if as26==0, by(oxt) // Lower AQ (AQ<26):  OXT vs Placebo
power twomeans 6.947368 8.609756, n(79) sd1(4.110219) sd2(4.22315)

ttest  transfer_ave if as26==1, by(oxt) // Higher AQ (AQ>=26) : OXT vs Placebo
power twomeans 6.8 5.285714, n(17) sd1(4.709329) sd2(3.545621)


