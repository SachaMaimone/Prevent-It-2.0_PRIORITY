################################################################################

# Prevent It 2.0 -- Additional Analyses

################################################################################

# Set up -----------------------------------------------------------------------

packages <- c("lme4", "lmerTest", "ggplot")

lapply(packages, library, character.only = TRUE)

# Data analysis ----------------------------------------------------------------

# This script is written with the assumption that the data have already been
# wrangled into suitable structures.

## Preliminary note ------------------------------------------------------------

# For all analyses using mixed-effects models, we will not impute missing data.

# This script does not provide exhaustive documentation of all the additional
# and exploratory analyses we will conduct. Rather, it is provided to indicate
# the planned additional analyses prior to data collection.

# Additional research questions ------------------------------------------------

### To what extent does the treatment produce sustainable changes in sexual urges (follow-up measure)?

# To assess the extent to which the treatment produces a sustainable change in
# problematic sexual urges, using the total SSAS scores, we will estimate the
# change from the post-treatment measure (or in the case of the waitlist, the
# last weekly) to the follow-up measure, controlling for the first (i.e.,
# baseline) measure.
#
# For SSAS scores, we will fit linear mixed effects models, with treatment group
# (treatment vs. waitlist, dummy coded) as a fixed predictor, measurement point
# (last weekly measure vs. follow-up, dummy coded) as a fixed predictor,
# baseline measure as a fixed predictor, and random intercepts for each
# participant. We will fit two models for each variable, one with only main
# effects and one that adds the interaction term for the treatment and
# measurement predictors. These models will be compared using a likelihood ratio
# test.
#
# A significant interaction such that the treatment group's slope from post-
# treatment to followup is smaller (i.e., less positive) than the  waitlist's 
# slope would provide support for the sustained effectiveness of the treatment.
# As long as the primary analyses also support the effectiveness of the treatment,
# a non-significant interaction would suggest that there may not be sustained change
# in the primary outcome at follow-up (4 weeks after completion of the program) given
# that this would show no difference in waitlist and treatment slopes representing 
# change from post-treatment to follow-up.

lmm_ssas_total_followup_main <- lmer(ssas_total ~ treatment + measurement + baseline + (1|id) + (1 + measurement + baseline|language), data = pi_data_csam_hours_sust)
lmm_ssas_total_followup_int  <- lmer(ssas_total ~ treatment * measurement + baseline + (1|id) + (1 + measurement + baseline|language), data = pi_data_csam_hours_sust)
lrt_ssas_total_followup      <- anova(lmm_ssas_total_followup_main, lmm_ssas_total_followup_int, test = "LRT")


## Is there a change in acute risk?

# The ACUTE-2007-SR is another outcome that will be analyzed to assess 
# partiicipants' acute dynamic risk to engage in child sexual exploitation or abuse 
# It is a self-reported version of the ACUTE-2007 that is usually file coded
# A composite, continuous, risk score is created out of the ACUTE-2007-SR items
# This measure will be assessed during the weekly assessments and therefore
# will be measured across 9 time points (i.e., 8 weekly assessments and 1 
# assessment at post-treatment) -- this administration is the same
# for treatment and control groups during the 26-week period.

# We will address this research question with models highly similar to those
# used for the primary research question.

lmm_acute_linear      <- lmer(acute_total ~ treatment + time + time_after + (1|id) + (1 + time + time_after|language), data = pi_data_long)
lmm_acute_quad        <- lmer(acute_total ~ treatment + time + time_after + time_sq + time_after_sq + (1|id) + (1 + time + time_after + time_sq + time_after_sq|language), data = pi_data_long)
lrt_acute             <- anova(lmm_acute_linear, lmm_acute_quad, test = "LRT")


### How severe are the side-effects of the treatment?

# Negative side effects are measured by the NEQ-20, which is administered to
# participants post-treatment.

## For present trial

# We will examine participants responses on the NEQ-20 descriptively, both the
# total sum score as well as the subscales in each language (German, Swedish, and Portuguese).


### Is there a change in dynamic risk factors related to child sexual exploitation and abuse?

# The following constructs and measures were assessed at pre-treatment (week 1) and post-treatment (week 9)
# As in the main analyses these models will include treatment (treatment vs. waitlist, dummy coded), 
# time (pre-treatment assessment vs. post-treatment assessment, dummy coded), 
# and time_after, assessing time-since-treatment (starting at 0, and counting up at each measurement 
# point after treatment begins) as fixed effects variables 
# and a random effects id variable with random intercepts for each participant 
# the models will also include a random effects language variable (German, Swedish, Portuguese) 
# with random intercepts and random slopes for each language.

## change in sexual preoccupation (sbims_a)

lmm_sbims_a_linear      <- lmer(sbims_a_total ~ treatment + time + time_after + (1|id) + (1 + time + time_after|language), data = pi_data_long)
lmm_sbims_a_quad        <- lmer(sbims_a_total ~ treatment + time + time_after + time_sq + time_after_sq + (1|id) + (1 + time + time_after + time_sq + time_after_sq|language), data = pi_data_long)
lrt_sbims_a             <- anova(lmm_sbims_a_linear, lmm_sbims_a_quad, test = "LRT")

## change in sexual self-control (hbi_cont)

lmm_hbi_cont_linear      <- lmer(hbi_cont_total ~ treatment + time + time_after + (1|id) + (1 + time + time_after|language), data = pi_data_long)
lmm_hbi_cont_quad        <- lmer(hbi_cont_total ~ treatment + time + time_after + time_sq + time_after_sq + (1|id) + (1 + time + time_after + time_sq + time_after_sq|language), data = pi_data_long)
lrt_hbi_cont             <- anova(lmm_hbi_cont_linear, lmm_hbi_cont_quad, test = "LRT")

## change in sexualized coping (hbi_cope)

lmm_hbi_cope_linear      <- lmer(hbi_cope_total ~ treatment + time + time_after + (1|id) + (1 + time + time_after|language), data = pi_data_long)
lmm_hbi_cope_quad        <- lmer(hbi_cope_total ~ treatment + time + time_after + time_sq + time_after_sq + (1|id) + (1 + time + time_after + time_sq + time_after_sq|language), data = pi_data_long)
lrt_hbi_cope             <- anova(lmm_hbi_cope_linear, lmm_hbi_cope_quad, test = "LRT")

## change in emotional identification with children (cecwc)

lmm_cecwc_linear      <- lmer(cecwc_total ~ treatment + time + time_after + (1|id) + (1 + time + time_after|language), data = pi_data_long)
lmm_cecwc_quad        <- lmer(cecwc_total ~ treatment + time + time_after + time_sq + time_after_sq + (1|id) + (1 + time + time_after + time_sq + time_after_sq|language), data = pi_data_long)
lrt_cecwc             <- anova(lmm_cecwc_linear, lmm_cecwc_quad, test = "LRT")

## change in offense-supportive cognitions (swch)

lmm_swch_linear      <- lmer(swch_total ~ treatment + time + time_after + (1|id) + (1 + time + time_after|language), data = pi_data_long)
lmm_swch_quad        <- lmer(swch_total ~ treatment + time + time_after + time_sq + time_after_sq + (1|id) + (1 + time + time_after + time_sq + time_after_sq|language), data = pi_data_long)
lrt_swch             <- anova(lmm_swch_linear, lmm_swch_quad, test = "LRT")

## change in alcohol use (quant_freq)

lmm_quant_freq_linear      <- lmer(quant_freq_total ~ treatment + time + time_after + (1|id) + (1 + time + time_after|language), data = pi_data_long)
lmm_quant_freq_quad        <- lmer(quant_freq_total ~ treatment + time + time_after + time_sq + time_after_sq + (1|id) + (1 + time + time_after + time_sq + time_after_sq|language), data = pi_data_long)
lrt_quant_freq             <- anova(lmm_quant_freq_linear, lmm_quant_freq_quad, test = "LRT")

## change in negative affect (phq_9)

lmm_phq_9_linear      <- lmer(phq_9_total ~ treatment + time + time_after + (1|id) + (1 + time + time_after|language), data = pi_data_long)
lmm_phq_9_quad        <- lmer(phq_9_total ~ treatment + time + time_after + time_sq + time_after_sq + (1|id) + (1 + time + time_after + time_sq + time_after_sq|language), data = pi_data_long)
lrt_phq_9             <- anova(lmm_phq_9_linear, lmm_phq_9_quad, test = "LRT")

## change in motivation for change (rtcq)

lmm_rtcq_linear      <- lmer(rtcq_total ~ treatment + time + time_after + (1|id) + (1 + time + time_after|language), data = pi_data_long)
lmm_rtcq_quad        <- lmer(rtcq_total ~ treatment + time + time_after + time_sq + time_after_sq + (1|id) + (1 + time + time_after + time_sq + time_after_sq|language), data = pi_data_long)
lrt_rtcq             <- anova(lmm_rtcq_linear, lmm_rtcq_quad, test = "LRT")

## change in leisure/recreation time (leis_rec)

lmm_leis_rec_linear      <- lmer(leis_rec_total ~ treatment + time + time_after + (1|id) + (1 + time + time_after|language), data = pi_data_long)
lmm_leis_rec_quad        <- lmer(leis_rec_total ~ treatment + time + time_after + time_sq + time_after_sq + (1|id) + (1 + time + time_after + time_sq + time_after_sq|language), data = pi_data_long)
lrt_leis_rec             <- anova(lmm_leis_rec_linear, lmm_leis_rec_quad, test = "LRT")

## change in social connection/ loneliness (fsozuk6)

lmm_fsozuk6_linear      <- lmer(fsozuk6_total ~ treatment + time + time_after + (1|id) + (1 + time + time_after|language), data = pi_data_long)
lmm_fsozuk6_quad        <- lmer(fsozuk6_total ~ treatment + time + time_after + time_sq + time_after_sq + (1|id) + (1 + time + time_after + time_sq + time_after_sq|language), data = pi_data_long)
lrt_fsozuk6             <- anova(lmm_fsozuk6_linear, lmm_fsozuk6_quad, test = "LRT")



### To what extent does the treatment produce sustainable changes in dynamic risk variables (follow-up measure)?

# These analyses involve the same predictors as the analysis conducted assessing sustained change on the SSAS as 
# the outcome -- outlined above.

## change in sexual preoccupation (sbims_a)

lmm_sbims_a_total_followup_main <- lmer(sbims_a_total ~ treatment + measurement + baseline + (1|id) + (1 + measurement + baseline|language), data = pi_data_csam_hours_sust)
lmm_sbims_a_total_followup_int  <- lmer(sbims_a_total ~ treatment * measurement + baseline + (1|id) + (1 + measurement + baseline|language), data = pi_data_csam_hours_sust)
lrt_sbims_a_total_followup      <- anova(lmm_sbims_a_total_followup_main, lmm_sbims_a_total_followup_int, test = "LRT")

## change in sexual self-control (hbi_cont)

lmm_hbi_cont_total_followup_main <- lmer(hbi_cont_total ~ treatment + measurement + baseline + (1|id) + (1 + measurement + baseline|language), data = pi_data_csam_hours_sust)
lmm_hbi_cont_total_followup_int  <- lmer(hbi_cont_total ~ treatment * measurement + baseline + (1|id) + (1 + measurement + baseline|language), data = pi_data_csam_hours_sust)
lrt_hbi_cont_total_followup      <- anova(lmm_hbi_cont_total_followup_main, lmm_hbi_cont_total_followup_int, test = "LRT")

## change in sexualized coping (hbi_cope)

lmm_hbi_cope_total_followup_main <- lmer(hbi_cope_total ~ treatment + measurement + baseline + (1|id) + (1 + measurement + baseline|language), data = pi_data_csam_hours_sust)
lmm_hbi_cope_total_followup_int  <- lmer(hbi_cope_total ~ treatment * measurement + baseline + (1|id) + (1 + measurement + baseline|language), data = pi_data_csam_hours_sust)
lrt_hbi_cope_total_followup      <- anova(lmm_hbi_cope_total_followup_main, lmm_hbi_cope_total_followup_int, test = "LRT")

## change in emotional identification with children (cecwc)

lmm_cecwc_total_followup_main <- lmer(cecwc_total ~ treatment + measurement + baseline + (1|id) + (1 + measurement + baseline|language), data = pi_data_csam_hours_sust)
lmm_cecwc_total_followup_int  <- lmer(cecwc_total ~ treatment * measurement + baseline + (1|id) + (1 + measurement + baseline|language), data = pi_data_csam_hours_sust)
lrt_cecwc_total_followup      <- anova(lmm_cecwc_total_followup_main, lmm_cecwc_total_followup_int, test = "LRT")

## change in offense-supportive cognitions (swch)

lmm_swch_total_followup_main <- lmer(swch_total ~ treatment + measurement + baseline + (1|id) + (1 + measurement + baseline|language), data = pi_data_csam_hours_sust)
lmm_swch_total_followup_int  <- lmer(swch_total ~ treatment * measurement + baseline + (1|id) + (1 + measurement + baseline|language), data = pi_data_csam_hours_sust)
lrt_swch_total_followup      <- anova(lmm_swch_total_followup_main, lmm_swch_total_followup_int, test = "LRT")

## change in alcohol use (quant_freq)

lmm_quant_freq_total_followup_main <- lmer(quant_freq_total ~ treatment + measurement + baseline + (1|id) + (1 + measurement + baseline|language), data = pi_data_csam_hours_sust)
lmm_quant_freq_total_followup_int  <- lmer(quant_freq_total ~ treatment * measurement + baseline + (1|id) + (1 + measurement + baseline|language), data = pi_data_csam_hours_sust)
lrt_quant_freq_total_followup      <- anova(lmm_quant_freq_total_followup_main, lmm_quant_freq_total_followup_int, test = "LRT")

## change in negative affect (phq_9)

lmm_phq_9_total_followup_main <- lmer(phq_9_total ~ treatment + measurement + baseline + (1|id) + (1 + measurement + baseline|language), data = pi_data_csam_hours_sust)
lmm_phq_9_total_followup_int  <- lmer(phq_9_total ~ treatment * measurement + baseline + (1|id) + (1 + measurement + baseline|language), data = pi_data_csam_hours_sust)
lrt_phq_9_total_followup      <- anova(lmm_phq_9_total_followup_main, lmm_phq_9_total_followup_int, test = "LRT")

## change in motivation for change (rtcq)

lmm_rtcq_total_followup_main <- lmer(rtcq_total ~ treatment + measurement + baseline + (1|id) + (1 + measurement + baseline|language), data = pi_data_csam_hours_sust)
lmm_rtcq_total_followup_int  <- lmer(rtcq_total ~ treatment * measurement + baseline + (1|id) + (1 + measurement + baseline|language), data = pi_data_csam_hours_sust)
lrt_rtcq_total_followup      <- anova(lmm_rtcq_total_followup_main, lmm_rtcq_total_followup_int, test = "LRT")

## change in leisure/recreation time (leis_rec)

lmm_leis_rec_total_followup_main <- lmer(leis_rec_total ~ treatment + measurement + baseline + (1|id) + (1 + measurement + baseline|language), data = pi_data_csam_hours_sust)
lmm_leis_rec_total_followup_int  <- lmer(leis_rec_total ~ treatment * measurement + baseline + (1|id) + (1 + measurement + baseline|language), data = pi_data_csam_hours_sust)
lrt_leis_rec_total_followup      <- anova(lmm_leis_rec_total_followup_main, lmm_leis_rec_total_followup_int, test = "LRT")

## change in social connection/ loneliness (fsozuk6)

lmm_fsozuk6_total_followup_main <- lmer(fsozuk6_total ~ treatment + measurement + baseline + (1|id) + (1 + measurement + baseline|language), data = pi_data_csam_hours_sust)
lmm_fsozuk6_total_followup_int  <- lmer(fsozuk6_total ~ treatment * measurement + baseline + (1|id) + (1 + measurement + baseline|language), data = pi_data_csam_hours_sust)
lrt_fsozuk6_total_followup      <- anova(lmm_fsozuk6_total_followup_main, lmm_fsozuk6_total_followup_int, test = "LRT")


### Is there a difference between treatment and control groups on static 
# risk variables related to risk of engaging in child sexual exploitation 
# and abuse across languages (German, Swedish, and Portuguese).

# All static risk variables are assessed prior to randomization at baseline (T1).

# Two-way ANOVA with interaction effect 
# Analysis will consist of a series of 2 by 3 ANOVAs (Type II with unbalanced groups) 
# with treatment group (1 = control, -1 = treatment) and 
# language version (German, Swedish, Portuguese, effect coded) on
# static risk variables related to risk of engaging in child sexual 
# exploitation and abuse behaviors (continuous DVs).
# Note that factors will be effect coded rather than the R default (i.e., treatment coded).

packages <- c("carData")

lapply(packages, library, character.only = TRUE)

# difference in prior CSA behaviors (SBIMS-Part B)

sbims_b.mod <- lm(sbims_b ~ treatment * language, data = pi_data_long, 
        contrasts=list(treatment = contr.sum, language = contr.sum))
Anova(sbims_b.mod)

# difference in paraphilic interests (lassie)

lassie.mod <- lm(lassie ~ treatment * language, data = pi_data_long, 
                  contrasts=list(treatment = contr.sum, language = contr.sum))
Anova(lassie.mod)

# difference in criminal history (prior violent, non-violent, and sexual offenses) (pri_crim)

pri_crim.mod <- lm(pri_crim ~ treatment * language, data = pi_data_long, 
                 contrasts=list(treatment = contr.sum, language = contr.sum))
Anova(pri_crim.mod)

# difference in self-reported static risk (static_99)

static_99.mod <- lm(static_99 ~ treatment * language, data = pi_data_long, 
                   contrasts=list(treatment = contr.sum, language = contr.sum))
Anova(static_99.mod)

# difference in history of relationship instability (rel_inst)

rel_inst.mod <- lm(rel_inst ~ treatment * language, data = pi_data_long, 
                    contrasts=list(treatment = contr.sum, language = contr.sum))
Anova(rel_inst.mod)

# difference in education/ employment instability (edu_empl_inst)

edu_empl_inst.mod <- lm(edu_empl_inst ~ treatment * language, data = pi_data_long, 
                   contrasts=list(treatment = contr.sum, language = contr.sum))
Anova(edu_empl_inst.mod)

# difference in alcohol abuse (audit_c)

audit_c.mod <- lm(audit_c ~ treatment * language, data = pi_data_long, 
                        contrasts=list(treatment = contr.sum, language = contr.sum))
Anova(audit_c.mod)

# difference in religion/ spirituality (rel_spir)

rel_spir.mod <- lm(rel_spir ~ treatment * language, data = pi_data_long, 
                  contrasts=list(treatment = contr.sum, language = contr.sum))
Anova(rel_spir.mod)

# difference in adverse childhood experiences (aces)

aces.mod <- lm(aces ~ treatment * language, data = pi_data_long, 
                   contrasts=list(treatment = contr.sum, language = contr.sum))
Anova(aces.mod)

# difference in autism (raads_14)

raads_14.mod <- lm(raads_14 ~ treatment * language, data = pi_data_long, 
               contrasts=list(treatment = contr.sum, language = contr.sum))
Anova(raads_14.mod)


# Will re-run analyses while controlling for any static risk variables that
# vary between treatment groups and/or language versions.

