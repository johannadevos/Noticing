## Script for the analysis reported in the article:
## "Noticing vocabulary holes aids incidental second language word learning: An experimental study" (2018)
## Published in Language Learning
## Script written by Johanna F. de Vos

# Clear workspace
rm(list = ls())

# Libraries
library(ggplot2); library(lme4); library(lsmeans); library(mediation); 

# Set working directory to where this script is stored

# Read in data and inspect
exp <- read.table(file = "DataNoticingTheHole.txt", sep = "\t", header = TRUE)
str(exp)

# Recode
exp$Participant <- as.factor(exp$Participant)

# Remove words that were already actively known, and remove participants who had too much active knowledge
# (For two participants, the experiment had already been aborted after the pre-test due to the extent of their active knowledge)
exp <- exp[exp$KnownBeforeActive != "Yes",]
exp <- exp[exp$Participant != "29" & exp$Participant != "36" & exp$Participant != "57",]


## DESCRIPTIVE STATISTICS (PHONEME-BASED)

# Participant means
exp$Score100 <- exp$Score*100
results <- aggregate(Score100~Participant + Condition + TestingMoment, exp, mean)
results$Score100 <- round(results$Score100, digits = 2)

# Construct various aggregated datasets to calculate descriptives from
# (This means that we will calculate macro rather than micro descriptives)

# Aggregate over Condition and Testing moment
exp$CondTM <- paste0(exp$Condition, exp$TestingMoment)
agg <- aggregate(Score100 ~ Participant + Condition + TestingMoment + CondTM, exp, mean, na.action = NULL) # na.action --> participants with missing values for AttentionForWords are not removed from the datafile

# Aggregate over Condition and Passive knowledge
exp$CondPK <- paste0(exp$Condition, exp$KnownBeforePassive)
agg2 <- aggregate(Score100 ~ Participant + Condition + KnownBeforePassive + CondPK, exp, mean, na.action = NULL) # na.action --> participants with missing values for AttentionForWords are not removed from the datafile

# Descriptives Condition and Testing moment
table(agg$CondTM) # n
tapply(agg$Score100, agg$CondTM, mean)
tapply(agg$Score100, agg$CondTM, sd)
tapply(agg$Score100, agg$CondTM, shapiro.test) # All normally distributed
tapply(agg$Score100, agg$CondTM, t.test) # 95% confidence intervals

# Descriptives Testing moment
table(agg$TestingMoment) # n
tapply(agg$Score100, agg$TestingMoment, mean)
tapply(agg$Score100, agg$TestingMoment, sd)
tapply(agg$Score100, agg$TestingMoment, shapiro.test) # All normally distributed
tapply(agg$Score100, agg$TestingMoment, t.test) # 95% confidence intervals

# Descriptives Condition and Passive knowledge
table(agg2$CondPK) # n
tapply(agg2$Score100, agg2$CondPK, mean)
tapply(agg2$Score100, agg2$CondPK, sd)
tapply(agg2$Score100, agg2$CondPK, shapiro.test) # Not all normally distributed
tapply(agg2$Score100, agg2$CondPK, t.test) # 95% confidence intervals

# Descriptives Passive knowledge
table(agg2$KnownBeforePassive) # n (Not all participants reported active knowledge of words)
tapply(agg2$Score100, agg2$KnownBeforePassive, mean)
tapply(agg2$Score100, agg2$KnownBeforePassive, sd)
tapply(agg2$Score100, agg2$KnownBeforePassive, shapiro.test) # Not all normally distributed
tapply(agg2$Score100, agg2$KnownBeforePassive, t.test) # 95% confidence intervals

# How many responses in each condidition and testing moment are correct/partially correct/incorrect?
tab <- table(exp$Correct, exp$Condition, exp$TestingMoment); tab
ftable(tab)


## COMPARE PARTICIPANTS BETWEEN CONDITIONS

part <- aggregate(cbind(Age, Lextale, ProficiencySelf, YearsOfDutch, Exposure, OtherLanguages, BigVocabulary, OnlyMeaning, UnknownWords, SmallDifferences, KnownBeforePassive) ~ Participant + Condition, exp, mean, na.action = NULL)

# Means
tapply(part$Age, part$Condition, mean); tapply(part$Age, part$Condition, sd)
tapply(part$Lextale, part$Condition, mean); tapply(part$Lextale, part$Condition, sd)
tapply(part$ProficiencySelf, part$Condition, mean); tapply(part$ProficiencySelf, part$Condition, sd)
tapply(part$YearsOfDutch, part$Condition, mean); tapply(part$YearsOfDutch, part$Condition, sd)
tapply(part$Exposure, part$Condition, mean); tapply(part$Exposure, part$Condition, sd)
tapply(part$OtherLanguages, part$Condition, mean); tapply(part$OtherLanguages, part$Condition, sd)

tapply(part$BigVocabulary, part$Condition, mean); tapply(part$BigVocabulary, part$Condition, sd) # "It is important to me to have a large Dutch vocabulary"
tapply(part$OnlyMeaning, part$Condition, mean); tapply(part$OnlyMeaning, part$Condition, sd) # "The way in which something is said is not important to me, only what it means"
tapply(part$UnknownWords, part$Condition, mean); tapply(part$UnknownWords, part$Condition, sd) # "When I hear a Dutch word I don't know, I try to learn it"
tapply(part$SmallDifferences, part$Condition, mean); tapply(part$SmallDifferences, part$Condition, sd) # "I pay attention to subtle differences between German and Dutch"

# One-way anova's
anAge <- lm(Age~Condition, data = part); summary(anAge)
anYears <- lm(YearsOfDutch~Condition, data = part); summary(anYears)
anProf <- lm(ProficiencySelf~Condition, data = part); summary(anProf)
anExp <- lm(Exposure~Condition, data = part); summary(anExp)
anOL <- lm(OtherLanguages~Condition, data = part); summary(anOL)
anLex <- lm(Lextale~Condition, data = part); summary(anLex)

anBig <- lm(BigVocabulary~Condition, data = part); summary(anBig)
anMea <- lm(OnlyMeaning~Condition, data = part); summary(anMea)
anUnk <- lm(UnknownWords~Condition, data = part); summary(anUnk)
anDif <- lm(SmallDifferences~Condition, data = part); summary(anDif)


## DATA VISUALISION

# Barplot of Condition and TestingMoment (grouped by Condition)
results$Condition <- factor(results$Condition, levels = c("[+O, +NTH]", "[-O, +NTH]", "[-O, -NTH]")) # Change order of TestingMoment
results$TestingMoment <- factor(results$TestingMoment, levels = c("Immediate", "Delayed")) # Change order of TestingMoment
bar_c <- ggplot(results, aes(Condition, Score100, fill = TestingMoment))
bar_c + stat_summary(fun.y = mean, geom = "bar", position = "dodge") + stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position = position_dodge(width=0.90), width = 0.2) + coord_cartesian(ylim = c(0, 40)) + theme(text = element_text(size = 16)) + scale_fill_discrete(name="Testing moment", labels = c("Immediate", "Delayed (15 min.)")) + labs(x = "\nCondition", y = "Learning score (%)\n")

# Barplot of Condition and TestingMoment (grouped by TestingMoment)
bar_tm <- ggplot(results, aes(Condition, Score100, fill = TestingMoment))
bar_tm <- bar_tm + stat_summary(fun.y = mean, geom = "bar", position = "dodge") + stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position = position_dodge(width=0.90), width = 0.2) + coord_cartesian(ylim = c(0, 40)) + theme(text = element_text(size = 20), axis.text.y = element_text(size = 20), axis.text.x = element_text(size = 13), strip.text = element_text(size=20)) + labs(x = "\nCondition", y = "Learning score (%)\n")
bar_tm + facet_wrap(~ TestingMoment)


## MODELS

# Define function to calculate probability from logit
logit2per = function(X){return(exp(X)/(1+exp(X)))}

# Relevelling factors
exp$TestingMoment <- factor(exp$TestingMoment, levels = c("Immediate", "Delayed"))
exp$Condition <- factor(exp$Condition, levels = c("[-O, -NTH]", "[+O, +NTH]", "[-O, +NTH]"))

# 'PhonemesCorrectRelative' is the number of phonemes corrected for word length
# (So that word length does not vary over participants)

# Final model
model <- glmer(cbind(PhonemesCorrectRelative,PhonemesIncorrectRelative) ~ 1 + Condition + TestingMoment + TestingMoment:Condition + KnownBeforePassive + Condition:KnownBeforePassive + (1|Participant) + (1|Word), data = exp, family = 'binomial')
summary(model)

# How did we arrive at the final model?

# Basic model
model0 <- glmer(cbind(PhonemesCorrectRelative,PhonemesIncorrectRelative) ~ 1 + Condition + TestingMoment + TestingMoment:Condition + (1|Participant) + (1|Word), data = exp, family = 'binomial')
summary(model0)

# Explore various models, trying to improve model fit

# Random effect: Slope of Testing moment over Participant
model1 <- glmer(cbind(PhonemesCorrectRelative,PhonemesIncorrectRelative) ~ 1 + Condition + TestingMoment + TestingMoment:Condition + (1+TestingMoment|Participant) + (1|Word), data = exp, family = 'binomial')
summary(model1)
anova(model0, model1)

# Random effect: Slope of Testing moment over Word
model2 <- glmer(cbind(PhonemesCorrectRelative,PhonemesIncorrectRelative) ~ 1 + Condition + TestingMoment + TestingMoment:Condition + (1|Participant) + (1+TestingMoment|Word), data = exp, family = 'binomial')
summary(model2)
anova(model0, model2)

# Fixed effect: Passive knowledge
model3 <- glmer(cbind(PhonemesCorrectRelative,PhonemesIncorrectRelative) ~ 1 + Condition + TestingMoment + TestingMoment:Condition + KnownBeforePassive + (1|Participant) + (1|Word), data = exp, family = 'binomial')
summary(model3)
anova(model0, model3) # Significant, thus better model fit

# Fixed effect: Interaction between Condition and Passive knowledge
model4 <- glmer(cbind(PhonemesCorrectRelative,PhonemesIncorrectRelative) ~ 1 + Condition + TestingMoment + TestingMoment:Condition + KnownBeforePassive + Condition:KnownBeforePassive + (1|Participant) + (1|Word), data = exp, family = 'binomial')
summary(model4)
anova(model3, model4) # Significant, thus better model fit

# Fixed effect: Number of phonemes
model5 <- glmer(cbind(PhonemesCorrectRelative,PhonemesIncorrectRelative) ~ 1 + Condition + TestingMoment + TestingMoment:Condition + KnownBeforePassive + Condition:KnownBeforePassive + Phonemes + (1|Participant) + (1|Word), data = exp, family = 'binomial')
summary(model5)
anova(model4, model5)

# Relevelling the Condition factor
exp$Condition <- factor(exp$Condition, levels = c("[+O, +NTH]", "[-O, +NTH]", "[-O, -NTH]"))


## POST-HOC COMPARISONS

# By Condition and TestingMoment
ls <- lsmeans(model, specs = "Condition", by = "TestingMoment")
summary(ls) # Summary in logit estimates
summary(ls, type = "response") # Estimates are back-transformed from the logit scale
pairs(ls) # Pairwise comparisons

# For different contrasts, try:
#ls <- lsmeans(model, specs = "Condition", by = "KnownBeforePassive")
#ls <- lsmeans(model, specs = "Condition", by = c("TestingMoment","KnownBeforePassive"))

# Get p-values for contrasts, using different corrections for multiple testing
contrast(ls, method = "pairwise", adjust = "none")
contrast(ls, method = "pairwise", adjust = "tukey") # This is identical to 'pairs(ls)'
contrast(ls, method = "pairwise", interaction = TRUE, adjust = "none")
contrast(ls, method = "pairwise", interaction = TRUE, adjust = "tukey")