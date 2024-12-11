# Results for Experiment 1 and Experiment 2
# Code used for 'Choosing the right frame: How context preferences influence subsequent choices'
setwd(".../OSF")
source("./Analysis/helperFunctions.R")
library(dplyr)
library(effsize)
library(ggplot2)

# Download datasets 
# Exp 1
# .../OSF/
fs_phase_exp1 <- read.csv("./Data/Experiment1/experiment1_FrameSelectionPhaseChoices.csv") 
initial_phase_exp1 <- read.csv("./Data/Experiment1/experiment1_InitialPhaseChoices.csv") 
subInfo_exp1 <- read.csv("./Data/Experiment1/experiment1_subInfo.csv")
grips_exp1 <- read.csv("./Data/Experiment1/experiment1_grips.csv") %>% select(-X) 

# Exp 2
fs_phase_exp2 <- read.csv("./Data/Experiment2/experiment2_FrameSelectionPhaseChoices.csv") 
learning_phase_exp2 <- read.csv("./Data/Experiment2/experiment2_LearningPhaseChoices.csv") 
subInfo_exp2 <- read.csv("./Data/Experiment2/experiment2_subInfo.csv") 

# Code for Experiment 1
bad_participants <- find_bad_participants_experiment1(subInfo_exp1, initial_phase_exp1, fs_phase_exp1, grips_exp1, 12)

# Clean datasets 
initial_phase_exp1_cleaned <- initial_phase_exp1 %>%
  filter(!(id %in% bad_participants) & catch_trial == 0 & practice_trial == 0) %>%
  mutate(gain_frame = ifelse(frame_choice == 'gain_frame',1,0))

fs_phase_exp1_cleaned <- fs_phase_exp1 %>%
  filter( !(id %in% bad_participants) & catch_trial == 0 & practice_trial == 0) %>%
  mutate(gain_frame = ifelse(frame_choice == 'gain_frame',1,0))

# Framing effects
framing_effects(initial_phase_exp1_cleaned)
plot_framing_effects(initial_phase_exp1_cleaned, "Initial Phase")
framing_effects(fs_phase_exp1_cleaned)
plot_framing_effects(fs_phase_exp1_cleaned, "Frame Selection Phase")
anova_framing_effect(initial_phase_exp1_cleaned, fs_phase_exp1_cleaned)

# Casino Preference
casino_preference(fs_phase_exp1_cleaned,1)
preference_by_trial_number(fs_phase_exp1_cleaned)

# Risk preference vs gain frame preference relationship
plot_risk_frame(initial_phase_exp1_cleaned, fs_phase_exp1_cleaned)
risk_difference(initial_phase_exp1_cleaned, fs_phase_exp1_cleaned)

# From pre-registration
risk_preference(initial_phase_exp1_cleaned, fs_phase_exp1_cleaned)
grips_exp1_cleaned <- grips_exp1 %>% filter(!(id %in% bad_participants))
risk_propensity_grips(grips_exp1_cleaned, fs_phase_exp1_cleaned, 0.95)

# Other strategies besides frame preference
run_other_strategies(fs_phase_exp1_cleaned)

# Code for Experiment 2

# Clean Code
bad_participants2 <- find_bad_participants_exp2(subInfo_exp2, learning_phase_exp2, fs_phase_exp2, num_catch_trials(learning_phase_exp2))

learning_phase_exp2_cleaned <- learning_phase_exp2 %>% 
  filter(!(id %in% bad_participants2) & catch_trial == 0 & practice_trial == 0) %>%
  mutate(gain_frame = ifelse(frame_choice == 'gain_frame',1,0))
fs_phase_exp2_cleaned <- fs_phase_exp2 %>% 
  filter(!(id %in% bad_participants2) & catch_trial == 0 & practice_trial == 0) %>%
  mutate(gain_frame = ifelse(frame_choice == 'gain_frame',1,0))

# Framing Effects
framing_effects(learning_phase_exp2_cleaned)
plot_framing_effects(learning_phase_exp2_cleaned, "Learning Phase")
framing_effects(fs_phase_exp2_cleaned)
plot_framing_effects(fs_phase_exp2_cleaned, "Frame Selection Phase")
anova_framing_effect(learning_phase_exp2_cleaned, fs_phase_exp2_cleaned)

# Casino Preference
casino_preference(fs_phase_exp2_cleaned,2) 
heatmap_casino_preferences(fs_phase_exp2_cleaned)
plot_indifference_point(fs_phase_exp2_cleaned)

# Risk preferences
same_fs_exp2_cleaned <- fs_phase_exp2_cleaned %>%
  filter(casinoLeft_multiplier == casinoRight_multiplier)
plot_risk_frame(learning_phase_exp2_cleaned, same_fs_exp2_cleaned)
risk_difference(learning_phase_exp2_cleaned, same_fs_exp2_cleaned)

# Comparison to participants who learned casinos
plc <- participants_learned_casinos(fs_phase_exp2_cleaned)
run_mixed_effects_model_by_participants_learn(fs_phase_exp2_cleaned, plc) 
# Note for code above: Output 1 is for treating percent correct as quantitative variable and output 2 is comparing groups.

# Value sensitive group
fs_phase_yes <- fs_phase_exp2_cleaned %>% filter(id %in% plc$id)
casino_preference(fs_phase_yes,2) 
plot_indifference_point(fs_phase_yes)
# Value insensitive group
fs_phase_no <- fs_phase_exp2_cleaned %>% filter(!(id %in% plc$id))
casino_preference(fs_phase_no,2) 
plot_indifference_point(fs_phase_no)
