# Functions used for analysis for Frame Selection Task
# Note that initial experiment refers to 'initial phase' (experiment 1) or 'learning phase' (experiment 2)
# Main experiment refers to the main phase of both experiments
library(dplyr)
library(ggplot2)
library(stringi)
library(reshape2)
library(tidyr)
library(lme4)
library(afex)


catch_trials_threshold <- function(ntrials){
  #' Catch Trials Threshold- Better Then Chance
  #' @param ntrials number of catch trials completed by participant
  #' 
  #' @ return proportion 
  return(1.64*.5/(sqrt(ntrials)) + 0.5)
}

num_catch_trials <- function(choices){
  temp <- choices %>%
    filter(catch_trial == 1) %>%
    group_by(id) %>%
    tally()
  return(median(temp$n))
}

casino_preference_significance <- function(n_trials){
  # Calculate the margin of error for a 95% confidence interval
  error_margin <- 1.96 * (0.5 / sqrt(n_trials))
  # Confidence interval around 0.5
  conf_interval <- c(0.5 - error_margin, 0.5 + error_margin)
  return(conf_interval)
} 


######################### FRAMING EFFECTS #########################

framing_effects <- function(experiment){
  #' Casino Option Preference
  #' 
  #' @description Prints bar graph of proportion of selecting safe and risky option in each frame. 
  #' Prints table of these proportions and returns t-test
  #'
  #' @param experiment df either of initial experiment of main experiment
  #' @param tyoe_of_experiment string of either "Initial" or "Main: used for ggplot
  #' 
  #' @return t-test testing if risky option is statistically greater in loss frame than gain frame
  #' @note Currently commented out plot
  # mytitle = paste("For", type_of_experiement, "Experiment", sep = " ")
  casino_choice <- experiment %>%
    filter(choice != 'no_choice') %>%
    group_by(id, frame_choice) %>%
    summarise('risky' = mean(choice == 'risky'),
              'safe' = mean(choice == 'safe')) %>%
    melt(idvar = c(safe,risky))
  # p <- ggplot(casino_choice, aes(x = variable, y = value))+
  #   geom_bar(stat = "identity", aes(fill = variable))+
  #   labs(x = 'Option', y = 'Proportion Selected', title = mytitle)+
  #   facet_wrap(~frame_choice)
  # print(p)
  risky <- experiment %>%
    filter(choice != 'no_choice') %>%
    group_by(frame_choice, id) %>%
    summarise('risky' = sum(choice == 'risky')/n(),
              'safe' = sum(choice == 'safe')/n()) %>%
    dplyr::select(-safe) %>%
    ungroup(frame_choice, id) %>%
    spread(frame_choice, risky) 
  table_results <- experiment %>%
    filter(choice != 'no_choice') %>%
    group_by(frame_choice) %>%
    summarise('risky' = round(mean(choice == 'risky'),2),
              'safe' = round(mean(choice == 'safe'),2))
  
  t <- t.test(risky$loss_frame, risky$gain_frame, paired = TRUE, alternative = 'greater') 
  print(t)
  print(cohen.d(risky$loss_frame, risky$gain_frame, paired = TRUE))
  return(table_results)
  
}

anova_framing_effect <- function(phase1, phase2){
  initial_phase2 <- phase1 %>%  
    filter(choice != 'no_choice') %>%
    group_by(id, frame_choice) %>%
    summarise('risky' = mean(choice == 'risky'),
              'safe' = mean(choice == 'safe')) %>%
    dplyr::select(-safe) %>%
    mutate(phase = "initial")
  
  fs_phase2 <- phase2 %>%  
    filter(choice != 'no_choice') %>%
    group_by(id, frame_choice) %>%
    summarise('risky' = mean(choice == 'risky'),
              'safe' = mean(choice == 'safe')) %>%
    dplyr::select(-safe) %>%
    mutate(phase = "fs_phase")
  
  combined_phases <- rbind(initial_phase2, fs_phase2) %>% arrange(id) %>% mutate(phase = as.factor(phase), frame_choice = as.factor(frame_choice))
  
  combined_phases_plot <- combined_phases %>% group_by(phase, frame_choice) %>% summarise(r = mean(risky))
  ggplot(combined_phases_plot, aes(x = phase, y = r, fill = frame_choice))+
    geom_bar(stat = "identity", position = "dodge", width = 0.8)
  anova_results <- afex::aov_car(risky ~ frame_choice * phase + Error(id/frame_choice * phase), data=combined_phases, type= 3, include_aov = TRUE)
  summary(anova_results)
}

plot_framing_effects <- function(experiment, myTitle){
  num_participants <- length(unique(experiment$id))
  df_prep <- experiment %>%
    filter(choice != 'no_choice') %>%
    group_by(frame_choice, id) %>%
    summarise('risky' = sum(choice == 'risky')/n()) %>%
    spread(key = frame_choice, value = 'risky') %>%
    mutate('subject_average' = (gain_frame + loss_frame)/2) %>%
    mutate('grand_average' = mean(subject_average)) %>%
    mutate('new_gain_frame' = gain_frame - subject_average + grand_average,
           'new_loss_frame' = loss_frame - subject_average + grand_average) %>%
    ungroup() %>%
    summarise('gain_frame' = mean(gain_frame),
              'loss_frame' = mean(loss_frame),
              'se_gain_frame' = sd(new_gain_frame)/sqrt(num_participants),
              'se_loss_frame' = sd(new_loss_frame)/sqrt(num_participants)) 
  df_choice <- data.frame('frame_choice' = c('Gain frame','Loss frame'), 'risky_choice' = c(df_prep$gain_frame,df_prep$loss_frame), 'se' = c(df_prep$se_gain_frame,df_prep$se_loss_frame))  
  p <- ggplot(df_choice, aes(x = frame_choice, y = risky_choice))+
    geom_bar(stat = "identity", aes(fill = frame_choice), color = "black", width = 0.8)+
    geom_errorbar(aes(ymin = risky_choice - se, ymax = risky_choice + se), width = 0.1)+
    scale_y_continuous(breaks = seq(0, 0.6, by=0.1), limits = c(0, 0.6), expand = c(0,0))+
   # labs(y = "P(Risky choice)", title = myTitle)+
    labs(y = "", title = "")+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position="none", axis.title.x=element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"), axis.text=element_text(size=20), 
          axis.title=element_text(size=20,face="plain"), plot.title = element_text(size = 20, hjust = 0.5))+ 
    scale_fill_manual(values=c('#009E73','firebrick2')) 
  return(p)
}

######################### FRAME PREFERENCE #########################
casino_preference <- function(main_experiment, experiment){
  if(experiment == 2){
    main_experiment <- main_experiment %>%
      filter(casinoLeft_multiplier == casinoRight_multiplier)
  }
  
  casino_preference <- main_experiment %>%
    filter(chose_casino == 1) %>%
    summarise('gain' = sum(frame_choice == 'gain_frame'),
              'gain_selection' = round(sum(frame_choice == 'gain_frame')/n(),2),
              'loss_selection' = round(sum(frame_choice == 'loss_frame')/n(),2),
              'trials' = n(),
              'df' = 0,
              't' = 0,
              'lowerci' = 0,
              'upperci' = 0,
              'pvalue' = 0) 
  
  
  cp <- main_experiment %>%
    filter(chose_casino == 1) %>%
    group_by(id) %>%
    summarise('gain_selection' = mean(frame_choice == 'gain_frame')) %>%
    mutate(condition = ifelse(gain_selection < 0.5, "Loss frame","Gain frame"),
           neutral = 0.5)%>%
    mutate(condition = ifelse(gain_selection == neutral, "No preference", condition))
  
  
  print(paste("Effect Size d:",mean(cp$gain_selection - 0.5)/sd(cp$gain_selection)))
  
  print(cp %>%
          group_by(condition) %>%
          tally())
  
  pp <- ggplot(cp, aes(x = reorder(id, -gain_selection), y = gain_selection))+
    geom_bar(aes(fill = condition), stat = "identity", width = 0.4)+
    geom_hline(yintercept = 0.5, linetype = 2, size = 1)+
    scale_y_continuous(expand = c(0,0))+
    labs(x = 'Participant', y = 'P(Gain frame choice)', fill = "Frame preference")+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), plot.title = element_text(size = 22),
          panel.background = element_blank(), axis.line = element_line(colour = "black"), axis.text=element_text(size=20),
          axis.title=element_text(size=22,face="plain"), axis.text.x = element_blank(), axis.ticks.x = element_blank(),
          legend.title = element_text(size=20), legend.text = element_text(size = 20))+
    scale_fill_manual(values=c('Blue','#FF6600','#958e8e'))
  print(pp)
  
  print(mean(cp$gain_selection-0.5)/sd(cp$gain_selection)) # Calculate effect size
  print(mean(cp$gain_selection-0.5)/sd(cp$gain_selection-0.5))
  pt <- t.test(cp$gain_selection, mu = 0.5, alternative = 'greater')
  casino_preference$t <- pt$statistic
  casino_preference$df <- pt$parameter
  casino_preference$lowerci <- pt$conf.int[1]
  casino_preference$upperci <- pt$conf.int[2]
  casino_preference$pvalue <-pt$p.value
  
  return(pt)  
}

preference_by_trial_number <- function(main_experiment){
  trials_per_block <- max(main_experiment$trial_number) / max(main_experiment$block)
  num_participants <- length(unique(main_experiment$id))
  per_block_df <- main_experiment %>%
    filter(chose_casino == 1) %>%
    mutate(trial_per_block = trial_number %% trials_per_block,
           trial_per_block = ifelse(trial_per_block == 0, trials_per_block, trial_per_block)) %>%
    group_by(id, trial_per_block) %>%
    summarise(prop_gain = mean(frame_choice == 'gain_frame')) %>%
    group_by(trial_per_block) %>%
    summarise(prop_gain_avg = mean(prop_gain),
              se = sd(prop_gain)/sqrt(num_participants))
  trial_plot <- ggplot(per_block_df, aes(x = trial_per_block, y = prop_gain_avg))+
    geom_line(col = "black")+
    geom_ribbon(aes(ymin = prop_gain_avg - se, ymax = prop_gain_avg + se), alpha = 0.2, fill = "black")+
    labs(x = "Trial number", y =  "P(Gain frame choice)")+
    ylim(0.45,0.7)+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"), axis.text=element_text(size=20), plot.title = element_text(size = 22),
          axis.title=element_text(size=22,face="plain"))
  return(print(trial_plot))
}

heatmap_casino_preferences <- function(experiment){
  test <- experiment %>%
    filter(casinoLeft_frame != casinoRight_frame) %>%
    group_by(id_pair) %>%
    filter(chose_casino == 1) %>% 
    summarise(proportion = mean(frame_choice == 'gain_frame')) %>%
    separate(col = id_pair, into = c('gain_frame', 'loss_frame'), sep = 'L') %>%
    drop_na() %>%
    filter_all(any_vars(grepl('G',.))) %>%
    mutate(loss_frame = paste("L",loss_frame))
  
  
  p <- ggplot(test, aes(x = as.factor(gain_frame), y =as.factor(loss_frame), fill = proportion))+
    geom_tile(color = "black", size = 0.2)+
    geom_text(aes(label = round(proportion,2)), size = 7)+
    scale_fill_gradient2(low="red", mid="white", high="blue", 
                         midpoint=0.5)+
    labs(x = "Gain frame value", y = "Loss frame value")+
    theme(legend.position="none",axis.ticks = element_blank(), plot.title = element_text(size = 22),
          panel.background = element_blank(), axis.line = element_blank(), axis.text=element_text(size=20),
          axis.title=element_text(size=22,face="plain"))+
    scale_x_discrete(labels = c("Low", "Medium", "High"))+
    scale_y_discrete(labels = c("Low", "Medium", "High"))
  
  return(p)
}

plot_indifference_point <- function(experiment){
  ee <- experiment %>%
    mutate(casinoGain_multiplier = ifelse(casinoLeft_frame == "gain_frame", casinoLeft_multiplier, casinoRight_multiplier)) %>%
    mutate(casinoLoss_multiplier = ifelse(casinoLeft_frame == "loss_frame", casinoLeft_multiplier, casinoRight_multiplier)) %>%
    mutate(ratio = log(casinoGain_multiplier/casinoLoss_multiplier))
  
  points <- ee %>%
    dplyr::select(ratio, id, gain_frame) %>%
    group_by(id, ratio) %>%
    summarise(prop = mean(gain_frame)) %>%
    group_by(ratio) %>%
    summarise(proportion = mean(prop))
  
  mixed_model_data <- ee %>%
    dplyr::select(id, ratio, gain_frame)
  mm <- glmer(gain_frame ~ ratio + (1|id), data = mixed_model_data, family = "binomial")
  print(confint(mm))

  jvalues <- with(ee, seq(from = -0.5, to = .5, length.out = 1000))
  
  # calculate predicted probabilities and store in a list
  mm_summary <- summary(mm)
  log_indifference <- -mm_summary$coefficients[1,1]/mm_summary$coefficients[2,1]
  
  print(summary(mm_summary))
  
  mm_coefficients <- c(mm_summary$coefficients[1,1],mm_summary$coefficients[2,1])
  mm_coefficients[1]
  
  logit_function_y <- function(x, beta0, beta1){
    return(1/(1+exp(-(beta0 + beta1*x))))
  }
  
  cc <- confint(mm)
  lower_beta0 <- cc[2,1]
  lower_beta1 <- cc[3,1]
  upper_beta0 <- cc[2,2]
  upper_beta1 <- cc[3,2]
  
  
  y <- logit_function_y(jvalues, mm_summary$coefficients[1,1], mm_summary$coefficients[2,1])
  
  lowery <- logit_function_y(jvalues, lower_beta0, lower_beta1)
  uppery <- logit_function_y(jvalues, upper_beta0, upper_beta1)
  plotdat <- data.frame(Ratio = jvalues, PredictedProbability = y, lower = lowery, upper = uppery)
  
  ci_indifference <- plotdat %>%
    filter(lower <= 0.5 & upper >= 0.5) %>%
    summarise(minRatio = min(Ratio), maxRatio = max(Ratio))
  
  
  p <- ggplot(data = plotdat, aes(x = Ratio, y = PredictedProbability)) + geom_line(col = "black") +
    ylim(c(0, 1))+
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2)+
    labs(x = "", y = "")+
    geom_segment(aes(x=ci_indifference$minRatio,xend=ci_indifference$maxRatio,y=0.5,yend=0.5), color = "red", width = 0.5)+
    #   geom_errorbarh(aes(xmin = ci_indifference$minRatio, xmax = ci_indifference$maxRatio), y = 0.5, height = 0.01, position = "identity")+
    # geom_segment(aes(ystart = 0.5, yend = 0.5, xstart = ci_indifference$minRatio, xend = ci_indifference$maxRatio), colour = "red")+
    #  geom_point(data = points, aes(x = ratio, y = proportion), col = "blue")+
    geom_point(aes(x = log_indifference), y = 0.5, col = "red", size = 2, shape = 21, fill = "white")+
    geom_vline(xintercept=0,linetype=2)+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"), axis.text=element_text(size=20),
          axis.title=element_text(size=20,face="plain"), plot.title = element_text(size = 20))
  
  
  # Ad hoc way, especially calculating the lower and upper bounds
  print(paste("Indifference Point: ", exp((log_indifference))))
  print(paste("Lower Bound:", exp(ci_indifference$minRatio)))
  print(paste("Upper Bound:", exp(ci_indifference$maxRatio)))
  return(p)
}

# This function is only needed for Experiment 2 since there is only a learning phase in this experiment. 
run_mixed_effects_model_by_participants_learn <- function(fs_phase, plc){
  prep_data <- fs_phase %>%
    mutate(casinoGain_multiplier = ifelse(casinoLeft_frame == "gain_frame", casinoLeft_multiplier, casinoRight_multiplier),
           casinoLoss_multiplier = ifelse(casinoLeft_frame == "loss_frame", casinoLeft_multiplier, casinoRight_multiplier),
           ratio = log(casinoGain_multiplier/casinoLoss_multiplier)) 
  
  by_group <- prep_data %>%
    rowwise() %>%
    mutate(learned_casino = ifelse(id %in% plc$id, 1, -1)) %>%
    ungroup() %>%
    dplyr::select(ratio, learned_casino, id, gain_frame)
  
  mm <- glmer(gain_frame ~ ratio*learned_casino + (1|id), data = by_group, family = "binomial")
  
  
  by_accuracy <- fs_phase %>%
    filter(casinoLeft_frame == casinoRight_frame) %>%
    group_by(id) %>%
    mutate(correct_selection = ifelse((casinoLeft_multiplier > casinoRight_multiplier & casino_choice == casinoLeft_name) | (casinoRight_multiplier > casinoLeft_multiplier & casino_choice == casinoRight_name),1,0 )) %>%
    dplyr::select(id, casinoLeft_name, casino_choice, casinoLeft_multiplier, casinoRight_multiplier, correct_selection) %>%
    summarise(percent_correct = mean(correct_selection), trials = n()) %>%
    select(id, percent_correct) %>% 
    ungroup() %>%
    mutate(average_percent_correct = mean(percent_correct),
           percent_correct = percent_correct - average_percent_correct) %>%
    select(id, percent_correct) %>%
    right_join(prep_data, by = "id")
  mm2 <- glmer(gain_frame ~ ratio*percent_correct + (1|id), data = by_accuracy, family = "binomial")
  
  print("Based on percent correct")
  print(confint(mm2))
  print(summary(mm2))
  
  
  print("Based on which condition classified")
  print(confint(mm))
  return(summary(mm))
}

######################### RISK-FRAME PREFERENCE RELATIONSHIP #########################

plot_risk_frame <- function(initial, main){
  w1 <- initial %>%
    filter(reaction_time_choice != -1) %>%
    group_by(id, gain_frame) %>%
    summarise(p_risky = mean(choice == "risky"),
              choice_rt = mean(reaction_time_choice)) %>%
    mutate(gain_frame2 = ifelse(gain_frame == 1, 1, -1)) 
  
  ww <- main %>%
    filter(catch_trial == 0 & reaction_time_casino != -1) %>%
    group_by(id) %>%
    summarise(p_gain = mean(gain_frame)) %>%
    ungroup() %>%
    mutate(p_gain_avg = mean(p_gain)) %>%
    group_by(id) %>%
    mutate(p_gain_center = p_gain - p_gain_avg)
  
  rt_fs <- left_join(w1, ww, by = "id") 
  
  p <- ggplot(rt_fs, aes(x = p_gain, y = p_risky, fill = as.factor(gain_frame), color = as.factor(gain_frame)))+
    geom_smooth(method = 'lm', se = TRUE)+
    scale_color_manual(values=c('#009E73','firebrick2'))+
    scale_fill_manual(values=c('firebrick2','#009E73'))+
    scale_color_manual(values=c('firebrick2','#009E73'), labels=c('Loss frame','Gain frame'))+
    labs(x = "P(Gain frame choice)", y = "P(Risky choice)", color = NULL)+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"), axis.text=element_text(size=20),
          axis.title=element_text(size=20,face="plain"), legend.text = element_text(size = 16))+
    guides(fill = "none")
  print(p)
  print(confint(lmer(p_risky ~  gain_frame2 * p_gain_center + (1|id), data = rt_fs)))
  return(summary(lmer(p_risky ~  gain_frame2 * p_gain_center + (1|id), data = rt_fs)))
}

risk_difference <- function(initial, main){
  # Calculate risky choice for initial and main phase
  w1 <- initial %>%
    filter(reaction_time_choice != -1) %>%
    group_by(id, gain_frame) %>%
    summarise(p_risky_initial = mean(choice == "risky"))
  
  w2 <- main %>%
    filter(catch_trial == 0 & reaction_time_casino != -1 & reaction_time_choice != -1) %>%
    group_by(id, gain_frame) %>%
    summarise(p_risky_main = mean(choice == "risky"))
  
  w12 <- left_join(w1, w2, by = c("id", "gain_frame")) %>% mutate(risky_main_minus_initial = p_risky_main - p_risky_initial)
  
  ww <- main %>%
    filter(catch_trial == 0 & reaction_time_casino != -1) %>%
    group_by(id) %>%
    summarise(p_gain = mean(gain_frame)) %>%
    ungroup() %>%
    mutate(p_gain_avg = mean(p_gain)) %>%
    group_by(id) %>%
    mutate(p_gain_center = p_gain - p_gain_avg) %>%
    select(-c(p_gain, p_gain_avg)) %>%
    rename(p_gain = p_gain_center)
  
  a1 <- initial %>%
    filter(reaction_time_choice != -1) %>%
    group_by(id) %>%
    summarise(p_risky_initial = mean(choice == "risky"))
  
  a2 <- main %>%
    filter(catch_trial == 0 & reaction_time_casino != -1 & reaction_time_choice != -1) %>%
    group_by(id) %>%
    summarise(p_risky_main = mean(choice == "risky"))
  
  w12 <- left_join(w1, w2, by = c("id", "gain_frame")) %>% mutate(risky_main_minus_initial = p_risky_main - p_risky_initial)
  a12 <- left_join(a1, a2, by = "id") %>% mutate(risky_main_minus_initial = p_risky_main - p_risky_initial)
  
  rt_fs <- left_join(w12, ww, by = "id") %>% mutate(gain_frame2 = ifelse(gain_frame == 1, 1, -1))
  rt_fs_a <- left_join(a12, ww, by = "id") 
  model <- lm(risky_main_minus_initial ~ p_gain, data = rt_fs_a)
  print(confint(model))
  # summary(lmer(risky_main_minus_initial ~  gain_frame2 * p_gain + (1|id), data = rt_fs))
  return(summary(model))
}


######################### Pre-registration #########################

risk_preference <- function(initial_experiment, main_experiment){
  c <- initial_experiment %>%
    filter(choice != 'no_choice') %>%
    group_by(id) %>%
    summarise('safe_initial' = sum(choice == 'safe')/n(),
              'risky_initial' = sum(choice == 'risky')/n()) %>%
    mutate('prefered_safe_initial'= safe_initial - risky_initial)
  cc <- main_experiment %>%
    filter(response_casino != 'no_choice') %>%
    group_by(id) %>%
    summarise('gain_selection' = sum(frame_choice == 'gain_frame')/n(),
              'loss_selection' = sum(frame_choice == 'loss_frame')/n(),
              'safe_main' = sum(choice == 'safe')/n(),
              'risky_main' = sum(choice == 'risky')/n()) %>%
    mutate('prefered_safe_main'= safe_main - risky_main,
           'preferred_gain' = gain_selection - loss_selection)
  casino_safe_agreement <- full_join(c, cc, by = "id") 
  
  return(cor.test(casino_safe_agreement$loss_selection, casino_safe_agreement$risky_initial, method = c("pearson")))
}

risk_propensity_grips <- function(grips, main_experiment, percentile_threshold){  
  r <- grips %>%
    select(-c(catch_trial)) %>%
    melt(id = "id") %>%
    group_by(id) %>%
    summarise(risk_score = sum(value)) %>%
    mutate(pc = quantile(risk_score, percentile_threshold)) 
  
  rr <- main_experiment %>%
    filter(response_casino != 'no_choice') %>%
    group_by(id) %>%
    summarise('gain_selection' = sum(frame_choice == 'gain_frame')/n(),
              'loss_selection' = sum(frame_choice == 'loss_frame')/n())
  risk_score_df <- full_join(r, rr, by = "id") %>%
    filter(risk_score < pc)
  return(cor.test(risk_score_df$loss_selection, risk_score_df$risk_score, method = c("pearson")))
}  

######################### Other Strategies #########################
run_other_strategies <- function(experiment){ # Note can only run for Experiment 1
  test <- experiment %>% 
    filter(chose_casino == 1) %>%
    select(id, block, trial_number, p, lumpSum, chose_casino, casino_choice, frame_choice) %>%
    mutate(ev = lumpSum * p) %>%
    group_by(id) %>%
    mutate(avg_ev = mean(ev)) %>%
    group_by(id, block) %>%
    mutate(choice_before = lag(casino_choice),
           high_p = ifelse(lag(p) > 0.5, 1, -1),
           higher_ev = ifelse(lag(ev) > avg_ev, 1, -1),
           chose_same_casino = ifelse(choice_before == casino_choice, 1, 0)) %>%
    filter(choice_before != "NA")
  
  
  high_p <- test %>%
    group_by(id, high_p) %>%
    summarise(prop_same = mean(chose_same_casino)) %>%
    mutate(p_values = ifelse(high_p == 1, "above", "below")) %>%
    select(-high_p) %>%
    pivot_wider(names_from = p_values, values_from = prop_same)
  
  print("Strategy: Selected same casino when risky probability was high (> 0.5):")
  print(t.test(high_p$below, high_p$above, paired = TRUE))
  print(cohen.d(high_p$below, high_p$above, paired = TRUE))
  
  high_ev <- test %>%
    group_by(id, higher_ev) %>%
    summarise(prop_same = mean(chose_same_casino)) %>%
    mutate(ev_s = ifelse(higher_ev == 1, "above", "below")) %>%
    select(-higher_ev) %>%
    pivot_wider(names_from = ev_s, values_from = prop_same)
  
  print("Strategy: Selected same casino when received higher than average reward:")
  print(t.test(high_ev$below, high_ev$above, paired = TRUE))
  print(cohen.d(high_ev$below, high_ev$above, paired = TRUE))
}


######################### CLEANING FUNCTIONS TO PREP DATA SET #########################

########## FOR EXPERIMENT 1 ONLY (SINCE INCLUDES GRIPS)

find_bad_participants_experiment1 <- function(subInfo, initial_phase, main_phase, grips, catch_trials){
  #' "Bad" Participants
  #' 
  #' @description Returns list of participants who failed any attention check described in preregistration 
  #' @note used exclusively for experiment 1
  #' @param subInfo dataframe
  #' @param choices dataframe
  #' @param grips dataframe
  #' 
  #' @return vector of "bad" participants' id
  # Is colorblind
  colorblind <- subInfo %>%
    mutate(colorblindness = toupper(colorblindness)) %>%
    filter(!grepl("NO", colorblindness)) %>%
    dplyr::select(id)
  
  # Failed catch trials
  threshold <- catch_trials_threshold(catch_trials)
  failed_catch_trials <- initial_phase %>%
    filter(catch_trial == 1 & practice_trial == 0) %>%
    dplyr::select(id, p, response_choice) %>%
    group_by(id) %>%
    mutate(correct = ifelse((p == 0.05 & response_choice == 'f') | (p == 0.95 & response_choice == 'j'),1,0)) %>%
    summarise(proportion = sum(correct)/n()) %>%
    filter(proportion < threshold) %>%
    dplyr::select(id)
  
  # Failed to explore
  failed_to_explore <- main_phase %>%
    filter(block == 1 & chose_casino == 1) %>%
    group_by(id) %>%
    mutate(select_gain = ifelse(frame_choice == 'gain_frame',1,0)) %>%
    summarise(proportion = sum(select_gain)/n()) %>%
    filter(proportion == 1 | proportion == 0) %>%
    dplyr::select(id)
  
  # Pressed one key
  initial_keys <- initial_phase %>%
    filter(practice_trial == 0) %>%
    group_by(id) %>%
    mutate(f_key = ifelse(response_choice == 'f',1,0)) %>%
    summarise(proportion = sum(f_key)/n()) %>%
    filter(proportion == 1 | proportion == 0) %>%
    dplyr::select(id)
  
  main_keys <- main_phase %>%
    filter(practice_trial == 0 & chose_casino == 1) %>%
    group_by(id) %>%
    mutate(f_key = ifelse(response_choice == 'f' & response_casino == 'f',1,0),
           j_key = ifelse(response_choice == 'j' & response_casino == 'j',1,0)) %>%
    summarise(prop_f = sum(f_key)/n(),
              prop_j = sum(j_key)/n()) %>%
    filter(prop_f == 1 | prop_j == 1) %>%
    dplyr::select(id)
  
  if(length(main_keys$id) > 0 & length(initial_keys) == 0) {
    failed_to_press_keys <- inner_join(main_keys, inner_keys, by = "id")
  }else if(length(main_keys$id) > 0 & length(initial_keys) == 0){
    failed_to_press_keys <- main_keys
  } else{
    failed_to_press_keys <- initial_keys
  }
  
  # failed grips catch trial
  failed_grips_check <- grips %>%
    filter(catch_trial != 3) %>%
    dplyr::select(id)
  
  # Incomplete 
  temp <- initial_phase %>%
    filter(!(id %in% subInfo$id)) %>%
    dplyr::select(id) %>%
    unique()
  temp2 <- grips %>%
    filter(!(id %in% subInfo$id)) %>%
    dplyr::select(id) %>%
    unique()
  print(temp2)
  incomplete <- full_join(temp, temp2, by = "id")
  
  
  bp_table <- data.frame(reason = c("colorblind", "failed to explore in first block", "only selected f or j key throughout task", "underpreformed on catch trials", "failed attention check on Grips", "Did not finish experiment"),
                         participants = c(length(colorblind$id), length(failed_to_explore$id), length(failed_to_press_keys$id), length(failed_catch_trials$id), length(failed_grips_check$id), length(incomplete$id)))
  #print(create_table(bp_table, "Reasons why Participants were excluded"))
  print(bp_table)
  bad_participants <- unique(c(colorblind$id, failed_catch_trials$id, failed_to_explore$id, failed_to_press_keys$id, failed_grips_check$id, incomplete$id))
  return(bad_participants)
}


########## FOR EXPERIMENT 2 ONLY

find_bad_participants_exp2 <- function(subInfo, initial_phase, main_phase, catch_trials){
  #' "Bad" Participants
  #' 
  #' @description Returns list of participants who failed any attention check described in preregistration 
  #'
  #' @param subInfo dataframe
  #' @param choices dataframe
  #' @param grips dataframe
  #' 
  #' @return vector of "bad" participants' id
  
  # Failed catch trials
  threshold <- catch_trials_threshold(catch_trials)

  failed_catch_trials <- initial_phase %>%
    filter(catch_trial == 1 & practice_trial == 0) %>%
    dplyr::select(id, p, response_choice) %>%
    group_by(id) %>%
    mutate(correct = ifelse((p == 0.05 & response_choice == 'f') | (p == 0.95 & response_choice == 'j'),1,0)) %>%
    summarise(proportion = mean(correct)) %>%
    filter(proportion < threshold) %>%
    dplyr::select(id)
  
  # Failed to explore
  failed_to_explore <- main_phase %>%
    filter(block == 1 & chose_casino == 1) %>%
    group_by(id) %>%
    mutate(select_gain = ifelse(frame_choice == 'gain_frame',1,0)) %>%
    summarise(proportion = mean(select_gain)) %>%
    filter(proportion == 1 | proportion == 0) %>%
    dplyr::select(id)
  
  # Pressed one key
  initial_keys <- initial_phase %>%
    filter(practice_trial == 0) %>%
    group_by(id) %>%
    mutate(f_key = ifelse(response_choice == 'f',1,0)) %>%
    summarise(proportion = sum(f_key)/n()) %>%
    filter(proportion == 1 | proportion == 0) %>%
    dplyr::select(id)
  
  main_keys <- main_phase %>%
    filter(practice_trial == 0 & chose_casino == 1) %>%
    group_by(id) %>%
    mutate(f_key = ifelse(response_choice == 'f' & response_casino == 'f',1,0),
           j_key = ifelse(response_choice == 'j' & response_casino == 'j',1,0)) %>%
    summarise(prop_f = sum(f_key)/n(),
              prop_j = sum(j_key)/n()) %>%
    filter(prop_f == 1 | prop_j == 1) %>%
    dplyr::select(id)
  
  if(length(main_keys$id) > 0 & length(initial_keys$id) > 0) {
    failed_to_press_keys <- inner_join(main_keys, initial_keys, by = "id")
  }else if(length(main_keys$id) > 0 & length(initial_keys$id) == 0){
    failed_to_press_keys <- main_keys
  } else{
    failed_to_press_keys <- initial_keys
  }
  
  # Incomplete 
  incomplete <- main_phase %>%
    filter(!(id %in% subInfo$id)) %>%
    dplyr::select(id) %>%
    unique()
  trials_per_participant <- main_phase %>%
    group_by(id) %>%
    summarise(num_trials = n()) %>%
    filter(num_trials > 300) %>%
    dplyr::select(id)
  
  bp_table <- data.frame(reason = c("failed to explore in first block", "only selected f or j key throughout task", "underpreformed on catch trials", "Did not finish experiment"),
                         participants = c(length(failed_to_explore$id), length(failed_to_press_keys$id), length(failed_catch_trials$id), length(incomplete$id)))
  #print(create_table(bp_table, "Reasons why Participants were excluded"))
  print(bp_table)
  bad_participants <- unique(c(failed_catch_trials$id, failed_to_explore$id, failed_to_press_keys$id, incomplete$id))
  return(bad_participants)
}

participants_learned_casinos <- function(main_experiment){
  result_df <- main_experiment %>%
    filter(casinoLeft_frame == casinoRight_frame) %>%
    group_by(id) %>%
    mutate(correct_selection = ifelse((casinoLeft_multiplier > casinoRight_multiplier & casino_choice == casinoLeft_name) | (casinoRight_multiplier > casinoLeft_multiplier & casino_choice == casinoRight_name),1,0 )) %>%
    dplyr::select(id, casinoLeft_name, casino_choice, casinoLeft_multiplier, casinoRight_multiplier, correct_selection) %>%
    summarise(percent_correct = mean(correct_selection), trials = n()) %>%
    mutate(threshold = catch_trials_threshold(trials)) %>%
    filter(percent_correct > 0.6)
  return(result_df)
}
