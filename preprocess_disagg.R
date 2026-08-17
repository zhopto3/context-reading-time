library(tidyverse)
path <- "./"
##############################
### LOAD L1 MECO V1.2 DATA ###
##############################
load(paste0(path,"joint_data_trimmed.rda"))
#############################
### FIX L1 MECO V1.2 DATA ###
#############################
non_unique_ids <- joint.data %>% 
  group_by(lang, trialid, ianum) %>%
  summarise(unique_words = length(unique(ia))) %>%
  filter(unique_words > 1)

affected_subjs <- subset(joint.data, lang == "en" & trialid == 3 & ianum == 149 & ia == "performance-")$subid
joint.data <- joint.data %>% 
  mutate(ianum = ifelse(lang == "en" & subid %in% affected_subjs & trialid == 3 & ianum >= 150, 
                        ianum + 1, ianum)) %>%
  filter(!(trialid == 3 & ianum == 149))

joint.data <- joint.data %>%
  mutate(trialid = ifelse(subid == "ru_8" & trialid >= 4, trialid + 1, trialid))

joint.data <- joint.data %>%
  mutate(trialid = ifelse(subid == "ee_22" & trialid >= 1, trialid + 1, trialid),
         trialid = ifelse(subid == "ee_09" & trialid >= 4, trialid +1, trialid))

non_unique_ids <- joint.data %>% 
  group_by(lang, trialid, ianum) %>%
  summarise(unique_words = length(unique(ia)), nSubj = n()) %>%
  filter(unique_words > 1)
#########################
### TRANSFORM COLUMNS ###
#########################  
rt_data <- joint.data
rt_data <- rt_data %>%
  mutate(dur = as.double(dur)) %>%
  rename(total_rt = dur) %>%
  
  mutate(firstrun.dur = as.double(firstrun.dur)) %>%
  rename(gaze_rt = firstrun.dur) %>%
  
  mutate(firstfix.dur = as.double(firstfix.dur)) %>%
  rename(firstfix_rt = firstfix.dur) %>%
  
  # Added subid to group_by to keep participants disaggregated
  group_by(subid, lang, trialid, ianum, ia) %>%
  summarise(sentnum = first(sentnum),
            total_rt = first(total_rt),
            gaze_rt = first(gaze_rt),
            firstfix_rt = first(firstfix_rt)) %>%
  ungroup()

rt_data <- rt_data %>%
  filter(!lang %in% c("ee", "no"))

########################################################################
### LOAD IN WORD FREQUENCY AND MULTILINGUAL GPT LONG CONTEXT RESULTS ###
########################################################################
# LONG CONTEXT = full window size of 512 previous characters
# Word frequency results are from Python library wordfreq

do_lags <- function(df) {
  result <- df %>%
    arrange(trialid, ianum) %>%
    group_by(trialid) %>%
    mutate(
      prev_surp = lag(surp),
      prev2_surp = lag(prev_surp),
      
      prev_freq = lag(freq),
      prev2_freq = lag(prev_freq),
      
      prev_len = lag(len),
      prev2_len = lag(prev_len),
      
      prev_ent = lag(ent),
      prev2_ent = lag(prev_ent)
    ) %>%
    ungroup()
}

langs <- c("du", "en", "fi", "ge", "gr", "he", "it", "sp", "ko", "tr", "ru")
mgpt_lc_df <- data.frame()
# MGPT LONG CONTEXT DATA
for (lang in langs) {
  mgpt_lc_df_i <- read.csv(paste0(path, "mgpt_lc/", lang, "_preds.csv"), header = T, sep = "\t") %>%
    rename(model_ia = ia) %>%
    mutate(ianum = ianum + 1) %>%
    dplyr::select(-X) %>%
    mutate(model = "mgpt_lc",
           lang = lang) %>%
    mutate(len = str_length(model_ia)) %>%
    do_lags(.)
  
  mgpt_lc_df <- rbind(mgpt_lc_df, mgpt_lc_df_i)
}

rt_data <-  rt_data %>%
  merge(mgpt_lc_df, by=c("lang", "trialid", "ianum")) %>%
  mutate(mismatch = model_ia != ia)

print(paste0(lang, " / MGPT LC: Filtered a total of ", sum(rt_data$mismatch), "rows, or ", sum(rt_data$mismatch)/nrow(rt_data), " of the data."))

##########################  
### SEPARATE LANGUAGES ###
##########################

for (l in langs) {
  langi <- subset(rt_data, lang == l)
  langi <- langi[order(langi$trialid, langi$ianum), ]
  langi$mismatch <- NULL
  write.csv(langi, paste0(path, "merged_data_no_zero_disagg/", l, ".csv"), quote = T, row.names = F)
}
