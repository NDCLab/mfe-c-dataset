# This script will load, and organize the Psychopy data for the mfe_c_object.
# For each participant, a single, new, organized csv file that has all the necessary information will be generated.
# Author: Kianoosh Hosseini at NDCLab @FIU (https://Kianoosh.info; https://NDClab.com)
# Last Update: 2025-10-16 (YYYY-MM-DD)

library(tidyverse)
library(dplyr)
library(stringr)



#Working directory should be the Psychopy experiment directory.
proje_wd <- "/Github_Repos/mfe-c-dataset"
setwd(proje_wd)

# Defining the input and output folders.
input_path <- paste(proje_wd, "sourcedata", "checked", "psychopy", "mfe-c-object", sep ="/", collapse = NULL) # input data directory
output_path <- paste(proje_wd, "derivatives", "psychopy", "csv_output", "mfe-c-object", sep ="/", collapse = NULL) # Directory that each new csv file will be stored
flanker_csv_fileName <- "_mfe_c_object_flankerDat_v1.csv" # each output csv file will have this on its filename
surprise_csv_fileName <- "_mfe_c_object_surpriseDat_v1.csv" # each output csv file will have this on its filename


## creating a list of all data csv files in the input folder.
datafiles_list <- c() # an empty list that will be filled in the next "for" loop!
csvSelect <- list.files(input_path, pattern = ".csv") # listing only csv files
for (i in 1:length(csvSelect)){
  temp_for_file <- ifelse (str_detect(csvSelect[i], "mfe_c_object", negate = FALSE), 1, 0)
  if (temp_for_file == 1){
    temp_list <- csvSelect[i]
    datafiles_list <- c(datafiles_list, temp_list)
  }
}



# will loop over all participant datafiles.
for(subject in 1:length(datafiles_list)){
  # creating an empty data frame that will store all the information drawn from the flanker task of a participant.
  # This data frame will be saved as a csv file for this participant.
  flanker_df <- setNames(data.frame(matrix(ncol = 22, nrow = 0)), c("participant_id", "current_trial_object", "pre_trial_object", "post_trial_object",
                                                                    "current_trial_accuracy", "pre_trial_accuracy", "post_trial_accuracy",
                                                                    "current_trial_congruency", "pre_trial_congruency", "post_trial_congruency",
                                                                    "current_trial_rt", "pre_trial_rt", "post_trial_rt",
                                                                    "current_trial_responded", "pre_trial_responded", "post_trial_responded",
                                                                    "current_trial_legitResponse", "pre_trial_legitResponse", "post_trial_legitResponse",
                                                                    "current_trial_resp_nums", "pre_trial_resp_nums", "post_trial_resp_nums"))
  # creating an empty data frame that will store all the information drawn from the surprise task of a participant.
  # This data frame will be saved as a csv file for this participant.
  surprise_df <- setNames(data.frame(matrix(ncol = 5, nrow = 0)), c("participant_id", "object", "surpAcc", "memory_surp_responses", "memory_surp_rt"))

  #for this participant, find the csv file
  psychopy_file <- paste(input_path,datafiles_list[subject], sep = "/", collapse = NULL)

  #read in the data for this participant, establish id, and remove extraneous variables
  psychopyDat <- read.csv(file = psychopy_file, stringsAsFactors = FALSE, na.strings=c("", "NA"))
  participant_id <- psychopyDat$id[1]

  psychopyDatTrim <- psychopyDat[c("id",
                                   "old_object_in_surp", # the old objects displayed in the surp task
                                   "which_side_old_object_displayed",
                                   "surprise_key_resp.keys", # The answers in the surprise task
                                   "surprise_key_resp.rt", # The RTs of the answers in the surprise task
                                   "congruent", # whether the flanker trial is congruent
                                   "stimNum",
                                   "accuracy", # whether the flanker trial response is correct
                                   "task1_stim_keyResp.keys", # the flanker trial responses
                                   "errorNum_text_box.text", # stores the number of reported errors after the flanker task
                                   "bigObject.started", # allows to remove the flanker practice trials
                                   "straightObj", # the object images shown during the flanker task
                                   "task1_stim_keyResp.rt", #  this stores reaction time for each flanker trial
                                   "task_trial_loop.thisTrialN", # the counter of trials within each flanker block
                                   "errorPercent_text_box.text")] # stores the reported percentage of errors after the flanker task

  #remove practice trials and any rows that do not reflect experiment data
  remove_prac_trials <- subset(psychopyDatTrim, !complete.cases(psychopyDatTrim$bigObject.started)) # removes practice trials

  flankerDat <- subset(remove_prac_trials, complete.cases(remove_prac_trials$congruent)) # only keeps flanker trials. For this study, flankerDat should have only 384 rows!
  # Extracting flanker RT
  flankerDat$task1_stim_keyResp.rt <- str_replace_all(flankerDat$task1_stim_keyResp.rt,"\\[", "") # removes the bracket
  flankerDat$task1_stim_keyResp.rt <- str_replace_all(flankerDat$task1_stim_keyResp.rt,"\\]", "") # removes the bracket
  flankerDat$task1_stim_keyResp.rt <- gsub(",.*","",flankerDat$task1_stim_keyResp.rt) # removing the RT for the second response within the same trial.
  flankerDat$task1_stim_keyResp.rt <- as.numeric(flankerDat$task1_stim_keyResp.rt)


  # Lets add a column that tells how many responses have been made in a given flanker trial
  for (flanker_sub in 1:nrow(flankerDat)){
    response_keys <- str_extract_all(flankerDat$task1_stim_keyResp.keys[flanker_sub], "[A-Za-z]+") # to extract all sequences of characters from the input string
    # The response_keys is a list with 1 component. To count the number of elements in each component, we use "lengths" function.
    if (response_keys[[1]][1] == "None"){
      flankerDat$number_of_responses[flanker_sub] <- 0
    } else{
      flankerDat$number_of_responses[flanker_sub] <- lengths(response_keys)
    }
  }
  # We extract the first response and store it in the responses column of the flankerDat dataframe
   flankerDat$task1_stim_keyResp.keys <- str_extract(flankerDat$task1_stim_keyResp.keys, "[A-Za-z]+")

  # loop over all flanker trials.
  for (trial in 1:nrow(flankerDat)){
    current_trial_object <- flankerDat$straightObj[trial]
    current_trial_congruency <- flankerDat$congruent[trial]
    current_trial_rt <- flankerDat$task1_stim_keyResp.rt[trial]
    current_trial_resp_nums <- flankerDat$number_of_responses[trial] # number of responses for the current trial

    if (flankerDat$task_trial_loop.thisTrialN[trial] == 0){ # if the trial is the first in its block
      pre_trial_object <- NA
      pre_trial_congruency <- NA
      pre_trial_rt <- NA
      pre_trial_resp_nums <- NA
    } else {
      pre_trial_object <- flankerDat$straightObj[trial - 1]
      pre_trial_congruency <- flankerDat$congruent[trial - 1]
      pre_trial_rt <- flankerDat$task1_stim_keyResp.rt[trial - 1]
      pre_trial_resp_nums <- flankerDat$number_of_responses[trial - 1]
    }
    if (flankerDat$task_trial_loop.thisTrialN[trial] == 31){ # if the trial is the last in its block
      post_trial_object <- NA
      post_trial_congruency <- NA
      post_trial_rt <- NA
      post_trial_resp_nums <- NA
    } else {
      post_trial_object <- flankerDat$straightObj[trial + 1]
      post_trial_congruency <- flankerDat$congruent[trial + 1]
      post_trial_rt <- flankerDat$task1_stim_keyResp.rt[trial + 1]
      post_trial_resp_nums <- flankerDat$number_of_responses[trial + 1]
    }

    if (flankerDat$number_of_responses[trial] == 0){ # When no response made in a flanker task trial
      current_trial_responded <- 0 # 0 = not responded; 1 = responded
      # Because of an error in the Python code for the Psychopy task, the accuracy values reported by Psychopy are not correct in trials with no response.
      # Thus, I am putting 0s for accuracy in trials in which no response has been made!
      current_trial_accuracy <- 0 # Accuracy is considered 0 when there is no response
    } else if (flankerDat$number_of_responses[trial] >= 1){ # When a response made in a flanker task trial
      current_trial_responded <- 1
      current_trial_accuracy <- flankerDat$accuracy[trial]
    }
    if (flankerDat$task_trial_loop.thisTrialN[trial] == 0){ # if the trial is the first in its block
      pre_trial_responded <- NA
      pre_trial_accuracy <- NA
    } else {
      if (flankerDat$number_of_responses[trial - 1] == 0){ # When no response made in a flanker task trial
        pre_trial_responded <- 0 # 0 = not responded; 1 = responded
        pre_trial_accuracy <- 0 # Accuracy is considered 0 when there is no response
      } else if (flankerDat$number_of_responses[trial - 1 ] >= 1){ # When a response made in a flanker task trial
        pre_trial_responded <- 1
        pre_trial_accuracy <- flankerDat$accuracy[trial - 1]
      }
    }
    if (flankerDat$task_trial_loop.thisTrialN[trial] == 31){ # if the trial is the last in its block
      post_trial_responded <- NA
      post_trial_accuracy <- NA
    } else {
      if (flankerDat$number_of_responses[trial + 1] == 0){ # When no response made in a flanker task trial
        post_trial_responded <- 0 # 0 = not responded; 1 = responded
        post_trial_accuracy <- 0 # Accuracy is considered 0 when there is no response
      } else if (flankerDat$number_of_responses[trial + 1 ] >= 1){ # When a response made in a flanker task trial
        post_trial_responded <- 1
        post_trial_accuracy <- flankerDat$accuracy[trial + 1]
      }
    }

    if (current_trial_responded == 1 && current_trial_rt > 0.15 ){
      current_trial_legitResponse <- 1
    } else {
      current_trial_legitResponse <- 0
    }
    if (flankerDat$task_trial_loop.thisTrialN[trial] == 0){ # if this is the 1st trial
      pre_trial_legitResponse <- NA
    } else {
      if (pre_trial_responded == 1 && pre_trial_rt > 0.15 ){
        pre_trial_legitResponse <- 1
      } else {
        pre_trial_legitResponse <- 0
      }
    }

    if (flankerDat$task_trial_loop.thisTrialN[trial] == 31){ # if this is the 32nd (last) trial
      post_trial_legitResponse <- NA
    } else {
      if (post_trial_responded == 1 && post_trial_rt > 0.15 ){
        post_trial_legitResponse <- 1
      } else {
        post_trial_legitResponse <- 0
      }
    }


    flanker_df[nrow(flanker_df) + 1,] <-c(participant_id, current_trial_object, pre_trial_object, post_trial_object,
                                          current_trial_accuracy, pre_trial_accuracy, post_trial_accuracy,
                                          current_trial_congruency, pre_trial_congruency, post_trial_congruency,
                                          current_trial_rt, pre_trial_rt, post_trial_rt,
                                          current_trial_responded, pre_trial_responded, post_trial_responded,
                                          current_trial_legitResponse, pre_trial_legitResponse, post_trial_legitResponse,
                                          current_trial_resp_nums, pre_trial_resp_nums, post_trial_resp_nums)
  } # Closing the loop for each trial

  flanker_name <- paste0(participant_id, flanker_csv_fileName, sep = "", collapse = NULL)
  write.csv(flanker_df, paste(output_path, flanker_name, sep = "/", collapse = NULL), row.names=FALSE) # Writing the flanker CSV file to disk

  ## Creating the Surprise csv file for each participant
  # We have one surprise task in this study (i.e., Surprise memory)
  surprise_memory_dat <- subset(remove_prac_trials, complete.cases(remove_prac_trials$old_object_in_surp)) # keeps rows from the surprise memory task

  # Looping through surprise memory trials.
  for (surpTrial in 1:nrow(surprise_memory_dat)){
    object <- surprise_memory_dat$old_object_in_surp[surpTrial]
    if (surprise_memory_dat$which_side_old_object_displayed[surpTrial] == "right"){
      surpAcc <- ifelse(surprise_memory_dat$surprise_key_resp.keys[surpTrial] == "k", 1, 0) # correctly chose the old object, the output will be 1, otherwise 0
    } else if (surprise_memory_dat$which_side_old_object_displayed[surpTrial] == "left"){
      surpAcc <- ifelse(surprise_memory_dat$surprise_key_resp.keys[surpTrial] == "s", 1, 0) # correctly chose the old object, the output will be 1, otherwise 0
    }
    memory_surp_rt <- surprise_memory_dat$surprise_key_resp.rt[surpTrial]
    memory_surp_responses <- surprise_memory_dat$surprise_key_resp.keys[surpTrial]
    surprise_df[nrow(surprise_df) + 1,] <-c(participant_id, object, surpAcc, memory_surp_responses, memory_surp_rt)

  } # closing the surprise memory trial loop

  # Adding an additional column to surprise_df to identify whether we should keep that trial based on RT in the surprise memory task or not.
  for (kk in 1:nrow(surprise_df)){
    surprise_df$keep_surp_memory_trial_based_on_rt[kk] <- ifelse (surprise_df$memory_surp_rt[kk] > 0.2, 1, 0) # 0.2 is 200 msec for the reaction time.
  }

  # Adding a column that tells to which surprise block a given surp trial belongs.
  surprise_df$surp_block_num <- rep(1:8, each = 48)

  # If a participant has 3 blocks to be removed due to the following criteria, that participant will
  # be excluded. However, if a participant has two or less than two blocks for exclusion, we keep those blocks and the participant
  # and do not exclude them.
  # The Block exclusion criteria are:
  # 1. If a person has chosen the same response in >= 90% cases in a given surprise task block, that block will be marked for exclusion.
  # 2. If a person has chosen the same response for at least 20 times in a row in a given surprise block, that block will be marked for exclusion.

  # Add columns that identifies whether a given surprise block will be excluded or not as well as the number of used items
  # in responding to surprise trials (max will be 6).
  surprise_df <- surprise_df %>%
    add_column(exclude_block = NA, number_of_surp_resp_items_used = NA)
  surprise_df$number_of_surp_resp_items_used <- length(group_size(group_by(surprise_df,memory_surp_responses)))

  num_sub_blocks_excluded <- 0 # will count the number of to-be-excluded blocks for the current participant
  for (surp_block in 1:8){
    # Step 1: Select rows of the current surprise block from the surprise_df that has data of all blocks.
    selected_rows <- surprise_df[surprise_df$surp_block_num == surp_block, ]

    # Step 2: Compute the percentage of the most used response in the current surprise task.
    percent_of_max_surp_response <- (max(group_size(group_by(selected_rows,memory_surp_responses)))/sum(group_size(group_by(selected_rows,memory_surp_responses)))) * 100 # Percentage of the most chosen response in this block

    # Step 3: If the the most common response has been used >= 90 %, the current block will be marked for exclusion.
    if (percent_of_max_surp_response >= 90){
      surprise_df[surprise_df$surp_block_num == surp_block, "exclude_block"] <- 1
      ### Printing output
      print(paste("Block ", surp_block," of participant", participant_id," was makrked for exclusion due to response frequency criterion."))
      #### end of printing output
      num_sub_blocks_excluded <- num_sub_blocks_excluded + 1
      # we use 'next' to jump to the next block without further checking for the second criterion.
      next # jumps to the next iteration of the 'for' loop which is the next block
    } else if (percent_of_max_surp_response < 90){
      surprise_df[surprise_df$surp_block_num == surp_block, "exclude_block"] <- 0
      # We check for the second criterion:
      # 2. If a person has chosen the same response for at least 20 times in a row in a given
      # surprise block, that block will be marked for exclusion.

      for (surprise_block_trial in 1:(nrow(selected_rows) - 19)){
        the_same_surp_answer_counter <- 1
        ref_surp_answer <- selected_rows$memory_surp_responses[surprise_block_trial]
        for (ggg in 1:19){
          following_surp_answer <- selected_rows$memory_surp_responses[surprise_block_trial + ggg]
          same_answer <- ifelse (ref_surp_answer == following_surp_answer, 1, 0)
          if (same_answer){
            the_same_surp_answer_counter <- the_same_surp_answer_counter + 1
          } else{
            break # the answer is different. so, I break the counter loop to proceed with the following surprise_block_trial.
          }
        }
        # Check to see if the same answer counter is at 20.
        if (the_same_surp_answer_counter >= 20){
          surprise_df[surprise_df$surp_block_num == surp_block, "exclude_block"] <- 1
          num_sub_blocks_excluded <- num_sub_blocks_excluded + 1
          ### Printing output
          print(paste("Participant ", participant_id, " has repeated ans in block ", surp_block))
          #### end of printing output
          break # break this loop to jump to the next block
        }
      }
    }
  }
  surprise_df$num_blocks_excluded <- num_sub_blocks_excluded

  surprise_name <- paste0(participant_id, surprise_csv_fileName, sep = "", collapse = NULL)
  write.csv(surprise_df, paste(output_path, surprise_name, sep = "/", collapse = NULL), row.names=FALSE) # Writing the surprise CSV file to disk


} # closing the loop for each participant


