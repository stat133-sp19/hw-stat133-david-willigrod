#Data Preparation 
##prepares data for GSW players by aggregating raw data of each individual player
###inputs: CSV files of GSW players 
###outputs: a csv file with required variables for visualization
library(readr)
library(dplyr)


iguodala <- read.csv("data/andre-iguodala.csv", stringsAsFactors = FALSE)
green <- read.csv("data/draymond-green.csv", stringsAsFactors = FALSE)
durant <- read.csv("data/kevin-durant.csv", stringsAsFactors = FALSE)
thompson <- read.csv("data/klay-thompson.csv", stringsAsFactors = FALSE)
curry <- read.csv("data/stephen-curry.csv", stringsAsFactors = FALSE)

iguodala$name <- "Andre Iguodala"
green$name <- "Draymond Green"
durant$name <- "Kevin Durant"
thompson$name <- "Klay Thompson"
curry$name <- "Stephen Curry"

iguodala$shot_made_flag[iguodala$shot_made_flag =='n'] <- 'shot_no'
iguodala$shot_made_flag[iguodala$shot_made_flag =='y'] <- 'shot_yes'
green$shot_made_flag[green$shot_made_flag =='n'] <- 'shot_no'
green$shot_made_flag[green$shot_made_flag =='y'] <- 'shot_yes'
durant$shot_made_flag[durant$shot_made_flag =='n'] <- 'shot_no'
durant$shot_made_flag[durant$shot_made_flag =='y'] <- 'shot_yes'
thompson$shot_made_flag[thompson$shot_made_flag =='n'] <- 'shot_no'
thompson$shot_made_flag[thompson$shot_made_flag =='y'] <- 'shot_yes'
curry$shot_made_flag[curry$shot_made_flag =='n'] <- 'shot_no'
curry$shot_made_flag[curry$shot_made_flag =='y'] <- 'shot_yes'

iguodala$minute <- (12 * (iguodala$period-1)) + (12 - iguodala$minutes_remaining)
green$minute <- (12 * (green$period-1)) + (12 - green$minutes_remaining)
durant$minute <- (12 * (durant$period-1)) + (12 - durant$minutes_remaining)
thompson$minute <- (12 * (thompson$period-1)) + (12 - thompson$minutes_remaining)
curry$minute <- (12 * (curry$period-1)) + (12 - curry$minutes_remaining)

sink("output/anre-iguodala-summary.txt")
summary(iguodala)
sink()

sink("output/draymond-green-summary.txt")
summary(green)
sink()

sink("output/kevin-durant-summary.txt")
summary(durant)
sink()

sink("output/klay-thompson-summary.txt")
summary(thompson)
sink()

sink("output/stephen-curry-summary.txt")
summary(curry)
sink()

shots <- rbind(iguodala, green, durant, thompson, curry)

tbl_df(shots)

save(shots, file = "./data/shots-data.csv")

sink("output/shots-data-summary.txt")
summary(shots)
sink()

save.image("code/shots-data-tables.RData")

two_point_shooting <- shots %>%
  group_by(name) %>%
  filter(shot_type == '2PT Field Goal') %>%
  summarise(total = n(),
            made = sum(shot_made_flag == 'shot_yes'),
            perc_made = sum(shot_made_flag == 'shot_yes')/ n(),
            eperc_made = perc_made)

three_point_shooting <- shots %>%
  group_by(name) %>%
  filter(shot_type == '3PT Field Goal') %>%
  summarise(total = n(),
            made = sum(shot_made_flag == 'shot_yes'),
            perc_made = sum(shot_made_flag == 'shot_yes')/ n(),
            eperc_made = perc_made * 1.5)

both <- rbind(two_point_shooting, three_point_shooting)

effective_shooting<- both %>%
  group_by(name) %>%
  summarise(total_shots = sum(total),
            made = sum(made),
            perc_made = made/total_shots,
            eperc_made = sum(total * eperc_made)/total_shots)

team_effective_shooting <- both %>%
  summarise(total_shots = sum(total),
            made = sum(made),
            perc_made = made/total_shots,
            eperc_made = sum(total * eperc_made)/total_shots)

save.image('code/make-shots-data-script-image.RData')
