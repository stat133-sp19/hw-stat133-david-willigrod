---
title: "Data Dictionary"
output: github_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(eval = FALSE)
```

#Data Dictionary
team_name: character, name of the team

game_date: character, date of the game

season: numberic, season year

period: numberic, An NBA game is divided in 4 periods of 12 mins each. For example, a value of period = 1 refers to the first period (the first 12 mins of the game)

minutes_remaining: numeric, amount of time remaining in a given period

seconds_remaining: numeric, amount of seconds remaining after the minutes_remaining has been accounted for

shot_made_flag: character, was the shot made (y or n)

action_type: character, basketball moves used by players, either to pass by defenders to gain access to the basket, or to get a clean pass to a teammate to score a two pointer or three pointer

shot_type: character, indicates whether a shot is a 2-point field goal, or a 3-point field goal

shot_distance: numeric, distance to the basket (measured in feet)

opponent: character, opposing team

x: numeric, x coordinate (in inches) of where shot occurred

y: numeric, y coordinate (in inches) of where shot occurred