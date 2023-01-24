# Package names
packages <- c("tidyverse", "ggplot2", "devtools", "StatsBombR", "SBpitch", "hrbrthemes")

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Packages loading
invisible(lapply(packages, library, character.only = TRUE))


# For this project, let's explore the FIFA World Cup Stats
free_competitions <- FreeCompetitions() %>%
  filter(competition_id == 43 & season_id == 106)
matches <- FreeMatches(free_competitions)
StatsBombData <- free_allevents(MatchesDF = matches, Parallel = T)
StatsBombData <- allclean(StatsBombData)


# Overall competition stats

# 1.0. Shots & Goals
shots_goals <- StatsBombData %>%
  group_by(team.name) %>% #1
  summarise(shots = sum(type.name=="Shot", na.rm = TRUE),
            goals = sum(shot.outcome.name=="Goal", na.rm = TRUE),
            shots_per_game = sum(type.name=="Shot", na.rm = TRUE)/n_distinct(match_id),
            goals_per_game = sum(shot.outcome.name=="Goal", na.rm = TRUE)/n_distinct(match_id),
            conversion_rate = round(sum(shot.outcome.name=="Goal", na.rm = TRUE)/sum(type.name=="Shot", na.rm = TRUE)*100, 2))


# Overall Shots & Goals
rbind(shots_goals %>% select(team.name, shots) %>% rename("value" = shots) %>% mutate(shots_and_goals = "Shots"),
      shots_goals %>% select(team.name, goals) %>% rename("value" = goals) %>% mutate(shots_and_goals = "Goals")
) %>%
  ggplot(aes(x = reorder(team.name, -value), y = value, fill = factor(shots_and_goals, levels = c('Shots', 'Goals')))) + 
  geom_bar(stat = "identity", position = "dodge", colour = "black") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  labs(x = "Team",
       y = "Shots And Goals",
       title = "Number Of Shots & Goals Through Tournament",
       subtitle = "FIFA World Cup 2022 Qatar") + labs(fill="Shots & Goals") +
  scale_fill_manual(values = c(rgb(139, 22, 47, max = 255), rgb(169, 133, 58, max = 255)))

# Conversion Rate
shots_goals %>%
  ggplot(aes(x = reorder(team.name, -conversion_rate), y = conversion_rate)) + 
  geom_bar(stat = "identity", position = "dodge", colour = "black", fill = rgb(139, 22, 47, max = 255)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  labs(x = "Team",
       y = "Conversion Rate (%)",
       title = "Shots To Goals Conversion Percentage",
       subtitle = "FIFA World Cup 2022 Qatar")


# What percentage of goals were scored by midfield forward and defense in the tournament
StatsBombData %>%
  group_by(team.name, player.name, player.id, position.name) %>%
  summarise(shots = sum(type.name=="Shot", na.rm = TRUE),
            goals = sum(shot.outcome.name=="Goal", na.rm = TRUE)) %>%
  drop_na() %>%
  mutate(position_simplified = if_else(position.name %in% c("Left Midfield", "Right Midfield", "Left Center Midfield",
                                                                         "Center Defensive Midfield", "Left Defensive Midfield",
                                                                         "Right Center Midfield", "Right Defensive Midfield",
                                                                         "Center Attacking Midfield", "Left Attacking Midfield", 
                                                                         "Right Attacking Midfield"), "Midfield",
                                       if_else(position.name %in% c("Left Wing", "Right Wing", "Center Forward",
                                                                                 "Left Center Forward", "Right Center Forward"), "Forward",
                                               if_else(position.name %in% c("Center Back", "Right Center Back", "Right Wing Back",
                                                                                         "Right Back", "Left Center Back", "Left Back", 
                                                                                         "Left Wing Back"), "Defense",
                                                       if_else(position.name == "Goalkeeper", "Goal-Keeper", "Substitute"))))) %>%
  group_by(position_simplified) %>%
  summarise(goals = sum(goals, na.rm = TRUE)) %>%
  arrange(desc(position_simplified)) %>%
  mutate(prop = goals / sum(goals) * 100) %>%
  mutate(ypos = cumsum(prop) - 0.5*prop) %>%
  ggplot(aes(x = "", y = prop, fill = position_simplified)) + geom_bar(stat="identity", width=3, color="black") +
    coord_polar("y", start=0) +
    theme_void() + 
    theme(legend.position="right") +
  labs(title = "Goals From Playing Position Through Tournament",
       subtitle = "FIFA World Cup 2022 Qatar") + labs(fill="Playing Position") 



player_shots = StatsBombData %>%
  group_by(team.name, player.name, player.id) %>%
  summarise(shots = sum(type.name=="Shot", na.rm = TRUE),
            goals = sum(shot.outcome.name=="Goal", na.rm = TRUE)) #1
player_minutes = get.minutesplayed(StatsBombData) #2
player_minutes = player_minutes %>%
  group_by(player.id) %>%
  summarise(minutes = sum(MinutesPlayed)) #3
player_shots = left_join(player_shots, player_minutes) #4
player_shots = player_shots %>% mutate(nineties = minutes/90) #5
player_shots = player_shots %>% mutate(shots_per90 = shots/nineties) #6

