# Package names
packages <- c("tidyverse", "ggplot2", "devtools", "StatsBombR", "SBpitch")

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Packages loading
invisible(lapply(packages, library, character.only = TRUE))


# See all the competitions that are free competitions
free_competitions <- FreeCompetitions()
Mathces <- FreeMatches(free_competitions)
StatsBombData <- StatsBombFreeEvents()


# For this project, let's explore the FIFA World Cup Stats
free_competitions <- FreeCompetitions() %>%
  filter(competition_id == 43 & season_id == 106)
matches <- FreeMatches(free_competitions)
StatsBombData <- free_allevents(MatchesDF = matches, Parallel = T)
StatsBombData <- allclean(StatsBombData)


shots_goals = StatsBombData %>%
  group_by(team.name) %>% #1
  summarise(shots = sum(type.name=="Shot", na.rm = TRUE),
            goals = sum(shot.outcome.name=="Goal", na.rm = TRUE)) #2

shots_goals_per_game = StatsBombData %>%
  group_by(team.name) %>%
  summarise(shots = sum(type.name=="Shot", na.rm = TRUE)/n_distinct(match_id),
            goals = sum(shot.outcome.name=="Goal", na.rm = TRUE)/n_distinct(match_id))

ggplot(data = shots_goals,
       aes(x = reorder(team.name, shots), y = shots)) + #1
  geom_bar(stat = "identity", width = 0.5) + #2
  labs(y="Shots") + #3
  theme(axis.title.y = element_blank()) + #4
  scale_y_continuous( expand = c(0,0)) + #5
  coord_flip() + #6
  theme_SB() #7



player_shots = StatsBombData %>%
  group_by(player.name, player.id) %>%
  summarise(shots = sum(type.name=="Shot", na.rm = TRUE)) #1
player_minutes = get.minutesplayed(StatsBombData) #2
player_minutes = player_minutes %>%
  group_by(player.id) %>%
  summarise(minutes = sum(MinutesPlayed)) #3
player_shots = left_join(player_shots, player_minutes) #4
player_shots = player_shots %>% mutate(nineties = minutes/90) #5
player_shots = player_shots %>% mutate(shots_per90 = shots/nineties) #6
