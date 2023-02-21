# Package names
packages <- c("tidyverse", "ggplot2", "devtools", "StatsBombR", "SBpitch", "hrbrthemes", "grid")

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Packages loading
invisible(lapply(packages, library, character.only = TRUE))


# For this project, let's explore the English Premier League
free_competitions <- FreeCompetitions() %>%
  filter(competition_id == 43 & season_id == 106)
matches <- FreeMatches(free_competitions)
matches <- matches %>%
  arrange(match_date, kick_off) %>%
  mutate('GameNum' = 1:nrow(matches)) %>%
  mutate(MatchNum = paste('Match:', GameNum)) %>%
  mutate(Match = paste('Match', GameNum, ':', home_team.country.name, 'vs.', away_team.country.name)) %>%
  select(-c(GameNum))


# Get all the match events
StatsBombData <- free_allevents(MatchesDF = matches, Parallel = T)
StatsBombData <- allclean(StatsBombData)

# Create a table for the overall match stats
match_stats <- matches %>%
  select(match_id, match_date, kick_off, Match, stadium.id, stadium.name, home_team.country.name, away_team.country.name) 

match_stats_temp <- StatsBombData %>%
  group_by(match_id, team.name) %>%
  summarise(shots = sum(type.name == "Shot", na.rm = TRUE),
            goals = sum(shot.outcome.name == "Goal", na.rm = TRUE),
            conversion_rate = round(sum(shot.outcome.name=="Goal", na.rm = TRUE)/sum(type.name=="Shot", na.rm = TRUE)*100, 2),
            passes = sum(type.name == "Pass", na.rm = TRUE),
            xG = round(sum(shot.statsbomb_xg, na.rm = TRUE), 2),
            foulWon = sum(type.name == "Foul Won", na.rm = TRUE),
            foulCommited = sum(type.name == "Foul Committed", na.rm = TRUE))

match_stats <- merge(match_stats, match_stats_temp, by = "match_id", all.x = TRUE, all.y = FALSE)

# Remove the unneccesary datasets and objects
rm(match_stats_temp)

# Number Of Goals
match_stats %>%
  ggplot(aes(x = reorder(team.name, -goals), y = goals)) + 
  geom_bar(stat = "identity", fill = rgb(139, 22, 47, max = 255)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  labs(x = "Team",
       y = "Goals",
       title = "Number Of Goals Throughout The Tournament",
       subtitle = "FIFA World Cup 2022 Qatar") + 
  scale_fill_manual(values = c(rgb(139, 22, 47, max = 255), rgb(169, 133, 58, max = 255)))



##############################################################################################################################
##############################################################################################################################
heatmap = StatsBombData %>%mutate(location.x = ifelse(location.x>120, 120, location.x),
                           location.y = ifelse(location.y>80, 80, location.y),
                           location.x = ifelse(location.x<0, 0, location.x),
                           location.y = ifelse(location.y<0, 0, location.y)) #1 
heatmap$xbin <- cut(heatmap$location.x, breaks = seq(from=0, to=120, by = 20),include.lowest=TRUE )
heatmap$ybin <- cut(heatmap$location.y, breaks = seq(from=0, to=80, by = 20),include.lowest=TRUE) #2
defensiveactivitycolors <- c("#dc2429", "#dc2329", "#df272d", "#df3238", "#e14348", "#e44d51",
                                      "#e35256", "#e76266", "#e9777b", "#ec8589", "#ec898d", "#ef9195",
                                      "#ef9ea1", "#f0a6a9", "#f2abae", "#f4b9bc", "#f8d1d2", "#f9e0e2",
                                      "#f7e1e3", "#f5e2e4", "#d4d5d8", "#d1d3d8", "#cdd2d6", "#c8cdd3", "#c0c7cd",
                                      "#b9c0c8", "#b5bcc3", "#909ba5", "#8f9aa5", "#818c98", "#798590",
                                      "#697785", "#526173", "#435367", "#3a4b60", "#2e4257", "#1d3048",
                                      "#11263e", "#11273e", "#0d233a", "#020c16") #1
heatmap = heatmap%>%
  filter(type.name=="Pressure" | duel.type.name=="Tackle" | 
           type.name=="Foul Committed" | type.name=="Interception" | 
           type.name=="Block" ) %>%
  group_by(team.name) %>%
  mutate(total_DA = n()) %>%
  group_by(team.name, xbin, ybin) %>%
  summarise(total_DA = max(total_DA),
            bin_DA = n(),
            bin_pct = bin_DA/total_DA,
            location.x = median(location.x),
            location.y = median(location.y)) %>%
  group_by(xbin, ybin) %>%
  mutate(league_ave = mean(bin_pct)) %>%
  group_by(team.name, xbin, ybin) %>%
  mutate(diff_vs_ave = bin_pct - league_ave) #
ggplot(data = heatmap, aes(x = location.x, y = location.y, fill = diff_vs_ave, group = diff_vs_ave)) +
  geom_bin2d(binwidth = c(20, 20), position = "identity", alpha = 0.9) + #2
  annotate("rect",xmin = 0, xmax = 120, ymin = 0, ymax = 80, fill = NA, colour = "black", size = 0.6) +
  annotate("rect",xmin = 0, xmax = 60, ymin = 0, ymax = 80, fill = NA, colour = "black", size = 0.6) +
  annotate("rect",xmin = 18, xmax = 0, ymin = 18, ymax = 62, fill = NA, colour = "white", size = 0.6) +
  annotate("rect",xmin = 102, xmax = 120, ymin = 18, ymax = 62, fill = NA, colour = "white", size = 0.6) +
  annotate("rect",xmin = 0, xmax = 6, ymin = 30, ymax = 50, fill = NA, colour = "white", size = 0.6) +
  annotate("rect",xmin = 120, xmax = 114, ymin = 30, ymax = 50, fill = NA, colour = "white", size = 0.6) +
  annotate("rect",xmin = 120, xmax = 120.5, ymin =36, ymax = 44, fill = NA, colour = "black", size = 0.6) +
  annotate("rect",xmin = 0, xmax = -0.5, ymin =36, ymax = 44, fill = NA, colour = "black", size = 0.6) +
  annotate("segment", x = 60, xend = 60, y = -0.5, yend = 80.5, colour = "white", size = 0.6)+
  annotate("segment", x = 0, xend = 0, y = 0, yend = 80, colour = "black", size = 0.6)+
  annotate("segment", x = 120, xend = 120, y = 0, yend = 80, colour = "black", size = 0.6)+
  theme(rect = element_blank(),
        line = element_blank()) +
  annotate("point", x = 12 , y = 40, colour = "white", size = 1.05) +
  annotate("point", x = 108 , y = 40, colour = "white", size = 1.05) +
  annotate("path", colour = "white", size = 0.6,
           x=60+10*cos(seq(0,2*pi,length.out=2000)),
           y=40+10*sin(seq(0,2*pi,length.out=2000)))+
  annotate("point", x = 60 , y = 40, colour = "white", size = 1.05) +
  annotate("path", x=12+10*cos(seq(-0.3*pi,0.3*pi,length.out=30)), size = 0.6,
           y=40+10*sin(seq(-0.3*pi,0.3*pi,length.out=30)), col="white") +
  annotate("path", x=108-10*cos(seq(-0.3*pi,0.3*pi,length.out=30)), size = 0.6,
           y=40-10*sin(seq(-0.3*pi,0.3*pi,length.out=30)), col="white") + #3
  theme(axis.text.x=element_blank(),
       axis.title.x = element_blank(),
       axis.title.y = element_blank(),
       plot.caption=element_text(size=13,family="Source Sans Pro", hjust=0.5, vjust=0.5),
       plot.subtitle = element_text(size = 18, family="Source Sans Pro", hjust = 0.5),
       axis.text.y=element_blank(),
       legend.title = element_blank(),
       legend.text=element_text(size=22,family="Source Sans Pro"),
       legend.key.size = unit(1.5, "cm"),
       plot.title = element_text(margin = margin(r = 10, b = 10), face="bold",size = 32.5, 
                                 family="Source Sans Pro", colour = "black", hjust = 0.5),
       legend.direction = "vertical",
       axis.ticks=element_blank(),
       plot.background = element_rect(fill = "white"),
       strip.text.x = element_text(size=13,family="Source Sans Pro")) + #4
  scale_y_reverse() + #5
  scale_fill_gradientn(colours = defensiveactivitycolors, trans = "reverse", labels = 
                         scales::percent_format(accuracy = 1), limits = c(0.03, -0.03)) + #6
  labs(title = "Where Do Teams Defend vs Tournament Average?", subtitle = "FIFA World Cup 2022") + #7
  coord_fixed(ratio = 95/100) + #8
  annotation_custom(grob = linesGrob(arrow=arrow(type="open", ends="last",
                                                 length=unit(2.55,"mm")), gp=gpar(col="black", fill=NA, lwd=2.2)),
                    xmin=25, xmax = 95, ymin = -83, ymax = -83) + #9
  facet_wrap(~team.name)+ #10
  guides(fill = guide_legend(reverse = TRUE)) #11

##############################################################################################################################
##############################################################################################################################



