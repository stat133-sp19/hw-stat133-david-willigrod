#Creating Shot Charts
##create pdf of shot chart for Warriors players
###inputs: data.frame of players shots
###outputs; pdf of shot chart
library(dplyr)
library(ggplot2)
library(readr)
library(jpeg)
library(grid)

load("code/shots-data-tables.RData")
klay_scatterplot <- ggplot(data = thompson) +
  geom_point(aes(x = x, y = y, color = shot_made_flag))

# court image (to be used as background of plot)
court_file <- "images/nba-court.jpg"

# create raste object
court_image <- rasterGrob(
  readJPEG(court_file),
  width = unit(1, "npc"),
  height = unit(1, "npc"))

# shot chart with court background
#pdf(file = 'images/klay-thompson-shot-chart.pdf', width = 6.5, height = 5)
klay_shot_chart <- ggplot(data = thompson) +
  annotation_custom(court_image, -250, 250, -50, 420) +
  geom_point(aes(x = x, y = y, color = shot_made_flag)) +
  ylim(-50, 420) +
  ggtitle('Shot Chart: klay thompson (2016 season)') +
  theme_minimal()
#dev.off()

#pdf(file = 'images/andre-iguodala-shot-chart.pdf', width = 6.5, height = 5)
andre_shot_chart <- ggplot(data = iguodala) +
  annotation_custom(court_image, -250, 250, -50, 420) +
  geom_point(aes(x = x, y = y, color = shot_made_flag)) +
  ylim(-50, 420) +
  ggtitle('Shot Chart: andre iguodala (2016 season)') +
  theme_minimal()
#dev.off()

#pdf(file = 'images/draymond-green-shot-chart.pdf', width = 6.5, height = 5)
draymond_shot_chart <- ggplot(data = green) +
  annotation_custom(court_image, -250, 250, -50, 420) +
  geom_point(aes(x = x, y = y, color = shot_made_flag)) +
  ylim(-50, 420) +
  ggtitle('Shot Chart: draymond green (2016 season)') +
  theme_minimal()
#dev.off()

#pdf(file = 'images/kevin-durant-shot-chart.pdf', width = 6.5, height = 5)
kevin_shot_chart <- ggplot(data = durant) +
  annotation_custom(court_image, -250, 250, -50, 420) +
  geom_point(aes(x = x, y = y, color = shot_made_flag)) +
  ylim(-50, 420) +
  ggtitle('Shot Chart: kevin durant (2016 season)') +
  theme_minimal()
#dev.off()

#pdf(file = 'images/stephen-curry-shot-chart.pdf', width = 6.5, height = 5)
stephen_shot_chart <- ggplot(data = curry) +
  annotation_custom(court_image, -250, 250, -50, 420) +
  geom_point(aes(x = x, y = y, color = shot_made_flag)) +
  ylim(-50, 420) +
  ggtitle('Shot Chart: stephen curry (2016 season)') +
  theme_minimal()

ggsave(filename = 'images/klay-thompson-shot-chart.pdf', 
       plot = klay_shot_chart,
       device = 'pdf',
       width = 6.5, height = 5)

ggsave(filename = 'images/klay-thompson-shot-chart.png', 
       plot = klay_shot_chart,
       device = 'png',
       width = 6.5, height = 5)

ggsave(filename = 'images/andre-iguodala-shot-chart.pdf', 
       plot = andre_shot_chart,
       device = 'pdf',
       width = 6.5, height = 5)

ggsave(filename = 'images/andre-iguodala-shot-chart.png', 
       plot = andre_shot_chart,
       device = 'png',
       width = 6.5, height = 5)

ggsave(filename = 'images/draymond-green-shot-chart.pdf', 
       plot = draymond_shot_chart,
       device = 'pdf',
       width = 6.5, height = 5)

ggsave(filename = 'images/draymond-green-shot-chart.png', 
       plot = draymond_shot_chart,
       device = 'png',
       width = 6.5, height = 5)

ggsave(filename = 'images/kevin-durant-shot-chart.pdf', 
       plot = kevin_shot_chart,
       device = 'pdf',
       width = 6.5, height = 5)

ggsave(filename = 'images/kevin-durant-shot-chart.png', 
       plot = kevin_shot_chart,
       device = 'png',
       width = 6.5, height = 5)

ggsave(filename = 'images/stephen-curry-shot-chart.pdf', 
       plot = stephen_shot_chart,
       device = 'pdf',
       width = 6.5, height = 5)

ggsave(filename = 'images/stephen-curry-shot-chart.png', 
       plot = stephen_shot_chart,
       device = 'png',
       width = 6.5, height = 5)

shots_chart <- ggplot(data = shots) +
  annotation_custom(court_image, -250, 250, -50, 420) +
  geom_point(aes(x = x, y = y, color = shot_made_flag), size = .5) +
  ylim(-50, 420) +
  ggtitle('Shot Charts: GSW (2016 season)') +
  theme_minimal() + 
  facet_wrap(~name, nrow = 2) +
  scale_color_discrete(name="Made Shot",
                      breaks=c("shot_yes", "shot_no"),
                      labels=c("yes", "no"))

ggsave(filename = 'images/gsw-shot-charts.pdf', 
       device = 'pdf',
       width = 8, height = 7)

ggsave(filename = "images/gsw-shot-charts.png",
       device = 'png',
       width = 8, height = 7, units = "in")
