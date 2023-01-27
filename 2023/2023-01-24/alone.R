library(tidyverse)
library(ggplot2)
library(tidyr)
library(dplyr)
library(lubridate)
library(gganimate)
library(showtext)

font_add_google('Raleway')
font_add_google('Mulish')



survivalists <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-01-24/survivalists.csv')

# Medical evacuations per season, by gender
df <- survivalists %>% select(season, age, gender, medically_evacuated) %>% 
	group_by(season, gender) %>% 
	summarise(medical = sum(medically_evacuated, na.rm = TRUE))%>% 
	rename(Gender = gender)

plot <- ggplot(data = df, aes(x=season, y=medical, colour = Gender, shape = Gender)) + 
	geom_line() + 
	geom_point() +
	scale_colour_manual(values=c('hotpink', 'deepskyblue')) +
	scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9))+
	labs(title = 'ALONE',
		caption = paste( "Vitor Pereira | Source: Alone data package"),
		x = 'Season',
		y = 'Medical Evacuations',
		subtitle = 'On average, Males tend to need more medical assistance than Females')+
	theme(plot.background = element_rect(fill = "mintcream"),
		panel.border = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line( size= 0.5, color="black"),
		panel.grid.minor.x = element_blank(),
		panel.background = element_rect(fill = "mintcream", color = NA),
		plot.title = element_text(hjust = 0.5, face = "bold", size = 18, family = 'Raleway'),
		plot.subtitle = element_text(hjust = 0.5, family = 'Raleway', size = 10),
		legend.title = element_text(family = 'Raleway', face = 'bold'),
		axis.title.x = element_text(family = 'Raleway', size = 15, vjust = -1),
    	axis.title.y = element_text(family = 'Raleway', size = 15, vjust = 1),
		legend.background = element_rect(fill="mintcream", size=0.5, linetype="solid", colour ="black"),
		legend.key = element_rect(fill = "mintcream", colour = NA),
		legend.text = element_text(family = 'Mulish'))

ggsave( filename = "alone.png", plot = plot , width = 20, height = 15, units = "cm", dpi = 600)



