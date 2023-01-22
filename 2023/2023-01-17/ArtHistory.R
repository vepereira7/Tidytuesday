library(tidyverse)
library(ggplot2)
library(tidyr)
library(dplyr)
library(lubridate)
library(gganimate)

# read raw dataset
ds <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-01-17/artists.csv')

#check NA values
sum(is.na(ds)) #how much

#if there is NA
dim(ds)
ds <- ds %>% drop_na()
dim(ds)

#data wrangling
plot_data <- ds %>% select(artist_name, year, artist_nationality_other, artist_gender, book, space_ratio_per_page_total) %>%
	group_by(artist_nationality_other, artist_gender)%>%
	mutate(space = mean(space_ratio_per_page_total))

#plot 
plot <- ggplot(plot_data, 
	aes(x=artist_nationality_other, y=space)) + 
    geom_point(color='black',
     shape=21, 
     size=4, 
     aes(fill=(artist_gender))) + 
    scale_fill_manual(values=c('pink', 'lightblue')) + 
    labs(title = 'Space ratio mean per page for each country, by gender',
    	fill = 'Gender',
    	x = 'Countries',
    	y = 'Space ratio',
    	caption = paste( "Vitor Pereira |" , today() )) +
    theme(panel.background = element_rect(fill = "white", colour = "grey50"),
    	plot.title = element_text(hjust = 0.5, face = "bold", size = 18, family = 'Monaco'),
    	axis.title.x = element_text(family = 'Monaco', size = 10, vjust = -1),
    	axis.title.y = element_text(family = 'Monaco', size = 10, vjust = 1),
    	legend.title = element_text(family = 'Monaco', face = 'bold'),
    	legend.text = element_text(family = 'Monaco'),
    	plot.caption = element_text(face = 'bold'))

ggsave( filename = "plot_p5.png", plot = plot , width = 20, height = 15, units = "cm", dpi = 600)
