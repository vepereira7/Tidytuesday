library(tidyverse)
library(ggplot2)
library(tidyr)
library(dplyr)
library(lubridate)
library(gganimate)

# read raw data

ds <- readr::read_csv(FILEPATH)

#dataset lenght
dim(ds)

#check outliers

#check NA values
sum(is.na(ds)) #how much
which(is.na(ds))#positions

#if there is NA
dim(ds)
ds <- ds %>% drop_na()

#data wrangling
plot_data <- ds %>% select(artist_name, year, artist_nationality_other, artist_gender, book, space_ratio_per_page_total) %>%
	group_by(artist_nationality_other, artist_gender)%>%
	mutate(space = mean(space_ratio_per_page_total))

#plot 
cor = c('pink', 'lightblue')
plot <- ggplot(plot_data, 
	aes(x=artist_nationality_other, y=space)) + 
    geom_point(color='black',
     shape=21, 
     size=4, 
     aes(fill=(artist_gender))) + 
    scale_fill_manual(values=c('pink', 'lightblue')) + 
    labs(title = 'Space ratio per page for each country, by gender',
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


plot_data_2 <- ds %>% select(artist_race_nwi, year, artist_nationality_other, artist_gender, book, space_ratio_per_page_total) %>%
    group_by(artist_race_nwi, year)%>%
    summarise(space = mean(space_ratio_per_page_total))

 
plot_2 <- ggplot(data = plot_data_3, aes(fill=artist_race_nwi, y=artist_nationality_other, x=space)) +
    geom_bar(position="dodge", stat="identity") +
    scale_fill_manual(values=c('pink', 'lightblue')) +
    labs(title = 'Space ratio per page during the years, by race',
    	subtitle = 'Year: {frame_time}',
    	fill = 'Race',
    	x = 'Years',
    	y = 'Space ratio',
    	caption = paste( "Vitor Pereira |" , today() )) + 
    theme(panel.background = element_rect(fill = "white", colour = "grey50"),
    	plot.title = element_text(hjust = 0.5, face = "bold", size = 18, family = 'Monaco'),
    	axis.title.x = element_text(family = 'Monaco', size = 10, vjust = -1),
    	axis.title.y = element_text(family = 'Monaco', size = 10, vjust = 1),
    	legend.title = element_text(family = 'Monaco', face = 'bold'),
    	legend.text = element_text(family = 'Monaco'),
    	plot.caption = element_text(face = 'bold')) + 
    transition_time(time = year) 


#ANIMATED LINE PLOT + DOT PLOT
