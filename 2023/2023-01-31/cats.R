#libraries
library(tidyverse)
library(ggplot2)
library(tidyr)
library(dplyr)
library(lubridate)
library(showtext)
library(ggtext)

#add fonts
#font_add_google('Teko')

# load data
cats_uk_reference <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-01-31/cats_uk_reference.csv')

# data wrangling
df <- cats_uk_reference %>% select(tag_id, animal_id, prey_p_month, animal_sex, age_years) %>%
	subset(tag_id != 'Lightening Bugg-Tag') %>%
	group_by(age_years, animal_sex) %>%
	summarise(prey = sum(prey_p_month)) %>%
	mutate(prey = round(prey)) %>%
	filter(age_years < 13) %>%
	rename(Gender = animal_sex)


df$Gender[df$Gender == 'm'] <- 'Male'
df$Gender[df$Gender == 'f'] <- 'Female'

# df for males and females
males <- df %>%
  filter(Gender == "Male")
females <- df %>%
  filter(Gender == "Female")

# mean pear gender
df %>% group_by(Gender) %>%
  summarise(mean = mean(prey))-> stats
stats_males <- stats %>%
  filter(Gender == "Male")
stats_females <- stats %>%
  filter(Gender == "Female")

# diff between gender
df2 <- males %>% 
	full_join(females) %>%
	arrange(age_years) %>%
	mutate(diff = abs(prey - lag(prey)))
diff <- df2 %>% 
  filter(Gender == "Female") %>% 
  mutate(x_pos = 57)

# plot
plot <- ggplot(df) +
    geom_vline(xintercept = stats_males$mean, linetype = "dashed", size = .5, alpha = .8, color = "#00BFFF") +
    geom_vline(xintercept = stats_females$mean, linetype = "dashed", size = .5, alpha = .8, color = "#FF69B4") +
    geom_segment(data = males,
                aes(x = prey, y = age_years,
                    yend = females$age_years, xend = females$prey),
                color = "#31363b",
                size = .5,
                alpha = .5) + 
    geom_point(aes(x = prey, y = age_years, color = Gender, shape= Gender), size = 5, show.legend = TRUE) +
    geom_text(data = diff,
             aes(label = paste("\u0394 ",diff), x = x_pos, y = age_years),
             color = "#4a4e4d",
             size = 2.7, hjust=0) +
    scale_shape_manual(values = c(18,20)) + 
    scale_color_manual(values = c("#FF69B4","#00BFFF")) + 

    geom_text(x = stats_females$mean+1, y = 1, label = "Avg", angle = 90, size = 3.5, color = "#FF69B4", family = 'Teko') +
    geom_text(x = stats_males$mean+1, y = 1, label = "Avg", angle = 90, size = 3.5, color = "#00BFFF", family = 'Teko') +
    facet_grid(age_years ~ ., scales = "free", switch = "y") + 
	labs(title = "**Cat's prey**",
			subtitle = "<span style = 'color: #00BFFF;'>**Male**</span> and <span style = 'color: #FF69B4;'>**Female**</span> Cat's prey from birth until 12 years old<br>",
	    caption = "**Vitor Pereira** | **Source:** Pets Cat UK",
	    x = 'Total',
	    y = 'Age') +
	theme_minimal() +
	theme(panel.grid.major.y = element_blank(),
	    panel.grid.minor.y = element_blank(),
	    panel.grid.major.x = element_blank(),
	    panel.grid.minor.x = element_blank(),
	    axis.text.y = element_blank(),
	    axis.ticks.y = element_blank(),
	    axis.ticks.x = element_line(color = "#4a4e4d"),
	    text = element_text(family = 'Teko', color = "#4a4e4d"),
	    strip.text.y.left  = element_text(angle = 0),
	    panel.background = element_rect(fill = "azure2", color = "azure2"),
	    strip.background = element_rect(fill = "azure2", color = "azure2"),
	    strip.text = element_text(color = "#4a4e4d", family = 'Teko'),
	    plot.background = element_rect(fill = "azure2", color = "azure2"),
	    panel.spacing = unit(0, "lines"),
	    plot.margin = margin(1,1,.5,1, "cm"),
	    plot.caption = element_markdown(hjust = 1, lineheight = 1.5, family = 'Teko'),
      plot.subtitle = element_markdown(size = 14, hjust = 0.5, family = 'Teko'),
      plot.title = element_markdown(size = 18, hjust = 0.5, family = 'Teko'),
      legend.title = element_text(face = 'bold', family = 'Teko'),
      legend.text = element_text(family = 'Teko', size = 15),
      axis.title.x = element_text(family = 'Teko', size = 15, vjust = -1),
    	axis.title.y = element_text(family = 'Teko', size = 15, vjust = 1),
    	)

ggsave(filename = "cats.png", plot = plot , width = 20, height = 15, units = "cm", dpi = 600)