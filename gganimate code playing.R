library(readr)
talent <- read_csv("C:/Users/KnudseQ/Desktop/Talent Pool.csv")
library(ggplot2)
library(gganimate)
library(hrbrthemes)
library(babynames)
install.packages("viridis")
library(viridis)

# Keep only 3 names
don <- babynames %>% 
  filter(babynames$name %in% c("Ashley", "Patricia", "Helen")) %>%
  filter(babynames$sex=="F")
don<- as.data.frame(don)

# Plot
don %>%
  ggplot( aes(x=don$year, y=n, group=don$name, color=don$name)) +
  geom_line() +
  geom_point() +
  scale_color_viridis(discrete = TRUE) +
  ggtitle("Popularity of American names in the previous 30 years") +
  theme_ipsum() +
  ylab("Number of babies born") +
  transition_reveal(don$year)


p <- ggplot(iris, aes(x = Petal.Width, y = Petal.Length)) +
  geom_point()

plot(p)

anim <- p +
  transition_states(Species,
                    transition_length = 2,
                    state_length = 1)

anim


anim +
  ease_aes('cubic-in-out') # Slow start and end for a smoother look



anim +
  ease_aes(y = 'bounce-out') # Sets special ease for y aesthetic


anim +
  ggtitle('Now showing {closest_state}',
          subtitle = 'Frame {frame} of {nframes}')



ggplot(iris, aes(x = Petal.Width, y = Petal.Length)) +
  geom_point(aes(colour = Species, group = 1L)) +
  transition_states(Species,
                    transition_length = 2,
                    state_length = 1)



anim <- ggplot(iris, aes(x = Petal.Width, y = Petal.Length)) +
  geom_point(aes(colour = Species), size = 2) +
  transition_states(Species,
                    transition_length = 2,
                    state_length = 1)

anim +
  enter_fade() +
  exit_shrink()


#https://www.datanovia.com/en/blog/gganimate-how-to-create-plots-with-beautiful-animation-in-r/
library(gapminder)
head(gapminder)
theme_set(theme_bw())
p <- ggplot(
  gapminder, 
  aes(x = gdpPercap, y=lifeExp, size = pop, colour = country)
) +
  geom_point(show.legend = FALSE, alpha = 0.7) +
  scale_color_viridis_d() +
  scale_size(range = c(2, 12)) +
  scale_x_log10() +
  labs(x = "GDP per capita", y = "Life expectancy")
p

p + transition_time(year) +
  labs(title = "Year: {frame_time}")


p + transition_time(year) +
  labs(title = "Year: {frame_time}") +
  view_follow(fixed_y = TRUE)

p + transition_time(year) +
  labs(title = "Year: {frame_time}") +
  shadow_wake(wake_length = 0.1, alpha = FALSE)


p + transition_time(year) +
  labs(title = "Year: {frame_time}") +
  shadow_mark(alpha = 0.3, size = 0.5)


p <- ggplot(
  airquality,
  aes(Day, Temp, group = Month, color = factor(Month))
) +
  geom_line() +
  scale_color_viridis_d() +
  labs(x = "Day of Month", y = "Temperature") +
  theme(legend.position = "top")
p

p + transition_reveal(Day)

p + 
  geom_point() +
  transition_reveal(Day)

p + 
  geom_point(aes(group = seq_along(Day))) +
  transition_reveal(Day)


library(dplyr)
mean.temp <- airquality %>%
  group_by(Month) %>%
  summarise(Temp = mean(Temp))
mean.temp


p <- ggplot(mean.temp, aes(Month, Temp, fill = Temp)) +
  geom_col() +
  scale_fill_distiller(palette = "Reds", direction = 1) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    panel.grid.major.y = element_line(color = "white"),
    panel.ontop = TRUE
  )
p

p + transition_states(Month, wrap = FALSE) +
  shadow_mark()


p + transition_states(Month, wrap = FALSE) +
  shadow_mark() +
  enter_grow() +
  enter_fade()



#Save animation
#If you need to save the animation for later use you can use the anim_save() function.

#It works much like ggsave() from ggplot2 and automatically grabs the last rendered animation if you do not specify one directly.