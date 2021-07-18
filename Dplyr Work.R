library(dplyr)
mtcars %>% 
  group_by(cyl) %>%
  summarise(avg=mean(mpg))

starwars %>%
  rowwise() %>%
  mutate(count=length(films))

mtcars %>% 
  arrange(desc(mpg))

mtcars %>% 
  count(cyl)

mtcars %>% 
  filter(cyl > 4)

mtcars %>% 
  distinct(cyl)

mtcars %>% 
  slice_sample(n=5)

mtcars %>%
  slice_max(mpg,prop = .25)


pull(mtcars, var=1)
select(mtcars,wt,mpg)
relocate(mtcars,mpg,cyl,.after=last_col())

mtcars %>%
  filter(mpg < 15 | mpg > 30)

mtcars %>%
  filter(mpg < 15 & hp > 230)

data <- mtcars %>% 
  slice_max(mpg,prop=.25)


data %>%
  arrange(desc(hp))

mtcars %>% 
    top_n(hp,n=3) %>%
    arrange(desc(hp))
    
mtcars$optimal <- scale(mtcars$wt) + scale(mtcars$hp)
mtcars %>% 
  arrange(desc(optimal))

mtcars %>%
  top_n(mpg,n=3) %>%
  arrange(desc(mpg))


