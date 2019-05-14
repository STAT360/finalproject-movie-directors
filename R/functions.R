Director.Gross <- Movie_Data %>% 
  group_by(director_name) %>% 
  summarise(Mean.Gross = sum(gross, na.rm = TRUE), 
            Mean.Budget = sum(budget, na.rm = TRUE)) %>%
  mutate(Net = Mean.Gross - Mean.Budget) %>% 
  arrange(desc(Net))

Director.Gross <- Movie_Data %>% 
  group_by(director_name) %>% 
  summarise(Mean.Gross = mean(gross, na.rm = TRUE), 
            Mean.Budget = mean(budget, na.rm = TRUE)) %>%
  mutate(Net = Mean.Gross - Mean.Budget) %>% 
  arrange(desc(Net))


ggplot(PG13movies, aes(x = duration, y = gross, colour = imdb_score)) + 
  geom_point()



gross.predictor <- function(director_name, budget, duration, actor_1_name, content_rating, imdb_score, title_year, data = regression.table) {

budget.coef <-  (regression.table %>% filter(term == budget) %>% select("estimate"))
duration.coef <-  (regression.table %>% filter(term == duration) %>% select("estimate"))
director.coef <-  (regression.table %>% filter(term == director_name) %>% select("estimate"))
actor.coef <-  (regression.table %>% filter(term == actor_1_name) %>% select("estimate"))
rating.coef <-  (regression.table %>% filter(term == content_rating) %>% select("estimate"))
imdb.coef <-  (regression.table %>% filter(term == imdb_score) %>% select("estimate"))
year.coef <-  (regression.table %>% filter(term == title_year) %>% select("estimate"))


  dollars <- (1938713878.08 + (director.coef) + (actor.coef) + (budget*budget.coef) + (duration*duration.coef) + (content_rating*rating.coef) + (imdb_score*imdb.coef) + (title_year*year.coef))
    
  return(dollars)
}

gross.predictor("Steven Spielberg", 50000000, 125, "Tom Cruise", "PG-13", 7.8, 2019)
  