Director.Gross <- Movie_Data %>% 
  group_by(director_name) %>% 
  summarise(Mean.Gross = mean(gross, na.rm = TRUE), 
            Mean.Budget = mean(budget, na.rm = TRUE)) %>%
  mutate(Net = Mean.Gross - Mean.Budget) %>% 
  arrange(desc(Net))


ggplot(PG13movies, aes(x = duration, y = gross, colour = imdb_score)) + 
  geom_point()



Director.Average.Gross <- function(director_name, gross) {
  
}