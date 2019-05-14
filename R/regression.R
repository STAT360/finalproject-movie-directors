Reduced.List <- Movie_Data %>% 
  count(director_name, na.rm = TRUE) %>% 
  filter(n >= 6) %>%
  slice(-1)

ReducedMovies <- Movie_Data %>%
  semi_join(Reduced.List, by = "director_name")

#Multiple regression model that predicts gross from director name, actor name, 
#budget, duration, content rating, and year released
multiple.mod = lm(gross ~ director_name + budget + duration + actor_1_name + content_rating + imdb_score + title_year, data = ReducedMovies)
(coef(multiple.mod))
anova(multiple.mod)
max(coef(multiple.mod))

coef(multiple.mod)["Movie_Data$title_year"]
coef(multiple.mod)[661:664]

#Creating a model that predicts gross based on director.
gross.mod = lm(gross ~ director_name, data = ReducedMovies)
(coef(gross.mod))
anova(gross.mod)
max(coef(gross.mod))

#Creating a model that predicts IMDb score based on director.
imdb.mod = lm(imdb_score ~ director_name, data = ReducedMovies)
(coef(imdb.mod))
anova(imdb.mod)
max(coef(imdb.mod))

#We only want to look at directors that add at least 54,000,000 dollars to gross.
gross.data <- test %>%
  arrange(desc(estimate)) %>% 
  filter(estimate > 54000000) %>% 
  mutate(term = str_remove(term, "director_name"))

#Plot of coefficients for directors in terms of gross added
ggplot(gross.data) + geom_col(aes(x=reorder(term, estimate), y=estimate, fill = ifelse(p.value < 0.05,'Significant','Insignificant'))) +
  labs(fill = 'p-value') +
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) +
  xlab("Director Name") + ylab("Gross Added") + ggtitle("Best Directors in Terms of Gross Added") +
  scale_y_continuous(breaks = c(0,50000000,100000000,150000000,200000000), labels = scales::comma) +
  coord_flip()
 
test <- tidy(imdb.mod)

#We only want to look at directors that add at least .5 IMDb points.
imdb.data <- imdb.data %>%
  arrange(desc(estimate)) %>% 
  filter(estimate > .5) %>% 
  mutate(term = str_remove(term, "director_name"))

#Plot of coefficients for directors in terms of imdb points added
ggplot(imdb.data) + geom_col(aes(x=reorder(term, estimate), y=estimate, fill = ifelse(p.value < 0.05,'Significant','Insignificant'))) +
  labs(fill = 'p-value') +
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) +
  xlab("Director Name") + ylab("IMDb Points Added") + ggtitle("Best Directors in Terms of IMDb Points Added")+
  coord_flip() 


#Create a regression model that combines

