
#4.3.3 GDP_POP represented in two graphs ----

ggplot(gdp_pop, aes(x = Date)) +
  geom_bar(aes(y = GDP, fill = Country), stat = "identity", alpha = 0.5) +
  labs(title = "GDP Over Years",
       x = "Year",
       y = "Values",
       color = "Country",
       fill = "Country") +
  theme_minimal() +
  facet_wrap(~ Country, scales = "free_y")

ggplot(gdp_pop, aes(x = Date)) +
  geom_bar(aes(y = Population, fill = Country), stat = "identity", alpha = 0.5) +
  labs(title = "Population Over Years",
       x = "Year",
       y = "Values",
       color = "Country",
       fill = "Country") +
  theme_minimal() +
  facet_wrap(~ Country, scales = "free_y")
