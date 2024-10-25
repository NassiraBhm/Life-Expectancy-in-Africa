
library(tidyverse)
library(dplyr)
library(gapminder)
library(ggplot2)

# EXPLORE THE DATASET

View(gapminder)
?gapminder
names(gapminder)

#CHECKing FOR DUPLICATES AND MISSING VALUES
gapminder[duplicated(gapminder), ]

gapminder %>% 
  filter(!complete.cases(.))

#SUMMARISING DATA

gapminder %>% 
  summary()

gapminder %>%
  group_by(continent) %>%
  summarise(
    mean_lifeExp = mean(lifeExp),
    mean_gdpPercap = mean(gdpPercap)
  )
 
# Data viz

africa <- gapminder %>% 
  filter(continent == 'Africa')

africa %>% 
  ggplot(aes(x=gdpPercap,
             y=lifeExp,
             color=year)) +
  geom_point()+
  labs(title = 'Life expectency explained by GDP', 
       x = 'GDP per capita', y = 'Life expectency')

# Is there a correlation between life expectancy and GDP per capita in Africa?
# linear modeling:   H0: there is no correlation between life expectency and
#                        GDP per capita in Africa
#                    H1: there is a correlation between life expectency and
#                        GDP per capita in Africa
# p-value threshold: 5%
model <- lm(lifeExp ~ gdpPercap, data = africa)

summary(model)

intercept <- coef(model)[1]
slope <- coef(model)[2]

new_data <- data.frame(gdpPercap = seq(min(africa$gdpPercap), max(africa$gdpPercap), length.out = 100))
predicted_lifeExp <- predict(model, new_data)

# Visualize the model
ggplot(africa, aes(x = gdpPercap, y = lifeExp)) +
  geom_point() +
  geom_abline(intercept = intercept, slope = slope, color = "red") +
  labs(title = "Life Expectancy vs. GDP per Capita (Africa)",
       x = "GDP per Capita",
       y = "Life Expectancy") +
  theme_classic()



# A p-value of < 2.2e-16 suggests  a statistically significant linear 
# relationship between life expectancy and GDP per capita. As GDP per capita 
# increases, life expectancy also tends to increase.


#Is there a correlation between life expectancy and the popolation growth in Africa?

africa %>% 
  ggplot(aes(x=pop,
             y=lifeExp,
             color=year)) +
  geom_point()+
  labs(title = 'Potential correlation between life expectancy and population growth', 
       x = 'Population', y = 'Life expectency') 

# linear modeling:   H0: there is no correlation between life expectency and
#                        population growth in Africa
#                    H1: there is a correlation between life expectency and
#                        population growth in Africa
# p-value threshold: 5%

model2 <- lm(pop ~ lifeExp , data = africa)

summary(model2)
# A p value of 0.002456 suggests a potential correlation between life expectency
# and population growth.
# We can suggest that the progress in life expectancy leads to population growth



# Compare the average life expectancy between Africa and the other continents

summary <-  gapminder %>%
  group_by(continent) %>%
  summarise(
    mean_lifeExp = mean(lifeExp)
    )

ggplot(summary, aes(x = continent, y = mean_lifeExp)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Mean Life Expectancy by Continent",
       x = "Continent",
       y = "Mean Life Expectancy")

# The mean life expectency in Africa appears lowers than the other continents, 
# is this difference statisticlly significant ?

# ANOVA test
# hypothesis testing:   H0: The average life expectency in Africa is the same as 
#                            in the other continents
#                       H1: The average life expectency in Africa than the other 
#                            other continents
# p-value threshold: 5%

gapminder %>%
  aov(lifeExp ~ continent, data = .) %>%
  summary()

# A p value of <2e-16  and f value of 408.7 suggest suggests that the 
# differences in mean life expectancy between the continents are very 
# likely to be statistically significant.
# We reject the null hypthesis and accept the alternative one.

gapminder %>%
  aov(lifeExp ~ continent, data = .) %>%
  TukeyHSD()

#                    diff       lwr       upr     p adj
# Americas-Africa  15.793407 14.022263 17.564550 0.0000000
# Asia-Africa      11.199573  9.579887 12.819259 0.0000000
# Europe-Africa    23.038356 21.369862 24.706850 0.0000000
# Oceania-Africa   25.460878 20.216908 30.704848 0.0000000

# Tukey's HSD tetst confirms that the difference in avaerage life expectancy 
# between Africa and the other continents is statistically significant.


# let's look at the life expectancy by country in Africa

africa %>% 
  group_by(country) %>% 
  summarise(mean_lifeExp = mean(lifeExp)) %>% 
  arrange(desc(mean_lifeExp)) %>% 
  View

# Reunion and Mauritius exhibit the highest average life expectancies in Africa,
# at 66.64 and 64.95 years respectively. Conversely, Sierra Leone and Angola 
# report the lowest average life expectancies on the continent, at 36.77 and 
# 37.88 years. 
  
  
  
  
  
