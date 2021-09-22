#Career stage vs gender
# Louise:
# Need to turn this into a pie chart or something 
# (I wasn't sure of the colour scheme etc so I didn't want to waste time on making 
#   figures before we have sorted that out). 
# Amanda Schadeberg
# My suggestion to the data team is a stacked bar chart 
# so we can show both career stage (bar) and gender (colours within the bar) 
# at the same time – more info for less ink?


# Maria:
# I would separate postdocs from research assistan&researchers. We talk about them separately in the text quite a lot, so I think it can be a bit confusing if we pool them together.
# 
# What about these categories:
# 
# 1. PhD (including PhD with PP)
# 2. Postdoc
# 3. Research assitant+researcher+other ECR

#We need to play with gender and stage (see 03_basic_manipulations for the criteria)
ggplot(D, aes(fill=gender, y=stage)) + 
  geom_bar(position="stack")+
  coord_flip() +
  # geom_bar(aes(fill=gender), alpha=.73, position="stack", stat = "count")+
  OYSTERtheme+
  labs(x="Number of respondents", y="")

D %>% 
  count(stage, gender) %>% 
  mutate(perc = n / nrow(D),
         stage = factor(stage, 
                        levels = c("Research assistant/Researcher/other ECR", "Postdoctoral researcher", "PhD student")))%>%
  ggplot(., aes(y = stage, x = perc, fill=gender)) +
  geom_bar(position="stack", stat = "identity")+
  OYSTERtheme+
  # coord_flip() +
  labs(y="Stage career", x="Percentage (%)", fill="Gender")+
  scale_x_continuous(labels = c("0", "20", "40", "60"))