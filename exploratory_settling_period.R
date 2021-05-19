# FIGURE OF Q14a VS Q22_1
# Ellie: to support the idea of the settling period
# Q14_a	How many weeks were you prevented from accessing your usual place(s) of work?
# 1	1-3 weeks
# 2	3-6 weeks
# 3	6-9 weeks
# 4	9-12 weeks
# 5	12-15 weeks
# 6	15-18 weeks
# 7	18-21 weeks
# 8	21-24 weeks
# 9	24 weeks or more

# Q22	How was communication with your research group affected by Covid-19?
# Q22_1	Please choose one
# 1	Much better
# 2	Somewhat better
# 3	Unchanged
# 4	Somewhat worse
# 5	Much worse
# 6	Not applicable

ggplot(d, aes(Q14_a, Q22_1))+
  geom_density_2d_filled(aes(fill=Q22_1))

ggplot(d, aes(Q14_a))+
  geom_histogram(aes(y = stat(width*density)))+
  scale_y_continuous(labels = percent_format())
  
ggplot(d, aes(Q22_1))+
  # geom_histogram(aes(y = stat(width*density)), fill="black", alpha=.5,binwidth = 1)+
  scale_y_continuous(labels = percent_format())+
  geom_histogram(data=d[d$Q14_a==9,], aes(y = stat(width*density)), fill="red", alpha=.5,binwidth = 1)+
  geom_histogram(data=d[d$Q14_a<9,], aes(y = stat(width*density)), fill="blue", alpha=.5,binwidth = 1)+
  labs(y="%")+
  scale_x_continuous(breaks = c(seq(1,6)), labels=c("Much better",
                                               "Somewhat better",
                                               "Unchanged",
                                               "Somewhat worse",
                                               "Much worse",
                                               "Not applicable"))+
  theme_minimal()+
  theme(axis.text.x = element_text(angle=90))
  
  geom_density(aes(y=..scaled..))+
  geom_density(data=d[d$Q14_a==9,],aes(y=..scaled..), colour="red")

  
  
  
