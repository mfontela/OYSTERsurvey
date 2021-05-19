# plot theme
#NOTE: preliminar! this is only a quick draft, someone with good taste for graphic design is appreciated 
#set the ggplot theme
OYSTERcolor<-rgb(0,94,157,maxColorValue = 255) #rgb colour from OYSTER logo letters https://www.euromarinenetwork.eu/sites/default/files/header%20images/OYSTER_col_logo_spaced_v2.png

OYSTERtheme<-theme_minimal()+
  theme(text = element_text(colour=OYSTERcolor),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title = element_text(size = rel(1.3)),
        plot.title=element_text(size=rel(1.3), lineheight=.9,
                                face="bold", colour=OYSTERcolor),
        axis.text = element_text(size = rel(1.30)))