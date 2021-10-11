#' Q18_Q12b
#' 
#' respondents whose pre-pandemic work was desk based 
#' felt less pressure to be productive, 
#' while ECRs who highly depended on lab- or field-work 
#' to acquire data felt significantly more pressure
#' 
#' @Marcos, do you think it's worth to do a graph showing these 
#' results? I think this comes from analysing Q18 vs 12b 
#' 
# Q12_b	Which location is the most important for performing your job?
# 0	Field
# 1	Laboratory
# 2	Desk or office
# 3	Other

D$Location=NA
D$Location[D$Q12_b==0]="Field"
D$Location[D$Q12_b==1]="Laboratory"
D$Location[D$Q12_b==2]="Desk"
D$Location[D$Q12_b==3]="Other"
# 
# Q18	Compared to before Covid-19 have you felt more or less pressure to be productive?
# Q18_1	Please choose one
# 0	Much more pressure
# 1	A little more pressure
# 2	No change
# 3	A little less pressure
# 4	A lot less pressure

#It is coded in Q18_1_0, Q18_1_1... not useful.

D$Q18_1=NA
D$Q18_1[D$Q18_1_0==1]="Much more"
D$Q18_1[D$Q18_1_1==1]="A little more"
D$Q18_1[D$Q18_1_2==1]="No change"
D$Q18_1[D$Q18_1_3==1]="A little less"
D$Q18_1[D$Q18_1_4==1]="A lot less"
D$Q18_1=as.factor(D$Q18_1)
D$Q18_1=factor(D$Q18_1, levels=c(
  "Much more",
  "A little more",
  "No change",
  "A little less",
  "A lot less"))


D$Q18_v2=NA
D$Q18_v2[D$Q18_1_0==1]=0
D$Q18_v2[D$Q18_1_1==1]=1
D$Q18_v2[D$Q18_1_2==1]=2
D$Q18_v2[D$Q18_1_3==1]=3
D$Q18_v2[D$Q18_1_4==1]=4

D$location=NA
D$location[D$Q12_b==2]="Desk"
D$location[D$Q12_b!=2]="Not-desk"

ggplot(D, aes(y=Q18_v2, x=location, colour=Location))+
  facet_wrap(. ~ stage)+
  geom_boxplot(size=1.4, outlier.shape = 17, outlier.size = 2.5)+
  geom_jitter(size=1, alpha=.6, aes(colour=Location))+
  OYSTERtheme+
  labs(x="Most important location", y="Pressure felt")+
  scale_y_continuous(breaks=c(0,1,2,3,4), labels = c(
    "Much more",
    "A little more",
    "No change",
    "A little less",
    "A lot less"))+
  theme(axis.text.x = element_text(angle=35),
        panel.grid.major.x = element_line(colour="gray", size=1,linetype=2))+
  coord_flip()+
  scale_color_manual(values=OYSTERpalette4)

save_svg=0
if (save_svg) {
  ggsave("Figure3_raw.svg", path = "output", width = 80, height = 50, units = "cm")
}