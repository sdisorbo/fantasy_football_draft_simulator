library(magick)
library(ggplot)
dev.off()



#plot the strategies out in a density graph to see the distribution of their results (total points from starters each iteration)
ggplot() + 
  geom_density(alpha=.6, fill = "mistyrose", aes(x=(p2$starters)))+
  geom_density(alpha=.6, fill = "white", aes(x=(p8$starters)))+
  geom_density(alpha=.6, fill = "lightsalmon2", aes(x=(p5$starters)))+
  geom_density(alpha=.6, fill = "powderblue", aes(x=(p1$starters)))+
  geom_density(alpha=.6, fill = "white", aes(x=(p3$starters)))+
  geom_density(alpha=.6, fill = "white", aes(x=(p6$starters)))+
  geom_density(alpha=.6, fill = "white", aes(x=(p4$starters)))+
  geom_density(alpha=.6, fill = "forestgreen", aes(x=(p10$starters)))+
  geom_density(alpha=.6, fill = "purple", aes(x=(p9$starters)))+
  
  #geom_density(alpha=.6, fill = "white", aes(x=(p7$starters)))+
 
  
  #xlim(105, 114)+
  labs(
    title = "Tital here",
    #subtitle = "Blue = AI 1 | Brown = AI 4 | Green = AI 6 | Red = AI 9",
    x = "Total Points Per Game (Starters Only)",
    y = "Density"
  )+
  theme_bw()+
  theme(plot.title = element_text(size = 16.5, hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(size = 13, hjust = 0.5),
        axis.text = element_text(size = 11),
        axis.title = element_text(size = 9),
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"),
        text = element_text(color = "black", family = "Font_Name"),
        panel.border = element_blank(),
    )
