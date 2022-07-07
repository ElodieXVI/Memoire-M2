#gif
g <- ggplot(class) +
  aes(x = mois, fill = statut) +
  geom_bar( width = 2) +
  scale_x_continuous(breaks =0:217*12, labels = etiquettes) +
  ggtitle("Chronogrammes des différents types de placement") +
  xlab("Âge") + ylab("") +
  theme_light() +
  theme(legend.position = "bottom") +
  labs(fill = "Statut") + 
  scale_fill_manual(values=met.brewer("Hokusai1", 6, direction = 1)) +
  guides(fill = guide_legend(nrow = 2)) +
  theme(legend.position = "top")+ theme(text = element_text(family = "Times"), plot.title = element_text(face = "bold"))

require(gganimate)
require(gifski)
p <- g + 
  transition_states(statut,
                    transition_length = 2,
                    state_length = 1)
animate(p, renderer = gifski_renderer())