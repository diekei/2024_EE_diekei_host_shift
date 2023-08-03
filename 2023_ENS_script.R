### INTRODUCTION #####

# Welcome. For the description of the project please visit: https://github.com/diekei/2023_ENS_diekei_host_shift
# Article is available at: 


### LIBRARY #####

library(dplyr)
library(car)
library(ggplot2)
library(plotly)
library(ggtext)


### DATA #####

# data available at 2023_ENS_datavis_host_acceptance.csv
# abbreviations explanation:
# race -> host-race population
# alt -> host acceptance index for alternative host plant (day 2)
# retain -> host acceptance index for re-consuming original host plant (day 3)
hac <- read.csv("2023_ENS_datavis_host_acceptance.csv")
str(hac)

# data available at 2023_ENS_data_area_consumed.csv
# abbreviations explanation
# id -> individual code
# race -> host-race population
# food -> food offered
# test -> day tested
# area -> leaf area consumed in mm^2
lac <- read.csv("2023_ENS_data_area_consumed.csv")
str(lac)
lac <- lac %>% mutate_at(c('race', 'sex', 'food', 'test'), as.factor)

# data available at 2023_ENS_datavis_area_consumed.csv
# abbreviations explanation
# id -> individual code
# race -> host-race population
# food -> food offered
# test -> day tested
# area -> leaf area consumed in mm^2
lac.v <- read.csv("2023_ENS_datavis_area_consumed.csv")
str(lac.v)
lac.v <- lac.v %>% mutate_at(c('race', 'sex', 'food', 'test'), as.factor)


### ANALYSIS - HOST ACCEPTANCE - FISHER EXACT PROBABILITY TEST #####

lhac.fet <- matrix(c(5, 4, 3, 0, 1, 2), 3, 2, 
                   dimnames = list(c("d1", "d2", "d3"), c("accept", "null")))
mhac.fet <- matrix(c(14, 7, 14, 1, 8, 1), 3, 2,
                   dimnames = list(c("d1", "d2", "d3"), c("accept", "null")))

fisher.test(lhac.fet, alternative = "two.sided", conf.int = T, conf.level = 0.95)
fisher.test(mhac.fet, alternative = "two.sided", conf.int = T, conf.level = 0.95)


### ANALYSIS - LEAF AREA CONSUMED - KRUSKAL-WALLIS TEST #####

lac2 <- na.omit(lac)
lac2$group <- paste(lac2$food, "_", lac2$test)
lac2 <- lac2 %>% mutate_at('group', as.factor)

kruskal.test(area ~ group, data = lac2)
pairwise.wilcox.test(lac2$area, lac2$group,
                     p.adjust.method = "BH", exact = FALSE)


### VISUALISATION - HOST ACCEPTANCE #####

hac.labs <- data.frame(label = c("L-race", "M-race"), face = c("italic","italic"))

hac.plot <- ggplot(hac, aes(x = retain, y = alt)) + 
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  geom_vline(xintercept = 0.6, linetype = "dashed", colour = "#FCBC66", size = 1) +
  geom_vline(xintercept = 0.93, linetype = "dashed", colour = "#BAC94A", size = 1) +
  geom_hline(yintercept = -0.2, linetype = "dashed", colour = "#FCBC66", size = 1) +
  geom_hline(yintercept = -0.467, linetype = "dashed", colour = "#BAC94A", size = 1) +
  geom_point(aes(color = race, size = 3), shape = 16) + 
  scale_color_manual(values = c("#FCBC66", "#BAC94A")) + 
  scale_fill_manual(values = c("#FCBC66", "#BAC94A")) + 
  xlab("\nAbility to reutilise original host") + 
  ylab("Ability to use alternative host\n") +
  scale_y_continuous(expand = c(0.02, 0.02), limits = c(-1, 1)) + 
  scale_x_continuous(expand = c(0.05, 0.02), limits = c(0, 1), breaks = seq(0,1, by = 0.5)) +
  theme_bw() + 
  theme(strip.text = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = 'none') +
  geom_text(x =c(0.45, 0.77), 
            y = c(-0.3, -0.57),
            aes(label = label, fontface = face), data = hac.labs, size = 4, 
            color = c("black","black"), parse = TRUE)

hac.plot
ggsave(filename = "Figure_3.png", width = 3.8, height = 5, device='png', dpi=1200)


### VISUALISATION - LEAF AREA CONSUMED #####

lac.labs <- c("L-race (3\u2640 2\u2642)", "M-race (6\u2640 9\u2642)")
names(lac.labs) <- c("lrace", "mrace")

lac.xlabs <- c("d1" = "D1: Ori.", "d2" = "D2: Alt.", "d3" = "D3: Ori.")

lac.plot <- ggplot(lac.v, aes(x = test, y = area, fill = food)) +
  geom_boxplot() +
  geom_point(position = position_jitterdodge(jitter.width = 0)) +
  scale_fill_manual(values = c("#FCBC66", "#BAC94A")) +
  scale_x_discrete(labels = parse(text = lac.xlabs)) +
  xlab("\nHost acceptance test") +
  ylab(bquote(Leaf ~ area ~ consumed ~ (mm^2))) +
  facet_wrap(~race, labeller = labeller(race = lac.labs)) +
  theme_bw() +
  theme(strip.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none")

lac.plot
ggsave(filename = "Figure_4.png", width = 5, height = 5, device='png', dpi=1200)
