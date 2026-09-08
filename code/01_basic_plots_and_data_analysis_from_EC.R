## basic scripts for data analysis and figure generation for FASTeDNA field lab manuscript
### updated by Nick July 17 2026
# loading relevant packages
library(tidyverse)
library(reshape2)
library(cowplot)
library(extrafont)
library(ggplot2)
library(dplyr)
library(forcats)
library(patchwork)
library(rphylopic)

##########################
### field trial - Evan ###
#########################

# bring in curated data
trialdat <- read.csv("data/field_trial_data.csv")

# setting the order of x-axis
datorder <- c("STD High",	"STD Low",	"S1 (Lab)",	"S1 (Field)",	"S2 (Lab)",	"S2 (Field)",	"S3 (Lab)",	"S3 (Field)",	"Field Neg (Lab)",	"Field Neg (Field)",	"Extr. Neg (Lab)",	"Extr. Neg (Field)",	"NTC")
trialdat$sample <- factor(trialdat$sample, levels = datorder)

# plotting Ct values and DNA concentration simultaneously to see what this looks like...
ggplot(trialdat, aes(x = sample)) +
  geom_point(aes(y = ct_value, fill = assay_site), shape = 21, size = 3.5, 
             position = position_dodge(0.5)) +
  geom_point(aes(y = dna_conc/2), shape = 8, size = 2) +
  scale_y_continuous(sec.axis = sec_axis(~ . * 2, name = expression(paste("DNA Concentration (ng/",  mu, "L)"))  # Label and transformation for secondary y-axis
  )) +
  labs(x = NULL, y = "Ct value", fill = "Assay Site")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust =1, size = 8),
        axis.title.y = element_text(margin = margin(r = 10)))
  
# maybe this is a useful way to represent data... maybe not!

#######################################
## implementation data visualization ##
#######################################

# st anns bank (Anarhichas lupus)
sadat <- read.csv("data/implementation_data_stannsbank.csv") %>% #
  mutate(sample = fct_relevel(sample, "STD High", "STD Low", "Pos. Ctrl", "eDNA 1", "eDNA 2", "Extr. Neg", "NTC"))  # bringing in the data and setting the order fo x values for plotting

sadatplot <- ggplot(sadat, aes(x = sample, y = ct_value, fill = protocol)) +
  geom_point(size = 3, shape=21, position = position_dodge(0.2), colour="black") +
  add_phylopic(name = "Anarhichas denticulatus", x=6.7, y=36, height = 3.8)+
  theme_bw()+ # making so you can see both y-values at same x-value
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        #axis.text.x = element_text(angle = 45, hjust =1, size = 10),    # angling x-axis labels
        axis.title.y = element_text(margin = margin(r = 8), size=14),  # increasing distance from y-axis label to y-axis scale
        legend.position = "none",  # removing the colour key for multiplot
        plot.margin = margin(10, 15, 10, 10, unit = "pt")) + # changing the margins for better multiplot
  labs(x = NULL, y = "Cq", fill = "Protocol")  # choosing which labels to include

sadatplot

# halifax harbour (sargassum muticum)
hhdat <- read.csv("data/implementation_data_hharbour.csv") %>%
  mutate(sample = fct_relevel(sample, "STD High", "STD Low", "Pos. Ctrl", "eDNA 1", "eDNA 2", "eDNA Neg", "Extr. Neg", "NTC"))
  
hhdatplot <- ggplot(hhdat, aes(x = sample, y = ct_value, fill = protocol)) +
  geom_point(size = 3, position = position_dodge(0.2), shape=21, colour="black") +
  add_phylopic(name="Fucus serratus", x=7.7, y=30, height=10)+
  theme_bw()+
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        #axis.text.x = element_text(angle = 45, hjust =1, size = 10),
        axis.title.y = element_text(margin = margin(r = 10)),
        plot.margin = margin(10, 15, 10, 2, unit = "pt"), 
        legend.position = "none") +
  labs(x = NULL, y = NULL, fill = "Protocol");hhdatplot


# hmcs william hall (Semibalanus balanoides)
whdat <- read.csv("data/implementation_data_williamhall.csv") %>%
  mutate(sample = fct_relevel(sample, "STD High", "STD Low", "Pos. Ctrl", "eDNA 1", "eDNA 2", "eDNA Neg", "Extr. Neg", "NTC"))


whdatplot <- ggplot(whdat, aes(x = sample, y = ct_value, fill = protocol)) +
  geom_point(size = 3, position = position_dodge(0.2), shape = 21, colour="black") +
  add_phylopic(name="Balanus", x=7.7, y=35, height = 7.5)+
    theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust =1, size = 12),
        axis.title.x = element_text(size=14),
        axis.title.y = element_text(margin = margin(r = 10), size=14),
        plot.margin = margin(10, 15, 10, 2, unit = "pt"), 
  legend.position = "none") +
  labs(x = "Sample", y = NULL, fill = "Protocol");whdatplot
  

# three mile lake  (Procambarus clarkii)
tmldat <- read.csv("data/implementation_data_threemile.csv") %>%
  mutate(sample = fct_relevel(sample, "STD High", "STD Low", "Pos. Ctrl", "eDNA 1", "eDNA 2", "Extr. Neg", "NTC"))

tmldatplot <- ggplot(tmldat, aes(x = sample, y = ct_value, fill = protocol)) +
  geom_point(size = 3, position = position_dodge(0.2), shape=21, colour= "black") +
  add_phylopic(name="Procambarus clarkii", x=6.7, y=38,horizontal = T, angle = 270, height = 6.6)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust =1, size = 12),
        axis.title.x = element_text(size=14),
        axis.title.y = element_text(margin = margin(r = 8), size=14),
        plot.margin = margin(10, 15, 10, 10, unit = "pt"), 
        legend.position = "none") +
  labs(x = "Sample", y = "Cq", fill = "Protocol");tmldatplot


# st catharines river - Keji (Hemigrapsus sanguineus)
scrdat <- read.csv("data/implementation_data_st_catherines_river.csv") %>%
  mutate(sample = fct_relevel(sample, "STD High", "STD Low", "Pos. Ctrl", "eDNA 1", "eDNA 2", "eDNA Neg", "Extr. Neg", "NTC"))

scrdatplot <- ggplot(scrdat, aes(x = sample, y = ct_value, fill = protocol)) +
  geom_point(size = 3, position = position_dodge(0.2), shape = 21, colour="black") +
  add_phylopic(name="Hemigrapsus takanoi", x=7.7, y=32.5, height = 6.1)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust =1, size = 12),
        axis.title.x = element_text(size=14),
        axis.title.y = element_text(margin = margin(r = 10), size=14),
        plot.margin = margin(10, 4, 10, 2, unit = "pt"), 
        legend.position = "none") +
  labs(x = "Sample", y = NULL, fill = "Protocol");scrdatplot


# little port joli - Keji (Hemigrapsus sanguineus)
lpjdat <- read.csv("data/implementation_data_little_port_joli.csv") %>%
  mutate(sample = fct_relevel(sample, "STD High", "STD Low", "Pos. Ctrl", "eDNA 1", "eDNA 2", "eDNA Neg", "Extr. Neg", "NTC"))

lpjdatplot <- ggplot(lpjdat, aes(x = sample, y = ct_value, fill = protocol)) +
  geom_point(size = 3, position = position_dodge(0.2), shape=21, colour="black") +
  add_phylopic(name="Hemigrapsus takanoi", x=7.7, y=32.5, height=6.1)+
  theme_bw()+
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_text(margin = margin(r = 10)),
        plot.margin = margin(10, 4, 10, 2, unit = "pt"), 
        legend.position = "none") +
  labs(x = NULL, y = NULL, fill = "Protocol");lpjdatplot


# combining plots
(sadatplot + hhdatplot + lpjdatplot) / (tmldatplot + whdatplot + scrdatplot) +
  plot_annotation(tag_levels = "A")+
  plot_layout(guides = "collect") & #learned here that the & symbol means the theme line below applies legend to the whole grid rather than just the last plot in the grid!
  theme(legend.position = "bottom",
        legend.text = element_text(size=14),
        legend.title = element_text(size=14))

ggsave(filename = "6panel_comboPlot.png", 
       plot = last_plot(), 
       device = "png", 
       path = "./figures/",
       width = 14, 
       height =10, 
       dpi = 400)

##################################################################
## visualizing assay stability test (prepared assays stored at RT)
# bring in the data for B. schlosseri
bsstabdat <- read.csv("data/b_schlosseri_assay_stability_data.csv") %>%
  filter(!is.na(Ct))

#plotting Ct values by time point
bsstabplotforleg <- ggplot(bsstabdat, aes(x = time, y = Ct, fill = sample)) +
  geom_point(size = 3, 
             #position = position_dodge(0.1), 
             shape=21,
             colour = "black") +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust =1, size = 10),
        axis.title.y = element_text(margin = margin(r = 5))) +
  scale_y_continuous(limits = c(30, 42), breaks = c(30, 34, 38, 42)) +
  labs(x = NULL, y = "Cq", color = "Sample")

stablegplot <- get_legend(bsstabplotforleg) 

bsstabplot <- ggplot(bsstabdat, aes(x = time, y = Ct, fill = sample)) +
  geom_point(size = 3,
             #position = position_dodge(0.1), 
             shape=21, 
             colour="black") +
  scale_y_continuous(limits = c(30, 42), breaks = c(30, 34, 38, 42)) +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust =1),
        axis.title.y = element_text(margin = margin(r = 5)),
        text = element_text(size = 16),
        legend.position = "right") +
  labs(x = NULL, y = "Cq", fill = "Sample")
bsstabplot


# now for C. intestinalis
cistabdat <- read.csv("data/c_intestinalis_assay_stability_data.csv") %>%
  filter(!is.na(Ct))

#plotting Ct values by time point
cistabplot <- ggplot(cistabdat, aes(x = time, y = Ct, fill = sample)) +
  geom_point(size = 3, 
             #position = position_dodge(0.1), 
             shape=21, 
             colour="black") +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust =1, margin = margin(r = 8)),
        axis.title.y = element_text(margin = margin(r = 5)),
        text=element_text(size=16),
        legend.position = "none") +
  scale_y_continuous(limits = c(30, 42), breaks = c(30, 34, 38, 42)) +
  labs(x = "Time", y = "Cq", fill = "Sample")
cistabplot

# combining plots together
bsstabplot/cistabplot + 
  plot_annotation(tag_levels = "A")+
  plot_layout(guides = "collect") & #learned here that the & symbol means the theme line below applies legend to the whole grid rather than just the last plot in the grid!
  theme(legend.position = "bottom")

ggsave(filename = "stability_test_2panel.png", 
       plot = last_plot(), 
       device = "png", 
       path = "./figures/", 
       width = 10, 
       height =8, 
       dpi = 400)


## test statistics for Ct values between time points
# trying Kruskal-Wallis test to test averages between values between the three groups (time), not assuming data normality
# for B. schlosseri data
bskruskal_test <- kruskal.test(
  Ct ~ time, 
  data = bsstabdat)
print(bskruskal_test)  # no significant difference, which is expected

# for C. intestinalis
cikruskal_test <- kruskal.test(
  Ct ~ time, 
  data = cistabdat)
print(cikruskal_test)  # no significant difference, which is expected



###############################################
## Comparing field and lab DNA concentration

# bringing in dataset
concdat <- read.csv("data/dna_concentration_summary.csv")
concdat <- concdat[1:72, 1:3]

# testing data for normality
sw_test <- shapiro.test(concdat$dna_conc)
print(sw_test)  # data is not normal according to Shapiro-Wilk

# Trying Wilcocoxon sign (for paired continuous data, independent observations, not normally distributed)
# preparing the data for test
dnatestdat <- pivot_wider(concdat, names_from = location, values_from = dna_conc)
# removing zero values required by test
dnatestdatw <- dnatestdat %>% filter(Laboratory !=0) 
# applying test
w_test <- wilcox.test(dnatestdatw$Laboratory, dnatestdatw$FASTeDNA, paired = TRUE)
print(w_test)  # no statistically significant difference in DNA concentration between lab and field protocol

# plotting values with linkage
dnacompareplot <- ggplot(concdat, aes(location, dna_conc)) +
  geom_line(aes(group=Test), color="gray20", linewidth =0.5, alpha=0.5) +
  geom_point(aes(color=Test), size =3.5) +
  scale_color_viridis_c(option = "viridis") +
  theme_bw() + 
  theme(legend.position = "none") +
  annotate("text", x = 2.3, y = 120, label = "p = 0.111") +
  labs(x = "Extraction Protocol", y = expression(paste("DNA Concentration")))
dnacompareplot

# plotting again but with zeros removed
# first removing zeros
concdatnozero <- concdat %>% filter(dna_conc !=0)

dnacompareplotnozero <- ggplot(concdatnozero, aes(location, dna_conc)) +
  geom_line(aes(group=Test), color="gray20", linewidth =0.5, alpha=0.5) +
  geom_point(aes(color=Test), size =3.5) +
  scale_color_viridis_c(option = "viridis") +
  theme_bw() + 
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust =1, margin = margin(r = 8))) +
  annotate("text", x = 2.3, y = 120, label = "p = 0.111") +
  labs(x = "Extraction Protocol", y = expression(paste("DNA Concentration")))
dnacompareplotnozero

ggsave(filename = "dna_concentration_plot.png", 
       plot = dnacompareplot, 
       device = "png", 
       path = "./figures/", 
       width = 8, 
       height =8, 
       dpi = 300)


### doing the same Wilcoxon test and visualization for paired eDNA samples Cq values across all equal tests ###
# bringing in the Cq data 
cqdat <- read.csv("data/cq_value_summary.csv")

# data with triplicate Cq values from samples
cqdat1 <- cqdat[, 1:5]
# data with replicate Cq values averaged per sample
cqdatavg <- cqdat[1:32,7:11]

# testing data for normality
sw_test <- shapiro.test(cqdatavg$cq_value)
print(sw_test)  # data is not normal according to Shapiro-Wilk

# Applying Wilcocoxon sign-ranked test to averaged Cq value dataset
cqtestdat <- pivot_wider(cqdatavg, names_from = protocol2, values_from = cq_value_avg) # altering dataframe for test
cqtestdat <- cqtestdat %>% filter(Laboratory !=0) # removing zeros as required by WSR test

cq_w_test <- wilcox.test(cqtestdat$Laboratory, cqtestdat$FASTeDNA, paired = TRUE)
print(cq_w_test)  # no statistically significant difference in DNA concentration between lab and field protocol

# plotting values with linkage
cqcompareplot <- ggplot(cqdat, aes(protocol, cq_value)) +
  geom_line(aes(group=test), color="gray20", linewidth =0.5, alpha=0.5) +
  geom_point(aes(color=test), size =3.5, position = position_dodge(0.1)) +
  scale_color_viridis_c(option = "plasma") +
  theme_bw() + 
  theme(legend.position = "none") +
  annotate("text", x = 2.3, y = 45, label = "p = 0.174") +
  labs(x = "Extraction Protocol", y = expression(paste("Cq")))
cqcompareplot

# trying again but with zeros removed and average Cqs per test
cqdatavgnozero <- cqdatavg %>% filter(cq_value_avg !=0)

cqcompareplotnozero <- ggplot(cqdatavgnozero, aes(protocol2, cq_value_avg)) +
  geom_line(aes(group=test2), color="gray20", linewidth =0.5, alpha=0.5) +
  geom_point(aes(color=test2), size =3.5) +
  scale_color_viridis_c(option = "plasma") +
  theme_bw() + 
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust =1, margin = margin(r = 8))) +
  annotate("text", x = 2.3, y = 40, label = "p = 0.350") +
  scale_y_continuous(limits = c(25,40), breaks = c(25,28,31,34,37,40)) +
  labs(x = "Complete Protocol", y = expression(paste("Cq")))
cqcompareplotnozero   # this looks better although many fewer data points


# combining DNA concentation and Cq paired-data plots
dnacompareplotnozero + cqcompareplotnozero + 
  plot_annotation(tag_levels = "A")
# exporting
ggsave(filename = "paired_data_plot.png", 
       plot = last_plot(), 
       device = "png", 
       path = "./figures/", 
       width = 10, 
       height = 7, 
       dpi = 400)


# Field trial data --------------------------------------------------------

fieldat <- read.csv("data/field_trial_data.csv", header = T) %>% glimpse()

# try a plot with points and lines connecting the groups

ggplot(data=fieldat, aes(x=assay_site, y=ct_value, fill=sample))+
  geom_point(shape=21, colour="black", size=3.5)+
  geom_line(aes(group=sample), color="gray20", size=0.5, alpha=0.5) +
  labs(x="Assay Site", y="Ct")+
  theme_bw()
