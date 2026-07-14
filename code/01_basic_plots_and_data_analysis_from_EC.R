## basic scripts for data analysis and figure generation for FASTeDNA field lab manuscript

# loading relevant packages
library(tidyverse)
library(reshape2)
library(cowplot)
library(extrafont)

##########################
### field trial - Evan ###
#########################

# bring in curated data
trialdat <- read.csv("field_trial_data.csv")

# setting the order of x-axis
datorder <- c("STD High",	"STD Low",	"S1 (Lab)",	"S1 (Field)",	"S2 (Lab)",	"S2 (Field)",	"S3 (Lab)",	"S3 (Field)",	"Field Neg (Lab)",	"Field Neg (Field)",	"Extr. Neg (Lab)",	"Extr. Neg (Field)",	"NTC")
trialdat$sample <- factor(trialdat$sample, levels = datorder)

# plotting Ct values and DNA concentration simultaneously to see what this looks like...
ggplot(trialdat, aes(x = sample)) +
  geom_point(aes(y = ct_value, fill = assay_site), shape = 21, size = 3.5, position = position_dodge(0.5)) +
  geom_point(aes(y = dna_conc/2), shape = 8, size = 2) +
  theme(axis.text.x = element_text(angle = 45, hjust =1, size = 8),
        axis.title.y = element_text(margin = margin(r = 10))) +
  scale_y_continuous(sec.axis = sec_axis(~ . * 2, name = expression(paste("DNA Concentration (ng/",  mu, "L)"))  # Label and transformation for secondary y-axis
  )) +
  labs(x = NULL, y = "Ct value", fill = "Assay Site")
# maybe this is a useful way to represent data... maybe not!

#######################################
## implementation data visualization ##
#######################################

# st anns bank (Anarhichas lupus)
sadat <- read.csv("implementation_data_stannsbank.csv") %>% #
  mutate(sample = fct_relevel(sample, "STD High", "STD Low", "Pos. Ctrl", "eDNA 1", "eDNA 2", "Extr. Neg", "NTC"))  # bringing in the data and setting the order fo x values for plotting

sadatplot <- ggplot(sadat, aes(x = sample, y = ct_value, color = protocol)) +
  geom_point(size = 3, position = position_dodge(0.2)) +  # making so you can see both y-values at same x-value
  theme(axis.text.x = element_text(angle = 45, hjust =1, size = 8),    # angling x-axis labels
        axis.title.y = element_text(margin = margin(r = 10)),  # increasing distance from y-axis label to y-axis scale
        legend.position = "none",  # removing the colour key for multiplot
        plot.margin = margin(10, 25, 10, 10, unit = "pt")) + # changing the margins for better multiplot
  labs(x = NULL, y = "Ct value")  # choosing which labels to include


# halifax harbour (sargassum muticum)
hhdat <- read.csv("implementation_data_hharbour.csv") %>%
  mutate(sample = fct_relevel(sample, "STD High", "STD Low", "Pos. Ctrl", "eDNA 1", "eDNA 2", "eDNA Neg", "Extr. Neg", "NTC"))
  
hhdatplot <- ggplot(hhdat, aes(x = sample, y = ct_value, color = protocol)) +
  geom_point(size = 3, position = position_dodge(0.2)) +
  theme(axis.text.x = element_text(angle = 45, hjust =1, size = 8),
        axis.title.y = element_text(margin = margin(r = 10)),
        plot.margin = margin(10, 2, 10, 2, unit = "pt"), 
        legend.position = "none") +
  labs(x = NULL, y = NULL, color = "Protocol")


# hmcs william hall (Semibalanus balanoides)
whdat <- read.csv("implementation_data_williamhall.csv") %>%
  mutate(sample = fct_relevel(sample, "STD High", "STD Low", "Pos. Ctrl", "eDNA 1", "eDNA 2", "eDNA Neg", "Extr. Neg", "NTC"))


whdatplot <- ggplot(whdat, aes(x = sample, y = ct_value, color = protocol)) +
  geom_point(size = 3, position = position_dodge(0.2)) +
  theme(axis.text.x = element_text(angle = 45, hjust =1, size = 8),
        axis.title.y = element_text(margin = margin(r = 10)),
        plot.margin = margin(10, 2, 10, 2, unit = "pt"), 
  legend.position = "none") +
  labs(x = "Sample", y = NULL, color = "Protocol")
  

# three mile lake  (Procambarus clarkii)
tmldat <- read.csv("implementation_data_threemile.csv") %>%
  mutate(sample = fct_relevel(sample, "STD High", "STD Low", "Pos. Ctrl", "eDNA 1", "eDNA 2", "Extr. Neg", "NTC"))

tmldatplot <- ggplot(tmldat, aes(x = sample, y = ct_value, color = protocol)) +
  geom_point(size = 3, position = position_dodge(0.2)) +
  theme(axis.text.x = element_text(angle = 45, hjust =1, size = 8),
        axis.title.y = element_text(margin = margin(r = 10)),
        plot.margin = margin(10, 25, 10, 10, unit = "pt"), 
        legend.position = "none") +
  labs(x = "Sample", y = "Ct value")


# combining all plots
combplot <- plot_grid(sadatplot, hhdatplot, tmldatplot, whdatplot, ncol = 2, nrow = 2, labels = "AUTO", align = "hv")

#extracting a legend for sharing between plots
hhdatplotleg <- ggplot(hhdat, aes(x = sample, y = ct_value, color = protocol)) +
  geom_point(size = 3, position = position_dodge(0.2)) +
  theme(axis.text.x = element_text(angle = 45, hjust =1, size = 8),
        axis.title.y = element_text(margin = margin(r = 10))) +
  labs(x = NULL, y = NULL, color = "Protocol")

legplot <- get_legend(hhdatplotleg)  #

# plotting all together with shared legend
plot_grid(combplot, legplot, ncol = 2, rel_widths = c(1,0.1))



##################################################################
## visualizing assay stability test (prepared assays stored at RT)
# bring in the data for B. schlosseri
bsstabdat <- read.csv("b_schlosseri_assay_stability_data.csv") %>%
  filter(!is.na(Ct))

#plotting Ct values by time point
bsstabplotforleg <- ggplot(bsstabdat, aes(x = time, y = Ct, color = sample)) +
  geom_point(size = 3, position = position_dodge(0.1)) +
  theme(axis.text.x = element_text(angle = 45, hjust =1, size = 8),
        axis.title.y = element_text(margin = margin(r = 5))) +
  scale_y_continuous(limits = c(30, 42), breaks = c(30, 34, 38, 42)) +
  labs(x = NULL, y = "Ct", color = "Sample")

stablegplot <- get_legend(bsstabplotforleg) 

bsstabplot <- ggplot(bsstabdat, aes(x = time, y = Ct, color = sample)) +
  geom_point(size = 3, position = position_dodge(0.1)) +
  theme(axis.text.x = element_text(angle = 45, hjust =1, size = 8),
        axis.title.y = element_text(margin = margin(r = 5)),
        legend.position = "none") +
  scale_y_continuous(limits = c(30, 42), breaks = c(30, 34, 38, 42)) +
  labs(x = NULL, y = "Ct", color = "Sample")
bsstabplot


# now for C. intestinalis
cistabdat <- read.csv("c_intestinalis_assay_stability_data.csv") %>%
  filter(!is.na(Ct))

#plotting Ct values by time point
cistabplot <- ggplot(cistabdat, aes(x = time, y = Ct, color = sample)) +
  geom_point(size = 3, position = position_dodge(0.1)) +
  theme(axis.text.x = element_text(angle = 45, hjust =1, size = 8, margin = margin(r = 8)),
        axis.title.y = element_text(margin = margin(r = 5)),
        legend.position = "none") +
  scale_y_continuous(limits = c(30, 42), breaks = c(30, 34, 38, 42)) +
  labs(x = "Time", y = "Ct", color = "Sample")
cistabplot

# combining plots together
stabcombplot <- plot_grid(bsstabplot, cistabplot, ncol = 1, nrow = 2, labels = c("B. schlosseri", "C. intestinalis"),
                          label_size = 10,
                          vjust = 0.1,
                          hjust = -0.1,
                          align = "hv")
stabcombplot

stabfinplot <- plot_grid(stabcombplot, stablegplot, ncol = 2, rel_widths = c(1,0.2))

stabfinplot + theme(plot.margin = margin(0.5,0.5,0.5,0.5, "cm"))

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
concdat <- read.csv("dna_concentration_summary.csv")
concdat <- concdat[1:48, 1:3]

# plotting values with linkage
dnacompareplot <- ggplot(concdat, aes(location, dna_conc)) +
                  geom_line(aes(group=Test), color="gray20", size=0.5, alpha=0.5) +
                  geom_point(aes(color=Test),size=3.5) +
                  scale_color_viridis_c(option = "viridis") +
                  theme(legend.position = "none") +
                  labs(x = "Extraction Protocol", y = "DNA Concentration")
dnacompareplot

# testing data for normality
sw_test <- shapiro.test(concdat$dna_conc)
print(sw_test)

# Trying paired t-test (for paired continuous data, independent observations, normally distributed)
# preparing the data for tests
dnatestdat <- pivot_wider(concdat, names_from = location, values_from = dna_conc)

t_test <- t.test(dnatestdat$lab, dnatestdat$field, paired = TRUE)
print(t_test)
                    
# Trying Wilcocoxon sign (for paired continuous data, independent observations, not normally distributed)
# have to remove zero values
dnatestdatw <- dnatestdat %>% filter(lab !=0) 

w_test <- wilcox.test(dnatestdatw$lab, dnatestdatw$field, paired = TRUE)
print(w_test)


