# User experience qPCR Cq comparisons plot


# Load libraries ----------------------------------------------------------
library(dplyr)
library(ggplot2)
library(forcats)
library(tidyr)
library(stringr)

# Load data ---------------------------------------------------------------

user.cq <- read.csv("data/UserExperience_CqValues.csv", header = T) %>% glimpse()

# fill in blanks
user.cq$User.Experience[2:9] <- "No Lab Experience"
user.cq$User.Experience[10:17] <- "Non-Molecular Lab Experience"
user.cq$User.Experience[18:21] <- "Previous Molecular Lab Experience"
user.cq$User.Experience[22:29] <- "Molecular Lab Experts"

#replace NAs with 0 - this won't affect Mean Cq values
user.cq.fixed <- user.cq %>% 
                 mutate(across(where(is.numeric), ~replace_na(.,0)), 
                        Sample = paste0(Type, " ", Sample.ID))

user.cq.fixed$Sample <- gsub(" AIS","", user.cq.fixed$Sample)
user.cq.fixed$Sample <- gsub("Extraction Negative ","", user.cq.fixed$Sample)


# Make the table long format to make ggplot happy

cq.long <- pivot_longer(user.cq.fixed, cols=contains("Cq"))

cq.long.fix <- cq.long %>% 
  filter(!str_detect(name, "Mean")) %>%
  mutate(Field_Lab = case_when(
    str_starts(name, "Field_") ~ "Field",
    str_starts(name, "Lab_") ~ "Lab")
    )


# Make some plots ---------------------------------------------------------

# ggplot(data=cq.long.fix, aes(x = Sample, y=value, fill = Field_Lab))+
#   geom_violin()+
#   theme_bw() # this looks terrible, not enough data

ggplot(data=cq.long.fix[-c(33,43:45,71:72,149),], 
       aes(x=fct_inorder(Sample), y=value, fill=Field_Lab, shape=factor(Type)))+
  stat_summary(fun = mean, geom = "point", size = 4) +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2) +
  scale_shape_manual(values=c(23,22, 21))+
  scale_fill_manual(values = c("gray","black"))+
  labs(fill="", shape="")+
  ylab(label = "Cq")+
  xlab(label="Sample")+
  guides(fill = guide_legend(override.aes = list(shape = 21, size = 4)))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45,hjust = 1, vjust = 1),
        text = element_text(size=15))

ggsave(filename = "UserExperience_Cq_comparison.pdf",
       plot = last_plot(), 
       path = "figures/",
       device = "pdf",
       width=10, height=8, dpi = 300)




