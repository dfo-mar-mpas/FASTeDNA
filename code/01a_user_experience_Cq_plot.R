# User experience qPCR Cq comparisons plot


# Load libraries ----------------------------------------------------------
library(dplyr)
library(ggplot2)
library(forcats)

# Load data ---------------------------------------------------------------

user.cq <- read.csv("data/UserExperience_CqValues.csv", header = T) %>% glimpse()

# fill in blanks
user.cq$User.Experience[2:7] <- "No Lab Experience"
user.cq$User.Experience[9:13] <- "Non-Molecular Lab Experience"
user.cq$User.Experience[15:16] <- "Previous Molecular Lab Experience"
user.cq$User.Experience[18:22] <- "Molecular Lab Experts"

#replace NAs with 0 - this won't affect Mean Cq values
user.cq.fixed <- user.cq %>% 
                 mutate(across(where(is.numeric), ~replace_na(.,0)), 
                        Sample = paste0(Type, " ", Sample.ID))

# Make the table long format to make ggplot happy

cq.long <- pivot_longer(user.cq.fixed, cols=contains("Cq"))

cq.long.fix <- cq.long %>% 
  filter(!str_detect(name, "Mean")) %>%
  mutate(Field_Lab = case_when(
    str_starts(name, "Field_") ~ "Field",
    str_starts(name, "Lab_") ~ "Lab")
    )


# Make some plots ---------------------------------------------------------

ggplot(data=cq.long.fix, aes(x = Sample, y=value, fill = Field_Lab))+
  geom_violin()+
  theme_bw() # this looks terrible, not enough data

ggplot(data=cq.long.fix[-27,], aes(x=fct_inorder(Sample), y=value, fill=Field_Lab))+
  stat_summary(fun = mean, geom = "point", size = 4, shape=21, colour = "black") +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2) +
  labs(fill="")+
  ylab(label = "Cq")+
  xlab(label="Sample")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45,hjust = 1, vjust = 1),
        text = element_text(size=14))

ggsave(filename = "UserExperience_Cq_comparison.png",
       plot = last_plot(), 
       path = "figures/",
       device = "png",
       width=10, height=8, dpi = 300)




