# Wonkhe Chart Code -----------------------------------------------------------
# Written by LA for Wonkhe blog 
# https://wonkhe.com/blogs/designing-good-charts-to-communicate-higher-education-data/

# Loading libraries and data --------------------------------------------------

# Loading necessary libraries ----
library(readxl)   # For reading Excel files
library(tidyverse) # For data manipulation and visualization
library(scales)   # For scale functions in ggplot2
library(ggtext)   # For enhanced text in ggplot2
library(extrafont) # For additional font support
library(Cairo)    # For high-quality graphics output

# Loading data ----
data <- read_excel("data_for_wonkhe_chart.xlsx", sheet = "Sheet1")

# Transforming data from wide to long format ----
data <- gather(data, key = "Disability", value = "Value", -Year)

# Basic plot ------------------------------------------------------------------

# Creating basic chart ----
basic_chart <- ggplot(data, aes(x = Year, y = Value, colour = Disability, group = Disability)) +
  geom_line() +
  scale_y_continuous(labels = label_percent()) +
  labs(title = "Percentage of undergraduates at the university with a declared disability",
       caption = "Note: This chart displays synthetic data for demonstration purposes only") +
  theme(legend.position = "top", 
        legend.title = element_blank()) + # Sets the legend at the top
  guides(colour = guide_legend(nrow = 3, byrow = TRUE))  # Adjusts the legend to be two rows

# Saving basic chart to file ----
ggsave("basic_chart.png", basic_chart, width = 180, height = 120, 
       units = "mm", type = "cairo")

# Advanced plot ---------------------------------------------------------------

# Custom theme for advanced plot ----
theme_taso <- function() {
  theme_minimal() +
    theme(
      text = element_text(size = 12, family = "URW DIN"), # Default text size for all text elements
      plot.title.position = "plot", # Aligns plot title to whole plot
      plot.title = element_text(size = 12, face = "bold"),
      plot.subtitle = element_text(size = 12),
      plot.caption.position = "plot", # Aligns caption to the left of the plot
      plot.caption = element_text(hjust = 0, size = 10, face = "italic"),
      panel.grid.major = element_line(colour = "#CECABC", linewidth = 0.3), # Gridline colour
      plot.background = element_rect(fill = "white", color = NA), # Background colour
      plot.margin = margin(0.25, 0.25, 0.25, 0.25, "in"), # Adding margin
      panel.border = element_blank(),
      panel.background = element_rect(fill = "white", color = NA),
      axis.title = element_blank(),
      axis.line.y = element_line(colour = "#6E6E6E", linewidth = 0.5),
      axis.text.x = element_text(margin = margin(t = 7, unit = "pt")), # Increase top margin to move text down
      axis.ticks.length = unit(0.3, "cm"), # Increase the length of ticks
      axis.ticks.x = element_line(colour = "#6E6E6E", linewidth = 0.5) 
    ) 
}

# Custom function to set colours based on Disability ----
chart_colours <- function(disability) {
  ifelse(disability == "Mental health condition", "#3b66bc",
         ifelse(disability == "Social or communication impairments", "#03826B", "#6E6E6E"))
}

# Creating advanced chart ----
advanced_chart <- ggplot(data, aes(x = Year, y = Value, group = Disability)) +
  geom_line(aes(color = chart_colours(Disability), 
                size = Disability == "Mental health condition" | Disability == "Social or communication impairments")) +
  scale_color_identity() +  # Use exact colours provided
  scale_size_manual(values = c("TRUE" = 0.75, "FALSE" = 0.5)) +
  scale_y_continuous(limits = c(0, 0.08), 
                     breaks = seq(0, 0.08, by = 0.02),
                     labels = label_percent(), 
                     expand = c(0,0)) +
  scale_x_discrete(expand = c(0,0)) +
  labs(title = 'The percentage of students with a <span style="color:#3b66bc;">mental health condition</span> has been increasing  \nand the percentage with <span style="color:#03826B;">social or communication impairments</span> has been falling',
       subtitle = "Percentage of undergraduates at the university with a declared disability",
       caption = "\nNote: This chart displays synthetic data for demonstration purposes only") +
  theme_taso() +
  theme(plot.title = ggtext::element_markdown(size = 12),
        legend.position = "none", 
        legend.title = element_blank(),
        axis.line.x = element_line(colour = "#6E6E6E", linewidth = 0.5),
        plot.margin = margin(0.25, 1, 0.25, 0.25, "in"),
        axis.ticks.x = element_line(colour = "#6E6E6E", linewidth = 0.5)) +
  annotate("text",
           x = 5.5, y = 0.02,
           label = "Social or communication\nimpairments",
           size = 3, colour = "#03826B", fontface = "bold",
           family = "URW DIN",
           hjust = 0
  ) +
  annotate("text",
           x = 5.5, y = 0.03,
           label = "Multiple impairments",
           size = 3, colour = "#6E6E6E", fontface = "bold",
           family = "URW DIN",
           hjust = 0
  ) +
  annotate("text",
           x = 5.5, y = 0.045,
           label = "Sensory, medical, or\nphysical impairments",
           size = 3, colour = "#6E6E6E", fontface = "bold",
           family = "URW DIN",
           hjust = 0
  ) +
  annotate("text",
           x = 5.5, y = 0.058,
           label = "Mental health condition",
           size = 3, colour = "#3b66bc", fontface = "bold",
           family = "URW DIN",
           hjust = 0
  ) +
  annotate("text",
           x = 5.5, y = 0.069,
           label = "Cognitive or learning\ndisability",
           size = 3, colour = "#6E6E6E", fontface = "bold",
           family = "URW DIN",
           hjust = 0
  ) +
  coord_cartesian(clip = "off")

# Saving advanced chart to file ----
ggsave("advanced_chart.png", 
       advanced_chart, width = 180, height = 120, units = "mm")
