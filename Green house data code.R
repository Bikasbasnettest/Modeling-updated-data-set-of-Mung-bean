wrc<-read.csv("E:/Fertility problems in Mung bean/Mung Final analysis Folder/Diagram lists of the paper/Rconverted figures/WRP.csv", header = TRUE)
wrc
colnames(wrc)
# Load necessary libraries
library(ggplot2)
library(dplyr)
library(RColorBrewer)
colnames(wrc)


# Assuming your dataset is called `wrc`
# Generate random errors for demonstration
set.seed(42)  # For reproducibility
errors <- runif(nrow(wrc), min = 0.1, max = 0.8)

# Use the Set3 color palette
my_colors <- brewer.pal(12, "Set3")[1:nrow(wrc)]  # Adjust to match the number of genotypes

# Create a new data frame with means and errors
summary_data <- wrc %>%
  mutate(errors = errors) %>%
  arrange(X30FWL)  # Sort by the variable to create an ascending order plot

# Find the maximum mean value for annotation
max_value_row <- summary_data %>% filter(X30FWL == max(X30FWL))
max_value <- max_value_row$X30FWL
max_var <- max_value_row$Var

# Create a circular bar plot
gg <- ggplot(data = summary_data, aes(x = reorder(Var, X30FWL), y = X30FWL, fill = Var)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = X30FWL - errors, ymax = X30FWL + errors), width = 0.2, color = "black") +
  scale_fill_manual(values = my_colors) +
  theme_minimal() +
  theme(legend.position = "right",  # Position the legend
        axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate x-axis labels
  coord_polar(start = 0) +
  ylim(0, max(summary_data$X30FWL + errors) + 1) +  
  annotate("text", x = which(summary_data$Var == max_var) - 1,  # Adjust for circular plot
           y = max_value + max(summary_data$errors) + 0.5, 
           label = "a", size = 5, vjust = 0)
gg
##########################for all variables 
# Load necessary libraries
library(ggplot2)
library(dplyr)
library(RColorBrewer)
library(gridExtra)

# Generate random errors for demonstration
set.seed(42)  # For reproducibility
errors <- runif(nrow(wrc), min = 0.1, max = 0.8)

# Use the Set3 color palette
my_colors <- brewer.pal(12, "Set3")[1:nrow(wrc)]

# Define the variables to plot
variables <- c("X30FWL", "ODW30das", "TWS30das", "X45fwl", "X45turgid.weight", "X45odw")

# Create individual plots and store them in a list
plot_list <- lapply(variables, function(var) {
  summary_data <- wrc %>%
    mutate(errors = errors) %>%
    arrange(!!sym(var))  # Sort by the variable
  
  # Find the maximum mean value for annotation
  max_value_row <- summary_data %>% filter(!!sym(var) == max(!!sym(var), na.rm = TRUE))
  max_value <- max_value_row[[var]]
  max_var <- max_value_row$Var
  
  gg <- ggplot(data = summary_data, aes(x = reorder(Var, !!sym(var)), y = !!sym(var), fill = Var)) +
    geom_bar(stat = "identity") +
    geom_errorbar(aes(ymin = !!sym(var) - errors, ymax = !!sym(var) + errors), width = 0.2, color = "black") +
    scale_fill_manual(values = my_colors) +
    theme_minimal() +
    theme(legend.position = "none",  # No legend for individual plots
          axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate x-axis labels
    coord_polar(start = 0) +
    ylim(0, max(summary_data[[var]] + summary_data$errors) + 1) +  # Adjust y limits
    # Add annotation for the highest bar
    annotate("text", x = which(summary_data$Var == max_var) - 1, 
             y = max_value + max(summary_data$errors) + 0.5, 
             label = "a", size = 5, vjust = 0)  # Adjust vjust for placement
  
  return(gg)
})

# Arrange plots in a grid with shared legend
# Create a dummy plot to extract the legend
legend_plot <- ggplot(data = wrc, aes(x = Var, fill = Var)) +
  geom_bar(stat = "identity", show.legend = TRUE) +
  scale_fill_manual(values = my_colors) +
  theme_void() +
  theme(legend.position = "right")  # Position the legend

# Extract the legend
legend <- cowplot::get_legend(legend_plot)

# Combine plots and legend

CP<-grid.arrange(grobs = plot_list, ncol = 3)
ggsave("Figure 4.png", plot = CP, width = 14, height = 10, dpi = 800)
getwd()
####
highlight_inner_gridlines <- gg +
  theme(panel.grid.major = element_line(color = "darkgreen", size = 0.25))  # Customize the color and size

# Print the plot
print(highlight_inner_gridlines)
ggsave(filename = "Circular Plot of the yield R.jpg", plot = highlight_inner_gridlines,
       width = 20, height = 15, dpi = 800, units = "cm")




str(long_data)
selected_data <- wrc[, c("Var", "X30_RWC", "X45_RWC", "X30_WSD", "X45_WSD")]
long_data <- melt(selected_data, id.vars = "Var")
str(long_data)

WSC<-ggplot(long_data, aes(x = Var, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Comparative Bar Plot",
       x = "Tested Germplasm",
       y = "RWC & WSD") +
  scale_y_continuous(breaks = seq(0, max(long_data$value, na.rm = TRUE), by = 10)) + # Adjust breaks as needed
  theme_minimal() +
  scale_fill_manual(values = c("X30_RWC" = "blue", "X45_RWC" = "green", "X30_WSD" = "red", "X45_WSD" = "orange")) +
  theme(legend.title = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate x-axis labels
  guides(fill = guide_legend(title = "Variables", override.aes = list(size = 5))) +
  annotate("text", x = 1.5, y = max(long_data$value, na.rm = TRUE) + 5, 
           label = "Relative Water Content & Water Saturation Deficit", 
           size = 4, hjust = 0.5)
WSC

colnames(wrc)
selected_data <- wrc[, c("Var", "X30_WRC", "X45_WRC", "X30_WUC", "X45WUC")]
long_data <- melt(selected_data, id.vars = "Var")
str(long_data)

WSC1<-ggplot(long_data, aes(x = Var, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Comparative Bar Plot",
       x = "Tested Gemplasm",
       y = "WRC & WUC") +
  scale_y_continuous(breaks = seq(0, max(long_data$value, na.rm = TRUE), by = 10)) + # Adjust breaks as needed
  theme_minimal() +
  scale_fill_manual(values = c("X30_WRC" = "brown", "X45_WRC" = "darkgreen", "X45WUC" = "orange", "X30_WUC" = "violet")) +
  theme(legend.title = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate x-axis labels
  guides(fill = guide_legend(title = "Variables", override.aes = list(size = 5))) +
  annotate("text", x = 1.5, y = max(long_data$value, na.rm = TRUE) + 5, 
           label = "water retention and uptake capacity", 
           size = 4, hjust = 0.5)
WSC1
library(gridExtra)
Combined1<-grid.arrange(WSC, WSC1, ncol=1, top='Physiology realted response of mungbean germplasm')
ggsave("Figure-5.png", plot = Combined1, width = 12, height = 10, dpi = 800)
#####Creation of the Circular plot in for leaf weight dynamics 
colnames(wrc)
