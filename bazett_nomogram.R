
#################################################################
##                Load packages and choose font                ##
#################################################################

# Load necessary libraries
library(dplyr)
library(ggplot2)
library(showtext)
library(cowplot)
library(magick)

# Load specific font from Google Fonts
font_add_google("Lato", "google")

# Invoke showtext
showtext_auto()

# Set the basic size for plots
basicsize <- 14

# Set the geom_point size
geompointsize <- 1.8

# Set the plot title size
plottitlesize <- 14

##################################################################
##                     Prepare the data set                     ##
##################################################################


# Load the dataset
data1 <- readRDS("data/manual_bazetts_qtcs.rds")

data1 <- data1 %>% filter(!mlaake.factor == "memantiini")

# Mutate a gender variable
data1 <- data1 %>%
  mutate(
    gender = ifelse(sukupuoli == 1, "Male", "Female") # Create a gender column with descriptive labels
  )

# Remove rows with NA values in relevant columns
data1_clean <- data1 %>%
  filter(!is.na(pre_ake_syketiheys_ekg) & !is.na(median_QT_preAKE) & !is.na(post_ake_syketiheys_ekg) & !is.na(median_QT_postAKE))

# Tidy up the data set
data1_clean <- data1_clean %>% select(pre_ake_syketiheys_ekg,
                                      post_ake_syketiheys_ekg,
                                      median_QT_preAKE,
                                      median_QT_postAKE,
                                      gender)


##################################################################
##    Define manually desired shapes and colors for the plot    ##
##################################################################


# Create a mapping of shapes to levels of risk
shape_mapping <- c("Female" = 24, "Male" = 21)  

# Define colors to levels of risk
custom_outline <- c("Female" = "black", "Male" = "black")

# Define colors to levels of risk
custom_fill <- c("Female" = "black", "Male" = "white")


##################################################################
##            Create plot for before ChEI medication            ##
##################################################################

# Create scatter plot with darker outlines
p_pre <- ggplot() +
  theme_classic(base_size = basicsize, base_family = "google") +
  
  theme(
    legend.position = c(.85, .85)) +
    theme(legend.title=element_blank(),
    legend.background = element_rect(colour = 'black',
                                     fill = 'white',
                                     linetype='solid')) +
  
  theme(plot.margin = margin(0.3, 0.3, 0.3, 0.3, "in")) +
  

  geom_point(size=geompointsize,
             stroke = 0.6,
             data = data1_clean,
             aes(x = pre_ake_syketiheys_ekg,
                 y = median_QT_preAKE,
                 color = gender,
                 fill = gender,
                 shape = gender)) +   
  
  
  stat_function(fun = function(HR) 450 * sqrt(60 / HR), linetype = "solid", color = "black", size = 1) +
  stat_function(fun = function(HR) 460 * sqrt(60 / HR), linetype = "dashed", color = "black", size = 1) +
  labs(
    title = "Before ChEI medication",
    x = "Heart rate (bpm)",
    y = "QT interval (ms)") +

  theme(
    plot.title = element_text(size = plottitlesize)  # Change the font size of the title
  ) +
  
  
  scale_x_continuous(breaks = seq(40, 120, 20),
                   limits=c(40, 120),
                   expand = c(0, 0)) +
  
  scale_y_continuous(breaks = seq(250, 550, 50),
                     limits=c(250, 550),
                     expand = c(0, 0)) +
  
  theme(axis.title.x = element_text(face = "bold",
                                    margin = margin(t = 13, unit = "pt")),
        axis.title.y = element_text(face = "bold",
                                    margin = margin(r = 14, unit = "pt"))) +

  scale_shape_manual(values = shape_mapping,
                     limits = c("Female", "Male")) + # Apply the shape mapping
  scale_color_manual(values = custom_outline,
                     limits = c("Female", "Male")) +  # Manually set outline colors
  scale_fill_manual(values = custom_fill,
                    limits = c("Female", "Male"))  # Manually set fill colors

# Print the plot
p_pre




##################################################################
##            Create plot for after ChEI medication             ##
##################################################################

# Create scatter plot with darker outlines
p_post <- ggplot() +
  theme_classic(base_size = basicsize, base_family = "google") +
  
  theme(
    legend.position = c(.85, .85)) +
  theme(legend.title=element_blank(),
        legend.background = element_rect(colour = 'black',
                                         fill = 'white',
                                         linetype='solid')) +
  
  theme(plot.margin = margin(0.3, 0.3, 0.3, 0.3, "in")) +
  
  
  geom_point(size=geompointsize,
             stroke = 0.6,
             data = data1_clean,
             aes(x = post_ake_syketiheys_ekg,
                 y = median_QT_postAKE,
                 color = gender,
                 fill = gender,
                 shape = gender)) +   
  
  
  stat_function(fun = function(HR) 450 * sqrt(60 / HR), linetype = "solid", color = "black", size = 1) +
  stat_function(fun = function(HR) 460 * sqrt(60 / HR), linetype = "dashed", color = "black", size = 1) +
  labs(
    title = "After ChEI medication",
    x = "Heart rate (bpm)",
    y = "QT interval (ms)") +
  
  theme(
    plot.title = element_text(size = plottitlesize)  # Change the font size of the title
  ) +
  

  scale_x_continuous(breaks = seq(40, 120, 20),
                     limits=c(40, 120),
                     expand = c(0, 0)) +
  
  scale_y_continuous(breaks = seq(250, 550, 50),
                     limits=c(250, 550),
                     expand = c(0, 0)) +
  
  theme(axis.title.x = element_text(face = "bold",
                                    margin = margin(t = 13, unit = "pt")),
        axis.title.y = element_text(face = "bold",
                                    margin = margin(r = 14, unit = "pt"))) +
  
  scale_shape_manual(values = shape_mapping,
                     limits = c("Female", "Male")) + # Apply the shape mapping
  scale_color_manual(values = custom_outline,
                     limits = c("Female", "Male")) +  # Manually set outline colors
  scale_fill_manual(values = custom_fill,
                    limits = c("Female", "Male"))  # Manually set fill colors

# Print the plot
p_post


###########################################################################
###########################################################################
###                                                                     ###
###                  CREATE PANEL AND SAVE GRAPH FILES                  ###
###                                                                     ###
###########################################################################
###########################################################################


# Create a panel

panel <- plot_grid(p_pre, p_post, nrow = 1, ncol = 2, scale = 1.0)

panel


# When showtext() is used to change font(s), plots have to be saved as pdf files,
# otherwise proportions get distorted.


# Save the panel to a PDF file
save_plot("1st_article/bazett_nomogram.pdf",
          panel,
          nrow = 1,
          ncol = 2,
          base_asp = 1.2)

# Load that pdf file with the magick package
pdf_image <- magick::image_read_pdf("1st_article/bazett_nomogram.pdf", density = 600)

# Save it as PNG
image_write(pdf_image,
            path = "1st_article/bazett_nomogram.png",
            format = "png",
            density = 600)
