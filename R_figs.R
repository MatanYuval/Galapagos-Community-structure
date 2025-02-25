library(dplyr)
library(tidyr)
library(vegan)
install.packages("ggfortify")
install.packages("ggvegan")
library(ggfortify)
library(ggvegan)
library(ggrepel)
# Load the CSV file into a data frame
df <- read.csv("parsed_annotations21_2.csv")


summary(df)
# Display the first few rows of the data frame
head(df)
# Count occurrences of each classification per project_id and site
summary_table <- df %>%
  count(project_id, classification) %>%
  pivot_wider(names_from = classification, values_from = n, values_fill = list(n = 0))
site_names <- c("Darwin", "Espanola", "Fl_tres_Cuevas", "Fl_cormorant_pt", 
                "Marchena", "Pinta", "Wolf")

# Add the 'site_name' column to your summary table
summary_table <- summary_table %>%
  mutate(site_name = site_names)
summary_table <- summary_table %>%
  mutate(project_id = site_name) %>%
    select( -site_name)
summary_table <- summary_table %>%
  rename(Tubastraea = Tubastrea)
# Display the updated summary_table with site_name column
print(summary_table)
# View the updated summary table
summary_table
# Display the summary table
print(summary_table)
cleaned_table <- summary_table %>%
  select( -Unknown, -Other, -Caulerpa)

# Display the cleaned table
print(cleaned_table)

coral_table <- cleaned_table %>%
  select( -`DeadCoral-Framwork`,  -`DeadCoral-Rubble`, -Rock,  -Sand, -Tubastraea )

# Define coral categories
coral_categories <- c("Pavona", "Pocillopora", "Porites", "Psammocora", "Tubastraea")

# Calculate coral cover percentage per site
coral_cover_percent <- cleaned_table %>%
  rowwise() %>%
  mutate(
    total_coral = sum(c_across(all_of(coral_categories))),  # Sum coral categories
    row_total = sum(c_across(-project_id)),                # Sum all columns except project_id
    coral_cover_percent = (total_coral / row_total) * 100  # Calculate percent
  ) %>%
  select(project_id, coral_cover_percent)  %>%
  arrange(coral_cover_percent) %>%                        # Arrange by coral cover percentage
  mutate(project_id = factor(project_id, levels = project_id))                # Keep only necessary columns

# View the result
print(coral_cover_percent)


# Plot the coral cover percent
ggplot(coral_cover_percent, aes(x = project_id, y = coral_cover_percent)) +
  geom_bar(stat = "identity", fill = "gray") +
  geom_text(aes(label = round(coral_cover_percent, 1)), 
            vjust = "inward", color = "blue", size = 5) +
  labs(
    title = "",
    x = "Site",
    y = "Coral Cover (%)"
  ) +
  theme_minimal() + theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),  # Rotate and size x-axis labels
    axis.text.y = element_text(size = 12),  # Increase y-axis tick size
    axis.title.y = element_text(size = 16)  # Increase y-axis label size
  )


numeric_data <- as.matrix(coral_table[,-1])  # Exclude the first column with site names
rownames(numeric_data) <- coral_table$project_id 

# Calculate Shannon diversity index
shannon_diversity <- diversity(numeric_data, index = "shannon")

# Combine the results back into the original coral_table data frame
coral_table$shannon_diversity <- shannon_diversity
coral_table <- coral_table %>%
  arrange(shannon_diversity)  # Sort by Shannon diversity
# View the updated table
print(coral_table)


ggplot(coral_table, aes(x = reorder(project_id, shannon_diversity), y = shannon_diversity)) +
  geom_bar(stat = "identity", fill ="gray") +
  geom_text(aes(label = round(shannon_diversity, 3)), 
            vjust = "inward",color = "blue", size = 4) +
  labs(
    title = " ",
    x = "Site",
    y = "Shannon Diversity"
  ) +
  theme_minimal() + theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),  # Rotate and size x-axis labels
    axis.text.y = element_text(size = 12),  # Increase y-axis tick size
    axis.title.y = element_text(size = 16)  # Increase y-axis label size
  )


summary_table_fraction <- summary_table %>%
  mutate(across(.cols = -project_id, ~ . / rowSums(select(summary_table, -project_id)) * 100))  # Exclude 'project_id' column

# Add the 'project_id' column back
summary_table_fraction <- bind_cols(summary_table$project_id, summary_table_fraction)

# Rename the first column (which is 'project_id') for clarity
colnames(summary_table_fraction)[1] <- "project_id"

# Reshape the data into a long format for easier plotting
summary_table_long <- summary_table_fraction %>%
  pivot_longer(cols = -project_id, 
               names_to = "CoralType", 
               values_to = "Percentage")

# View the reshaped data
print(summary_table_long)

ggplot(summary_table_long, aes(x = reorder(CoralType, Percentage), y = Percentage, fill = CoralType)) +
  geom_bar(stat = "identity", position = "dodge") +  # Use position = "dodge" for side-by-side bars
  geom_text(aes(label = round(Percentage, 1)),  vjust = "inward", size = 3) +  # Adding labels
  facet_wrap(~ project_id, scales = "free_y", ncol = 4) +  # Facet by site
  labs(
    title = "all classes relative abundance",
    x = "Class",
    y = "Percentage"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),  # Rotate x-axis labels
    strip.text = element_text(size = 10),  # Increase facet title size
    legend.position = "none" ,plot.title = element_text(size = 14) # Move legend to bottom-right
  )

numeric_data <- as.matrix(cleaned_table[,-1])  # Exclude the first column with site names
rownames(numeric_data) <- cleaned_table$project_id   # Set site names as row names

pca <- prcomp(numeric_data, scale. = TRUE)
# Create PCA plot with site_names as the coloring factor and labels using ggrepel
autoplot(pca, data = numeric_data,
         loadings = TRUE, loadings.colour = '#999999',
         loadings.label = TRUE, loadings.label.size = 5, loadings.label.colour ='#999999',
         loadings.label.repel = TRUE) +
  geom_text_repel(aes(label = site_names), 
                  box.padding = 1,           # Increased box padding for more space around the labels
                  max.overlaps = 1000,       # Increased max overlaps to handle dense labels
                  point.padding = 1,         # Increased point padding to move labels further away from points
                  segment.color = 'gray', 
                  size = 5) +                # Increased text size for site labels
  theme_minimal() +                # Use minimal theme
  theme(panel.background = element_blank(), # Set background to white
        plot.background = element_blank(),   # Remove plot background
        axis.text.x = element_text(size = 14),   # Increase size of x-axis ticks
        axis.text.y = element_text(size = 14),   # Increase size of y-axis ticks
        axis.title.x = element_text(size = 16),  # Increase size of x-axis label
        axis.title.y = element_text(size = 16)) +coord_fixed()+
  theme_classic()+ 
  theme(panel.background = element_rect(fill = NA, colour = "black", size = 1, linetype = "solid"))+
  theme(legend.position = "right", legend.text = element_text(size = 14), legend.title = element_text(size = 16), axis.text = element_text(size = 16),
        axis.title=element_text(color="#000000",size=16),
        axis.line = element_line(colour = "#000000",size=0.8),
        axis.ticks=element_line(colour="#000000",size=3),
        axis.ticks.length=unit(.3, "cm"),
        aspect.ratio=0.8)  # Increase size of y-axis label
