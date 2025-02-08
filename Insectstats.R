# Import necessary libraries
library(readr)
library(ggplot2)
library(dplyr)
library(tidyr)
library(vegan)

# Load the data
data <- read.csv("BugCollection.csv")

# Preview the data
head(data)

# Filter out rows where 'Biomass' is NA and use only rows where Family is 'Total'
total_data <- data %>%
  filter(!is.na(Biomass) & Family == "Total")

# Preview total data
head(total_data)


# Calculate species richness over time
richness_data <- data %>%
  group_by(Habitat, Date) %>%
  summarise(Species_Richness = n_distinct(Family))

# Calculate species richness over time by habitat
richness_data <- data %>%
  group_by(Habitat, Date) %>%
  summarise(Species_Richness = n_distinct(Family))

# Preview the calculated richness data
head(richness_data)

# Convert 'Date' column to Date format
data$Date <- as.Date(data$Date, format = "%d-%b") # Adjust this format as per your data (e.g., "12-Jun")

# Re-run the species richness calculation
richness_data <- data %>%
  group_by(Habitat, Date) %>%
  summarise(Species_Richness = n_distinct(Family))

# Check for duplicate Date-Habitat observations
duplicated_data <- data %>%
  group_by(Habitat, Date) %>%
  filter(n() > 1)

# View potential duplicates
head(duplicated_data)

############Species Richness over time
# Smoothed plot for species richness over time by habitat
ggplot(richness_data, aes(x = Date, y = Species_Richness, color = Habitat)) +
  geom_smooth(method = "loess", se = FALSE) +  # Smooth the lines, remove confidence intervals (se = FALSE)
  labs(title = "Smoothed Species Richness Over Time by Habitat", x = "Date", y = "Number of Families") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Adjust the x-axis text for better readability

# Smoothed plot with facets by habitat
ggplot(richness_data, aes(x = Date, y = Species_Richness)) +
  geom_smooth(method = "loess", se = FALSE) +  # Smoothing with LOESS
  facet_wrap(~ Habitat, scales = "free_y") +
  labs(title = "Smoothed Species Richness Over Time by Habitat", x = "Date", y = "Number of Families") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

######Biomass over time
# Filter the data to keep only the 'Total' family rows for biomass
total_biomass_data <- data %>%
  filter(Family == "Total") %>%
  group_by(Habitat, Date) %>%
  summarise(Total_Biomass = sum(Biomass, na.rm = TRUE))

# Preview the data
head(total_biomass_data)

# Smoothed plot for total biomass over time by habitat
ggplot(total_biomass_data, aes(x = Date, y = Total_Biomass, color = Habitat)) +
  geom_smooth(method = "loess", se = FALSE) +  # Smoothed lines for biomass trends
  labs(title = "Smoothed Total Biomass Over Time by Habitat", x = "Date", y = "Total Biomass (g)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Adjust the x-axis text for better readability

# Smoothed plot with facets by habitat
ggplot(total_biomass_data, aes(x = Date, y = Total_Biomass)) +
  geom_smooth(method = "loess", se = FALSE) +  # Smoothing with LOESS
  facet_wrap(~ Habitat, scales = "free_y") +
  labs(title = "Smoothed Total Biomass Over Time by Habitat", x = "Date", y = "Total Biomass (g)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

######Location differences

# Filter the data to keep only the 'Total' family rows for biomass and include Location
total_biomass_data <- data %>%
  filter(Family == "Total") %>%
  group_by(Location, Habitat, Date) %>%
  summarise(Total_Biomass = sum(Biomass, na.rm = TRUE))

# Preview the data
head(total_biomass_data)

# Smoothed plot comparing biomass over time by location
ggplot(total_biomass_data, aes(x = Date, y = Total_Biomass, color = Location)) +
  geom_smooth(method = "loess", se = FALSE) +  # Smooth the lines
  facet_wrap(~ Habitat, scales = "free_y") +   # Facet by Habitat to compare within each habitat
  labs(title = "Smoothed Total Biomass Over Time by Location and Habitat", 
       x = "Date", y = "Total Biomass (g)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Adjust the x-axis text for readability

# Smoothed plot comparing biomass over time by location without faceting
ggplot(total_biomass_data, aes(x = Date, y = Total_Biomass, color = Location)) +
  geom_smooth(method = "loess", se = FALSE) +  # Smooth the lines
  labs(title = "Smoothed Total Biomass Over Time by Location", 
       x = "Date", y = "Total Biomass (g)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Adjust the x-axis text for readability

####### stats difference between habitat types

# Group by Location, Habitat, and Date, and summarize Avg_Wind while keeping the Moon phase as is
total_biomass_data <- data %>%
  filter(Family == "Total") %>%  # Use only the rows where Family is "Total" for each sample
  group_by(Location, Habitat, Date, Moon) %>%  # Group by Moon as well, since it's consistent per group
  summarise(
    Total_Biomass = sum(Biomass, na.rm = TRUE),  # Summing the total biomass
    Avg_Wind = mean(Avg_Wind, na.rm = TRUE)      # Averaging wind speed
  )

# View the updated dataset
head(total_biomass_data)

# Mixed effects model for total biomass, controlling for Avg Wind, Moon, and Location
biomass_model <- lmer(Total_Biomass ~ Habitat + Avg_Wind + Moon + (1 | Location), data = total_biomass_data)

# View the model summary
summary(biomass_model)

# Mixed effects model for species richness, controlling for Avg Wind, Moon, and Location
richness_model <- lmer(Species_Richness ~ Habitat + Avg_Wind + Moon + (1 | Location), data = richness_data)

# View the model summary
summary(richness_model)


# Load the package
library(emmeans)

# Pairwise comparisons between habitats for species richness
richness_emmeans <- emmeans(richness_model, pairwise ~ Habitat)

# View the pairwise comparison results
summary(richness_emmeans)

# Pairwise comparisons between habitats for biomass
biomass_emmeans <- emmeans(biomass_model, pairwise ~ Habitat)

# View the pairwise comparison results
summary(biomass_emmeans)

#####Habitats over time

# Group the data by Location, Habitat, and Date, and calculate species richness and Avg_Wind
richness_data <- data %>%
  group_by(Location, Habitat, Date) %>%
  summarise(
    Species_Richness = n_distinct(Family),  # Count distinct families for species richness
    Avg_Wind = mean(Avg_Wind, na.rm = TRUE)  # Calculate average wind for each group
  )

# View the updated richness dataset with Avg_Wind included
head(richness_data)

# Mixed effects model for species richness, focusing on Habitat and Date, with Avg_Wind as a control variable
richness_model_with_wind <- lmer(Species_Richness ~ Habitat + Date + Avg_Wind + (1 | Location), 
                                 data = richness_data)

# View the model summary
summary(richness_model_with_wind)

# Mixed effects model for biomass, focusing on Habitat and Date, with Avg_Wind as a control variable
biomass_model_with_wind <- lmer(Total_Biomass ~ Habitat + Date + Avg_Wind + (1 | Location), 
                                data = total_biomass_data)

# View the model summary
summary(biomass_model_with_wind)


#GLMM________________________________

# Load the necessary library
library(lme4)

# Function to calculate overdispersion in a GLMM
overdisp_fun <- function(model) {
  rdf <- df.residual(model)  # Residual degrees of freedom
  rp <- residuals(model, type = "pearson")  # Pearson residuals
  Pearson_chisq <- sum(rp^2)  # Sum of squared Pearson residuals
  prat <- Pearson_chisq / rdf  # Overdispersion ratio
  pval <- pchisq(Pearson_chisq, df = rdf, lower.tail = FALSE)  # P-value for the chi-squared test
  c(chisq = Pearson_chisq, ratio = prat, rdf = rdf, p = pval)  # Return chi-squared, ratio, and p-value
}

# Apply the function to your Poisson GLMM model
overdisp_fun(richness_glmm_wind) #Not over dispersed so Poisson is good!

# Fit a Poisson GLMM with Avg_Wind as a random effect
richness_glmm_wind <- glmer(Species_Richness ~ Habitat + Date + (1 | Location) + (1 | Avg_Wind), 
                            data = richness_data, 
                            family = poisson)
# View the model summary
summary(richness_glmm_wind)

# Load emmeans package
library(emmeans)

# Pairwise comparisons within each Date
richness_emmeans_within_date <- emmeans(richness_glmm_wind, ~ Habitat | Date)

# View pairwise comparisons within each date
pairs(richness_emmeans_within_date)

# Correctly order the Date variable (assuming these are the correct dates in your dataset)
richness_data$Date <- factor(richness_data$Date, 
                             levels = c("19-May", "29-May", "03-Jun", "12-Jun", "25-Jul", "02-Aug", "11-Aug", "18-Aug", "28-Aug", "09-Sep"), 
                             ordered = TRUE)

# Re-run the GLMM model after fixing the Date order
richness_glmm_wind <- glmer(Species_Richness ~ Habitat + Date + (1 | Location) + (1 | Avg_Wind), 
                            data = richness_data, 
                            family = poisson)
# Pairwise comparisons between successive dates for the same Habitat
richness_emmeans_across_dates <- emmeans(richness_glmm_wind, ~ Date | Habitat)

# Perform sequential comparisons between successive dates
sequential_contrasts <- contrast(richness_emmeans_across_dates, method = "consec")

# View the pairwise comparisons between successive dates for each habitat
summary(sequential_contrasts)

# Convert Date column to Date format if it's not already
richness_data$Date <- as.Date(richness_data$Date, format = "%d-%b")  # Adjust date format based on your data

# Plot smooth lines for species richness trends over time by habitat
ggplot(richness_data, aes(x = Date, y = Species_Richness, color = Habitat)) +
  geom_smooth(method = "loess", se = FALSE) +  # Add smooth lines without confidence intervals
  labs(y = "Species Richness", x = "Date", title = "Species Richness Trends by Habitat over Time") +
  theme_minimal()


# Correctly order the Date variable for the biomass data
total_biomass_data$Date <- factor(total_biomass_data$Date, 
                                  levels = c("19-May", "29-May", "03-Jun", "12-Jun", "25-Jul", "02-Aug", "11-Aug", "18-Aug", "28-Aug", "09-Sep"), 
                                  ordered = TRUE)
# Perform pairwise comparisons between successive dates for the same Habitat (for biomass)
biomass_emmeans_across_dates <- emmeans(biomass_glmm_wind, ~ Date | Habitat)

# Perform sequential comparisons between successive dates
sequential_biomass_contrasts <- contrast(biomass_emmeans_across_dates, method = "consec")

# View the pairwise comparisons between successive dates for each habitat
summary(sequential_biomass_contrasts)

# Shapiro-Wilk normality test for Total_Biomass
shapiro.test(total_biomass_data$Total_Biomass)

# Fit a Tweedie model for biomass data
biomass_tweedie_model <- glmmTMB(Total_Biomass ~ Habitat + Date + (1 | Location) + (1 | Avg_Wind), 
                                 data = total_biomass_data, 
                                 family = tweedie(link = "log"))

# View the model summary
summary(biomass_tweedie_model)

# Load the emmeans package
library(emmeans)

# Pairwise comparisons between habitats within each date
biomass_emmeans_habitat <- emmeans(biomass_tweedie_model, ~ Habitat | Date)
pairs(biomass_emmeans_habitat)

# Pairwise comparisons between dates for each habitat
biomass_emmeans_dates <- emmeans(biomass_tweedie_model, ~ Date | Habitat)
pairs(biomass_emmeans_dates)

# Convert Date column to Date format 
total_biomass_data$Date <- as.Date(total_biomass_data$Date, format = "%d-%b")  # Adjust the date format to match your data

# Plot smooth lines for biomass trends over time by habitat
ggplot(total_biomass_data, aes(x = Date, y = Total_Biomass, color = Habitat)) +
  geom_smooth(method = "loess", se = FALSE) +  # Add smooth lines without points
  labs(y = "Biomass (g)", x = "Date", title = "Biomass Trends by Habitat over Time") +
  theme_minimal()

###See if big water and little water should be combined (yes they should be)
# Create a new variable combining Big Water and Little Water into a single category
richness_data$Combined_Habitat <- richness_data$Habitat
richness_data$Combined_Habitat[richness_data$Habitat %in% c("Big Water", "Little Water")] <- "Water"

# Create a new variable combining Big Water and Little Water for biomass dataset
total_biomass_data$Combined_Habitat <- total_biomass_data$Habitat
total_biomass_data$Combined_Habitat[total_biomass_data$Habitat %in% c("Big Water", "Little Water")] <- "Water"

# Original model with Big Water and Little Water separate
richness_model_separate <- glmer(Species_Richness ~ Habitat + Date + (1 | Location), 
                                 data = richness_data, family = poisson)

# Model with combined "Water" category
richness_model_combined <- glmer(Species_Richness ~ Combined_Habitat + Date + (1 | Location), 
                                 data = richness_data, family = poisson)

# Compare the models
anova(richness_model_separate, richness_model_combined)

# Original model with Big Water and Little Water separate
biomass_model_separate <- glmmTMB(Total_Biomass ~ Habitat + Date + (1 | Location) + (1 | Avg_Wind), 
                                  data = total_biomass_data, family = tweedie(link = "log"))

# Model with combined "Water" category
biomass_model_combined <- glmmTMB(Total_Biomass ~ Combined_Habitat + Date + (1 | Location) + (1 | Avg_Wind), 
                                  data = total_biomass_data, family = tweedie(link = "log"))

# Compare the models
anova(biomass_model_separate, biomass_model_combined)

###GLMM when waters are combined####################3
# Combine Big Water and Little Water into "Water"
total_biomass_data$Combined_Habitat <- total_biomass_data$Habitat
total_biomass_data$Combined_Habitat[total_biomass_data$Habitat %in% c("Big Water", "Little Water")] <- "Water"

# Combine Big Water and Little Water into "Water"
richness_data$Combined_Habitat <- richness_data$Habitat
richness_data$Combined_Habitat[richness_data$Habitat %in% c("Big Water", "Little Water")] <- "Water"

# Fit the GLMM for biomass using the combined "Water" category
biomass_glmm_combined <- glmmTMB(Total_Biomass ~ Combined_Habitat + Date + (1 | Location) + (1 | Avg_Wind), 
                                 data = total_biomass_data, family = tweedie(link = "log"))

# View the model summary
summary(biomass_glmm_combined)


# Check for negative or zero values in Total_Biomass
summary(total_biomass_data$Total_Biomass)

# Load the necessary library
library(glmmTMB)
# Fit a Tweedie GLMM for biomass data
biomass_tweedie_model <- glmmTMB(Total_Biomass ~ Combined_Habitat + Date + (1 | Location) + (1 | Avg_Wind), 
                                 data = total_biomass_data, 
                                 family = tweedie(link = "log"))

# View the model summary
summary(biomass_tweedie_model)

# Plot residuals to check model fit
plot(residuals(biomass_tweedie_model))

# Q-Q plot for residuals
qqnorm(residuals(biomass_tweedie_model))
qqline(residuals(biomass_tweedie_model), col = "red")

# Plot residuals against fitted values
plot(fitted(biomass_tweedie_model), residuals(biomass_tweedie_model))
abline(h = 0, col = "red")

# Check for extreme values in the Total_Biomass column
summary(total_biomass_data$Total_Biomass)

# Plot to visually inspect for outliers
boxplot(total_biomass_data$Total_Biomass)

# Log-transform the Total_Biomass variable (add a small constant to handle zeros)
total_biomass_data$Log_Biomass <- log(total_biomass_data$Total_Biomass + 1)

# Refit the model after removing or transforming the outlier
biomass_tweedie_model_clean <- glmmTMB(Total_Biomass ~ Combined_Habitat + Date + (1 | Location) + (1 | Avg_Wind), 
                                       data = total_biomass_data, 
                                       family = tweedie(link = "log"))

# Check the model summary
summary(biomass_tweedie_model_clean)

######Proposal figures

ggplot(total_biomass_data, aes(x = Date, y = Total_Biomass, color = Combined_Habitat)) +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"), se = FALSE) +  # Use GAM smoother
  labs(title = "Biomass Over Time by Habitat Type",
       x = "Date",
       y = "Total Biomass",
       color = "Habitat Type") +
  theme_minimal() +
  ylim(0, NA)  # Set lower limit of y-axis to 0


# Plot Species Richness over Time by Habitat Type
ggplot(richness_data, aes(x = Date, y = Species_Richness, color = Combined_Habitat)) +
  geom_smooth(method = "loess", se = FALSE) +  # Smoothing line without confidence intervals
  labs(title = "Species Richness Over Time by Habitat Type",
       x = "Date",
       y = "Species Richness",
       color = "Habitat Type") +
  theme_minimal()

####Oct 17 trial 

insect_data <- read.csv("BugCollection.csv")

# Load necessary libraries
library(ggplot2)
library(dplyr)
library(lubridate)  # To help with date manipulation

# Ensure the Quantity column is numeric
insect_data$Quantity <- as.numeric(insect_data$Quantity)

# Convert Date column to Date type
insect_data$Date <- as.Date(insect_data$Date, format = "%Y-%m-%d")

# Create a new column for Month (without grouping by month, just for labeling)
insect_data$Month <- floor_date(insect_data$Date, unit = "month")

# Filter the data for Lepidoptera only
lepidoptera_data <- insect_data %>%
  filter(Order == "Lepidoptera")

# Summarize the total abundance per family over time
lepidoptera_abundance <- lepidoptera_data %>%
  group_by(Date, Family) %>%
  summarise(total_abundance = sum(Quantity, na.rm = TRUE))

# Plotting the trends over time
ggplot(lepidoptera_abundance, aes(x = Date, y = total_abundance, color = Family)) +
  geom_smooth(method = "loess", se = FALSE, span = 0.3) +  # Smoothed trend lines
  labs(title = "Lepidoptera Abundance Over Time (Smoothed)",
       x = "Month",
       y = "Total Abundance",
       color = "Family") +
  theme_minimal() +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +  # Label by month only
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(limits = c(0, NA))  # Sets minimum y-axis to 0, allowing upper limit to be auto-scaled


#### Above is all lep

#### Bellow is top 10 most frqeunt moths

# Ensure the Quantity column is numeric
insect_data$Quantity <- as.numeric(insect_data$Quantity)

# Convert Date column to Date type
insect_data$Date <- as.Date(insect_data$Date, format = "%Y-%m-%d")

# Create a new column for Month (for x-axis labeling)
insect_data$Month <- floor_date(insect_data$Date, unit = "month")

# Filter the data for Lepidoptera only
lepidoptera_data <- insect_data %>%
  filter(Order == "Lepidoptera")

# Calculate total abundance for each family
family_abundance <- lepidoptera_data %>%
  group_by(Family) %>%
  summarise(total_abundance = sum(Quantity, na.rm = TRUE)) %>%
  arrange(desc(total_abundance))

# Select the top 10 most abundant families
top_10_families <- family_abundance %>%
  top_n(10, total_abundance) %>%
  pull(Family)

# Filter the data to include only the top 10 families
top_10_data <- lepidoptera_data %>%
  filter(Family %in% top_10_families)

# Filter out "Micro Lep" and create a frequency table for the number of unique dates each family was sampled
family_frequency <- lepidoptera_data %>%
  filter(Family != "Micro Lep") %>%  # Exclude "Micro Lep"
  group_by(Family) %>%
  summarise(sampled_dates_count = n_distinct(Date)) %>%
  arrange(desc(sampled_dates_count))

# Filter out "Micro Lep" and calculate the number of unique sampling dates for each family
family_frequency <- lepidoptera_data %>%
  filter(Family != "Micro Lep") %>%  # Exclude "Micro Lep"
  group_by(Family) %>%
  summarise(sampled_dates_count = n_distinct(Date)) %>%
  arrange(desc(sampled_dates_count))

# Get the top 10 most commonly sampled families based on frequency
top_10_families_by_frequency <- family_frequency %>%
  slice_max(order_by = sampled_dates_count, n = 10)  # Ensure top 10 families are selected

# Filter the top 10 abundance data to include only the top 10 families
top_10_abundance <- lepidoptera_data %>%
  filter(Family %in% top_10_families_by_frequency$Family)

# Reorder the Family factor in the top_10_abundance dataset
top_10_abundance$Family <- factor(top_10_abundance$Family, levels = top_10_families_by_frequency$Family)

# Plot with reordered legend based on frequency of sampling (excluding "Micro Lep")
ggplot(top_10_abundance, aes(x = Date, y = Quantity, color = Family)) +
  geom_smooth(method = "loess", se = FALSE, span = 0.3) +  # Smoothed trend lines
  labs(title = "Lepidoptera Family Abundance During 2024 Field Season",
       x = "Month",
       y = "Individuals Sampled",
       color = "Family") +
  theme_minimal() +
  scale_x_date(
    date_breaks = "1 month",
    date_labels = "%B",
    expand = c(0, 0),
    limits = as.Date(c("2024-05-01", "2024-09-30")),
    breaks = seq(as.Date("2024-05-01"), as.Date("2024-09-01"), by = "1 month")  # Specify exact months
  ) +
  theme(
    axis.text = element_text(size = 14),  # Increase x and y axis text size
    axis.title = element_text(size = 16),  # Increase axis title size
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),  # Increase title size and center it
    legend.title = element_text(size = 16),  # Increase legend title size
    legend.text = element_text(size = 14)    # Increase legend text size
  ) +
  scale_y_continuous(limits = c(0, NA))  # Ensures y-axis starts at 0


# Count the number of unique sampling dates each family appears in and the total number sampled
# Filter out the "Micro Lep" family from the data
family_summary <- lepidoptera_data %>%
  filter(Family != "Micro Lep") %>%
  group_by(Family) %>%
  summarise(
    sampled_dates_count = n_distinct(Date),
    total_individuals = sum(Quantity, na.rm = TRUE)
  ) %>%
  arrange(desc(sampled_dates_count))

# Get the top 10 families based on the number of sampled dates
top_families_summary <- family_summary %>%
  top_n(10, sampled_dates_count)

# Create a table to display the top families with their occurrence counts and total number sampled
top_families_table <- top_families_summary %>%
  select(Family, sampled_dates_count, total_individuals)

# Display the table
library(knitr)
kable(top_families_table, caption = "Top 10 Most Commonly Captured Moth Families Based on Occurrence Across Sampling Dates and Total Individuals Sampled (Excluding Micro Lep)")



###Coleoptera

# Ensure the Quantity column is numeric
insect_data$Quantity <- as.numeric(insect_data$Quantity)

# Convert Date column to Date type
insect_data$Date <- as.Date(insect_data$Date, format = "%Y-%m-%d")

# Create a new column for Month (for x-axis labeling)
insect_data$Month <- floor_date(insect_data$Date, unit = "month")

# Filter the data for Coleoptera only
coleoptera_data <- insect_data %>%
  filter(Order == "Coleoptera")

# Calculate total abundance for each family
coleoptera_abundance <- coleoptera_data %>%
  group_by(Family) %>%
  summarise(total_abundance = sum(Quantity, na.rm = TRUE)) %>%
  arrange(desc(total_abundance))

# Select the top 10 most abundant families
top_10_coleoptera_families <- coleoptera_abundance %>%
  top_n(10, total_abundance) %>%
  pull(Family)

# Filter the data to include only the top 10 families
top_10_coleoptera_data <- coleoptera_data %>%
  filter(Family %in% top_10_coleoptera_families)

# Summarize the total abundance per family over time
top_10_coleoptera_abundance <- top_10_coleoptera_data %>%
  group_by(Date, Family) %>%
  summarise(total_abundance = sum(Quantity, na.rm = TRUE))

# Plotting the trends over time for the top 10 Coleoptera families
ggplot(top_10_coleoptera_abundance, aes(x = Date, y = total_abundance, color = Family)) +
  geom_point(size = 2) +  # Points for individual data points
  geom_smooth(method = "loess", se = FALSE, span = 0.3) +  # Smoothed trend lines
  labs(title = "Top 10 Coleoptera Families Abundance Over Time (Smoothed)",
       x = "Month",
       y = "Total Abundance",
       color = "Family") +
  theme_minimal() +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +  # Label by month only
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## Order trends over time

# Load necessary libraries
library(ggplot2)
library(dplyr)
library(lubridate)

# Ensure the Quantity column is numeric
insect_data$Quantity <- as.numeric(insect_data$Quantity)

# Convert Date column to Date type
insect_data$Date <- as.Date(insect_data$Date, format = "%Y-%m-%d")

# Create a new column for Month (for x-axis labeling)
insect_data$Month <- floor_date(insect_data$Date, unit = "month")

# Exclude rows where Order is NA, blank, or "Unknown" or "Varies (immature)"
insect_data_filtered <- insect_data %>%
  filter(!is.na(Order) & Order != "" & Order != "Unknown" & Order != "Varies (immature)")

# Summarize the total abundance per order over time
order_abundance <- insect_data_filtered %>%
  group_by(Date, Order) %>%
  summarise(total_abundance = sum(Quantity, na.rm = TRUE))

# Plotting the trends over time for all Orders (using loess smoothing and excluding unwanted orders)
ggplot(order_abundance, aes(x = Date, y = total_abundance, color = Order)) +
  geom_smooth(method = "loess", span = 0.4, se = FALSE) +  # Use LOESS smoothing without individual points
  labs(title = "Abundance of Insect Orders Over Time",
       x = "Month",
       y = "Total Abundance",
       color = "Order") +
  theme_minimal() +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +  # Label by month only
  scale_y_continuous(limits = c(0, NA)) +  # Restrict y-axis to non-negative values
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Reorder Order based on total abundance
order_abundance$Order <- factor(order_abundance$Order, 
                                levels = order_abundance %>% 
                                  group_by(Order) %>% 
                                  summarise(total = sum(total_abundance)) %>% 
                                  arrange(desc(total)) %>% 
                                  pull(Order))


# Create a frequency table for the number of samples each order appears in
order_frequency <- order_abundance %>%
  group_by(Order) %>%
  summarise(sampled_dates_count = n_distinct(Date)) %>%
  arrange(desc(sampled_dates_count))

# Reorder the 'Order' factor in the 'order_abundance' dataset based on the frequency of sampling
order_abundance$Order <- factor(order_abundance$Order, levels = order_frequency$Order)

# Plot with the legend reordered by frequency of sampling USE IN PROPOSAL 
ggplot(order_abundance, aes(x = Date, y = total_abundance, color = Order)) +
  geom_smooth(method = "loess", span = 0.4, se = FALSE) +  # Use LOESS smoothing without individual points
  labs(title = "Abundance of Insect Orders Over 2024 Field Season",
       x = "Month",
       y = "Total Abundance",
       color = "Order") +
  theme_minimal() +
  scale_x_date(
    date_breaks = "1 month",
    date_labels = "%B",
    expand = c(0, 0),
    limits = as.Date(c("2024-05-01", "2024-09-30")),
    breaks = as.Date(c("2024-05-01", "2024-06-01", "2024-07-01", "2024-08-01", "2024-09-01"))  # Specify exact months
  ) +
  scale_y_continuous(limits = c(0, NA)) +  # Restrict y-axis to non-negative values
  theme(
    axis.text = element_text(size = 14),      # Increase x and y axis text size
    axis.title = element_text(size = 16),     # Increase axis title size
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),  # Increase title size and center it
    legend.title = element_text(size = 16),   # Increase legend title size
    legend.text = element_text(size = 14),    # Increase legend text size
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis labels
  )


####biomass overtime by habitat USAE FOR PROPOSAL   
# Convert Biomass to numeric and filter out any rows with NA values
insect_data$Biomass <- as.numeric(insect_data$Biomass)
insect_data <- insect_data %>%
  filter(!is.na(Biomass))

biomass_plot=ggplot(insect_data, aes(x = Date, y = Biomass, color = Habitat)) +
  geom_smooth(method = "loess", span = 0.4, se = FALSE) +  # Use LOESS smoothing without individual points
  labs(title = "Biomass Over Time by Habitat (g)",
       x = "Month",
       y = "Total Biomass",
       color = "Habitat") +
  theme_minimal() +
  scale_x_date(date_breaks = "1 month", date_labels = "%B", 
               expand = expansion(add = c(15, 15))) +  # Ensures May appears on the axis
  scale_y_continuous(limits = c(0, NA)) +  # Restrict y-axis to non-negative values
  theme(
    axis.text = element_text(size = 14),      # Increase x and y axis text size
    axis.title = element_text(size = 16),     # Increase axis title size
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),  # Increase title size and center it
    legend.title = element_text(size = 16),   # Increase legend title size
    legend.text = element_text(size = 14),    # Increase legend text size
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis labels
  )
biomass_plot


####Families overtime by habitat ###USE FOR PROPOSAL

# Combine "Little Water" and "Water" into a single category
insect_data <- insect_data %>%
  mutate(Habitat = ifelse(Habitat %in% c("Little Water", "Water"), "Water", Habitat))

# Summarize data to get the number of unique families per habitat per date
family_count_data <- insect_data %>%
  group_by(Date, Habitat) %>%
  summarise(num_families = n_distinct(Family), .groups = 'drop')

# Plot the number of families sampled per habitat over time
richness_plot=ggplot(family_count_data, aes(x = Date, y = num_families, color = Habitat)) +
  geom_smooth(method = "loess", span = 0.4, se = FALSE) +  # Smoothed trend lines
  labs(title = "Number of Families Sampled Over Time by Habitat",
       x = "Month",
       y = "Number of Families",
       color = "Habitat") +
  theme_minimal() +
  scale_x_date(date_breaks = "1 month", date_labels = "%B", 
               expand = expansion(add = c(15, 15))) +  # Ensures May appears on the axis
  scale_y_continuous(limits = c(0, NA)) +  # Restrict y-axis to non-negative values
  theme(
    axis.text = element_text(size = 14),      # Increase x and y axis text size
    axis.title = element_text(size = 16),     # Increase axis title size
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),  # Increase title size and center it
    legend.title = element_text(size = 16),   # Increase legend title size
    legend.text = element_text(size = 14),    # Increase legend text size
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis labels
  )
richness_plot


# Print a summary of the number of unique families per habitat and date for verification
family_count_check <- insect_data %>%
  filter(!is.na(Family) & Family != "") %>%  # Ensure no missing or empty family values
  group_by(Date, Habitat) %>%
  summarise(num_families = n_distinct(Family), .groups = 'drop')

# Print a sample of the data for inspection
print(head(family_count_check, 20))
summary(family_count_check$num_families)

# Plot the data
richness_plot = ggplot(family_count_check, aes(x = Date, y = num_families, color = Habitat)) +
  geom_smooth(method = "loess", span = 0.4, se = FALSE) +  # Smoothed trend lines
  labs(title = "Number of Families Sampled Over Time by Habitat",
       x = "Month",
       y = "Number of Families",
       color = "Habitat") +
  theme_minimal() +
  scale_x_date(date_breaks = "1 month", date_labels = "%B", 
               expand = expansion(add = c(15, 15))) +  # Ensures May appears on the axis
  scale_y_continuous(limits = c(0, NA)) +  # Restrict y-axis to non-negative values
  theme(
    axis.text = element_text(size = 14),      # Increase x and y axis text size
    axis.title = element_text(size = 16),     # Increase axis title size
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),  # Increase title size and center it
    legend.title = element_text(size = 16),   # Increase legend title size
    legend.text = element_text(size = 14),    # Increase legend text size
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis labels
  )
richness_plot



# Ensure patchwork is loaded
library(patchwork)

# Combine the plots using wrap_plots to avoid ggplot_add() issues
combined_plot <- wrap_plots(richness_plot, biomass_plot, ncol = 2) +
  plot_annotation(tag_levels = 'A') &
  theme(plot.tag = element_text(size = 20, face = "bold"))

# Display the combined plot
print(combined_plot)

##################NMDS multivariate analysis trial
# Filter out the "Total" rows
data_filtered <- data %>%
  filter(Family != "Total")  # Exclude rows where Family is "Total"

# Restructure data to create a community matrix
community_matrix <- data_filtered %>%
  group_by(Habitat, Date, Family) %>%
  summarise(Total_Quantity = sum(Quantity), .groups = "drop") %>%
  pivot_wider(names_from = Family, values_from = Total_Quantity, values_fill = 0)

# Save metadata (Habitat and Date)
metadata <- community_matrix %>% select(Habitat, Date)

# Extract only the community data (families as columns)
community_data <- community_matrix %>% select(-Habitat, -Date)

# Check for NA values in the community matrix (trouble shooting)
which(is.na(community_data), arr.ind = TRUE)  # Shows positions of NA values

# View the rows with NA values (checking where issues are)
community_data[c(2, 6, 19, 21, 46), ]

# Replace all NA values with 0 (fixing issues)
community_data[is.na(community_data)] <- 0

# Run NMDS
nmds_result <- metaMDS(community_data, distance = "bray", k = 2, trymax = 100)

# Optionally inspect your data
summary(community_data)  # Summary to see if any column has unexpected NAs

# Plot NMDS result
plot(nmds_result, type = "t")

# Add hulls for habitats
ordihull(nmds_result, groups = metadata$Habitat, draw = "polygon", col = c("red", "blue", "green"))

# Test for significant differences (PERMANOVA)
adonis2(community_data ~ Habitat + Date, data = metadata, method = "bray")
