#####################Determinants of Access to Livestock Information Sources: Evidence from Poisson Regression Analysis
##########OBJECTIVE: To describe and visualize the frequency of access 
#             to different beef value chain information sources
###########data imporation###############################
install.packages("readxl") 
library("readxl")
data =  read_excel("C:/Users/user/Downloads/infor_acc.xlsx")
names(data)
#######droping unwanted variables
install.packages("dplyr")
library("dplyr")
data = data %>% select(-Gender,-Education,age,-beef_cattle_herd_size,-dist_beef_mkt,-beef_value_chain)
View(data) # viewing to confirm unwanted variables have been dropped
unique(data)
head(data) #checking few rows of my data

#check for missing values in my data set
####Count total number of missing values in my data set
sum(is.na(data))

# checking for missing values per variable/ column
colSums(is.na(data))
#no missing  values lets proceed

## lets now summarize the number of times each source was accessed
install.packages("tidyverse")
library("tidyverse")
info_summary <- data %>%
  select(-index) %>%  # excludes the  index column
  summarise_all(sum) %>%
  pivot_longer(cols = everything(), names_to = "source", values_to = "count")

#lets convert all variables to numeric so that the function summarise() works
View(data)
data = data %>% select(-age)
View(data)
info_summary <- data %>%
  select(-index) %>%                             # remove index column
  mutate(across(everything(), as.numeric)) %>%  # convert all to numeric
  summarise(across(everything(), sum, na.rm = TRUE)) %>%
  pivot_longer(cols = everything(), names_to = "source", values_to = "count")

#view summary of data
print(info_summary)

####plotting our bar graphs
install.packages("gglot2")
library("ggplot2")
ggplot(info_summary, aes(x = reorder(source, -count), y = count, fill = source)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Number of Respondents Accessing Each Information Source",
    x = "Information Source",
    y = "Number of Respondents"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#what the code does 
# Add percentage column
info_summary <- info_summary %>%
  mutate(percentage = round(100 * count / sum(count), 1))
#lets plot with pecentages
library(ggplot2)

ggplot(info_summary, aes(x = reorder(source, -count), y = count, fill = source)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(percentage, "%")), vjust = -0.5, size = 3.5) +
  labs(
    title = "Number of Respondents Accessing Each Information Source",
    x = "Information Source",
    y = "Number of Respondents"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(legend.position = "none")


ggplot_object <- ggplot(info_summary, aes(x = reorder(source, -count), y = count, fill = source)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(percentage, "%")), vjust = -0.5, size = 3.5) +
  labs(
    title = "Number of Respondents Accessing Each Information Source",
    x = "Information Source",
    y = "Number of Respondents"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(legend.position = "none")

# Save the plot with smaller size (in inches)
ggsave("info_source_access_plot.png", plot = ggplot_object, width = 6, height = 4, dpi = 300)

##########################lets proceed to regression analysis##############################
data = read_excel("C:/Users/user/Downloads/infor_acc.xlsx")
###preparation of y variable by;
#Defining the list of dummy columns representing information sources
info_vars <- c("gov_xtension", "research_inst", "training_inst", "stockists",
               "farmer_grps", "cooperatives", "ngos_cbos", "other_farmers",
               "faith_org", "kcsap", "private_firms", "other_source")
#by concatination we combine multiple items together into a single object which called a vector

###now we convert infor_vars to numeric so that we can obtain the sum
data[info_vars] <- lapply(data[info_vars], function(x) as.numeric(as.character(x)))

#====Create a new variable that sums across all these columns==========##########
data$total_info_sources <- rowSums(data[, info_vars], na.rm = TRUE)

# View the new variable
head(data$total_info_sources)


###########now we proceed on working on predictor variables
########convert categorical variables to factors
#lets re_IMPORT THE DATA 
library("readxl")
#data = read_excel("C:/Users/user/Downloads/infor_acc.xlsx")
data$Education <- factor(data$Education,
                         levels = c(0, 1, 2, 3, 4),
                         labels = c("No_Ed", "Adult_Ed", "Primary", "Secondary", "Tertiary"))
data$beef_cattle_herd_size <- factor (data$beef_cattle_herd_size,
                                     levels = c(1, 2, 3, 4, 5),
                                     labels = c("1_25", "26_50", "51_75", "76_100", "Above_100"))


data$Gender <- factor(data$Gender,
  levels = c(0, 1),
  labels = c("Female", "Male"))
data <- data[!is.na(data$beef_value_chain) & data$beef_value_chain != "", ]
data$beef_value_chain <- as.factor(data$beef_value_chain)                                   
# Convert back to numeric (if they were mistakenly converted to factors or characters)
data$age <- as.numeric(data$age)
data$dist_beef_mkt <- as.numeric(data$dist_beef_mkt)


install.packages("MASS")    # Only if you plan to check for overdispersion
install.packages("car")     # Optional, for VIF or diagnostics

# Load libraries
library(readxl)
library(MASS)
library(car)

# Temporarily drop beef_value_chain if it has only one level
poisson_model <- glm(total_info_sources ~ age + Gender + Education + 
                       beef_cattle_herd_size + dist_beef_mkt,
                     data = data, family = poisson())
summary(poisson_model)



