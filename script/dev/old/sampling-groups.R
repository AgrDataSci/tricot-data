# Load necessary library
library(dplyr)


participants = read.csv("output/avisa-adcin/names.csv")
# # Sample dataset of participants
# set.seed(42)  # For reproducibility
# participants = data.frame(
#   ID = 1:15,
#   Expertise = c("Seed System", "Breeder", "CGIAR", "Data Scientist", "Tricot Implementer",
#                 "Seed System", "Breeder", "CGIAR", "Data Scientist", "Tricot Implementer",
#                 "Seed System", "Breeder", "CGIAR", "Data Scientist", "Tricot Implementer")
# )

# Check the dataset
print("Original Participants Data")
print(participants)

# Function to split participants into balanced groups
split_into_groups = function(data, num_groups) {
  data %>%
    group_by(Expertise) %>%
    mutate(Group = sample(rep(1:num_groups, length.out = n()))) %>%
    ungroup()
}

# Split into 3 balanced groups
balanced_groups = split_into_groups(participants, 3)

# Display the result
balanced_groups = 
  balanced_groups 

balanced_groups$Group = LETTERS[balanced_groups$Group]

print("Balanced Groups:")
print(balanced_groups)

groups = vector()
for(i in seq_along(1:6)) {
  groups = cbind(groups, LETTERS[split_into_groups(participants, 3)$Group])
}

groups = data.frame(groups)

# Save the result to a CSV file (optional)
write.csv(groups, "output/avisa-adcin/balanced_groups.csv", row.names = FALSE)
