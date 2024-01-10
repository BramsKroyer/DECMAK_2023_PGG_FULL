# Loading tidyverse
library(tidyverse)

# Making the contribution tables based off of the information in the .ztt files opened in text-edit. These cond[x] columns are what col 'avg_cond' are made from, but since something has gone wrong in conversion between their code and data to osf - because it is in the trillions if not more sometimes - the avg_cond is excluded:

contribution_table <- GLMdataset %>% select(
  id,   # id
  type,         # the type they got classified to be (Freerider, CondCoop...)
  condition,    # STRAT, PREF, RAND condition
  
  # Conditions for possible group averages: 
  cond1,        # "if group avg was 0...
  cond2,        # "if group avg was 10...
  cond3,        # and so on...
  cond4,
  cond5,
  cond6,
  cond7,
  cond8,
  cond9,
  cond10,
  cond11,
  cond12,
  cond13,
  cond14,
  cond15,
  cond16,
  cond17,
  cond18,
  cond19,
  cond20,
  cond21        # Last condition: "if group avg was 200...
  
)
# Get one row per subject
contribution_table <- contribution_table[!duplicated(contribution_table$id),]

# Make id a factor
contribution_table$id <- as.factor(contribution_table$id)

# -- Make data long
contribution_table_long <- gather(contribution_table, 
                                  condition_contr_table, 
                                  preferred_contribution, cond1:cond21, 
                                  factor_key=TRUE)

# Create the named vector for mapping
values <- seq(0, 200, by = 10)
names <- paste0("cond", seq(1, 21))
mapping_vector <- setNames(values, names)
mapping_vector

# Create a new column in the dataframe
contribution_table_long <- contribution_table_long %>% 
  mutate(avg_group_contr= mapping_vector[condition_contr_table])
