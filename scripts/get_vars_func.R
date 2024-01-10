get_vars <- function (df){
  # --------------------- GET VARS# --------------------- 
  # Get the variables needed for modelling: 
  # - ngroups
  # - ntrials
  # - groupSize
  # - Ga (avg contribution in the group without yourself)
  # - c (own contribution)
  
  # ---- ngroups
  ngroups <- length(unique(df$idgroup)) # 64
  
  # ---- ntrials
  ntrials <- length(unique(df$period)) # 15
  
  # ---- groupSize
  groupSize <- 3
  
  # ---- Ga
  Ga <- array(0, c(groupSize, ntrials, ngroups))
  
  # The mapping for idgroups (assuming idgroup values are not sequential starting from 1)
  unique_idgroups <- sort(unique(df$idgroup))
  idgroup_mapping <- setNames(seq_along(unique_idgroups), unique_idgroups)
  
  for (i in 1:nrow(df)) {
    
    # Map the idgroup to a sequential index
    g <- idgroup_mapping[as.character(df$idgroup[i])]
    t <- df$period[i]
    s <- df$group_subjectid[i]  # Assuming this ranges from 1 to groupSize
    
    # Check for NA values and ensure indices are within bounds
    if (!is.na(s) && !is.na(t) && !is.na(g) && s >= 1 && s <= groupSize && t >= 1 && t <= ntrials && g >= 1 && g <= ngroups) {
      Ga[s, t, g] <- df$avg_contr_g_others[i]
    } else {
      warning(sprintf("Index out of bounds at row %d: s = %d, t = %d, g = %d\n", 
                      i, s, t, g))
    }
  }
  
  # ---- c
  c <- array(0, c(groupSize, ntrials, ngroups))
  
  # Create mappings for ids and idgroups to sequential indices
  unique_ids <- sort(unique(df$group_subjectid))
  unique_idgroups <- sort(unique(df$idgroup))
  
  for (i in 1:nrow(df)) {
    # Check for NA values
    if (is.na(df$group_subjectid[i]) || is.na(df$period[i]) || is.na(df$idgroup[i])) {
      next  # Skip this iteration if there's an NA
    }
    
    # Map the ids and idgroups to sequential indices
    s <- match(df$group_subjectid[i], unique_ids)
    t <- df$period[i]
    g <- match(df$idgroup[i], unique_idgroups)
    
    # Now perform the check and assignment
    if(s > 0 && s <= groupSize && t > 0 && t <= ntrials && g > 0 && g <= ngroups) {
      c[s, t, g] <- df$contribution_id[i]
    } else {
      warning(sprintf("Index out of bounds at row %d: s = %d, t = %d, g = %d\n", 
                      i, s, t, g))
    }
  }
  my_vars <- list("ngroups" = ngroups, "ntrials" = ntrials, "groupSize" = groupSize,"Ga" = Ga, "c" = c)
  return(my_vars)
}