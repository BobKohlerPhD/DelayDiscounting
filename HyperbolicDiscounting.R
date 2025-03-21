# Get unique IDs
unique_ids <- unique(discountingdata$src_subject_id)

# Calculate k for each ID  - Dataframe must have the indifference score with associated delay
kvalue_list <- lapply(unique_ids, function(id) {
  unname(coef(nls(indifference ~ 100 / (1 + k * delay_revalue), #updated as needed depending on discounting methods
                  data = discountingdata,
                  subset = src_subject_id == id,
                  start = list(k = 0.1), #updated as needed 
                  control = list(maxiter = 50000)))) #update as needed 
})

names(kvalue_list) <- unique_ids

# Unlist to vector and combine IDs with K value 
k_vector <- unlist(kvalue_list)
all_k <- data.frame(src_subject_id = names(k_vector), k = k_vector)

