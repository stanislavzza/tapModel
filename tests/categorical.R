library(tidyverse)
library(tapModel)

cat_ratings <- generate_sample_ratings_cat()

fleiss_kappa_cat(cat_ratings)
e_m_step_cat(cat_ratings, TRUE)

cat_ratings <- fit_ratings_cat(cat_ratings)
avg_params_cat(cat_ratings)

cat_ratings <- as_cat_ratings(wine)

fit_counts_cat(cat_ratings)
