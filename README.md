# Benchmarking of ROC vs. AUC

Richardson et al. (2024) benchmarked ROC vs. AUC by sampling with class
imbalance from defined score distributions, which is unrealistic.

This benchmarking analysis evaluates whether the score distributions
change under different class imbalance levels, as well as the characteristics
of ROC and AUC at various class imbalances at the level of the raw data.

## Results

- Score distributions change under different class imbalances in the data
- AUC-ROC remains stable across class imbalances
- AUC-PRC decreases with lower prevalence because the precision converges
  to the prevalence in the PRC curve

