Created preliminary linear model that makes a very simplified visualization of data, that will be replaced by a more advanced mixed model.

We could calculate PC1 and then divide all other measurements by PC1
Alternatively, we could just use the thorax measurement itself as the divisor

Calculated principle components analysis for male and female trait data

Looked at loadings to see what component contributed most to observed variation

It looks like PC1 (size) contributes to the large majority of the variation, with very little actually being attributed to PC2/3/4, so we will use absolute trait size and PC1 as a response.

Predictors will be sex, condition, and interactions.

We will figure out what the random effects and fit them into model as well.