Our data set (`MP_SpeciesStarvation_Clean.csv` - located in our main QMEE directory) contains foreleg (tarsus and tibia length and width), thorax length and wing area measurements taken from male and female *Drosophila* following a developmental nutrition manipulation experiment (Pesevski 2021). The nutrition manipulation experiment involved 27 species of the *D. melanogaster* species group, that were exposed to increasing periods of starvation during larval development. The primary objective of this experiment was to study the relationship between sexual size dimorphism and condition (i.e., the resources available to the flies) within and across the phylogeny. It was hypothesized that traits and species exhibiting a greater degree of sexual dimorphism should also exhibit a greater degree of condition dependence.

Examining the evolution of sexual size dimorphism and condition dependence across the phylogeny showed that *Drosophila prolongata* exhibits an interesting relationship between condition and size dimorphism. Despite exhibiting very strong sexual dimorphism in both the tibia and tarsus size of the forelegs, these traits were only moderately condition dependent. This contrasts with the rest of the phylogeny, which exhibit both moderate sexual dimorphism and condition dependence in the foreleg traits. 

We will first test the hypothesis that starvation during larval development will decrease the extent of sexual size dimorphism in the forelegs of adult *D. prolongata*. To test this hypothesis, we plan to test the effect of condition (i.e., categorical predictor variable: ‘condition’, with levels: high condition and low condition) on the difference in the sex-specific mean of relative size of each foreleg trait (i.e., tibia length, tibia width, tarsus length, tarsus width). Our measure of relative trait size will be represented by the slope of the line representing the allometric relationship between each foreleg trait and thorax length (as a proxy for body size). Because the starvation experiment was completed in different blocks, we will incorporate block number into our model as a random effect.  We will use a linear mixed model for this analysis.

**BMB**: it's a little confusing to think of an allometric slope as a "relative trait size".  Wouldn't we instead want to think of something like a trait size once PCA1 (size) was factored out of an equation? (e.g. as in McCoy et al 2006)

McCoy, Michael W., Benjamin M. Bolker, Craig W. Osenberg, Benjamin G. Miner, and James R. Vonesh. 2006. “Size Correction: Comparing Morphological Traits among Populations and Environments.” Oecologia 148: 547–54. https://doi.org/10.1007/s00442-006-0403-6.

It would be good to write out the exact model you mean to fit to help me think about it ...

Also, at some point it would be good to consider a multivariate model where you fit the effects on all traits simultaneously (and allow for correlations among them), rather than one at a time.

---

To test the hypothesis that traits that are more sexually dimorphic are also more condition dependent, we will examine how the relationship between condition dependence and sexual size dimorphism in the forelegs differs from that observed in a trait that is less sexually dimorphic within the species. To this end, we will use a linear mixed model to model the effect of the interaction between condition and sex on relative tibia and tarsus length and width, as well as relative wing size – which will act as a within-species control. Relative wing size will be taken as the slope of the line representing the allometric relationship between wing area and thorax length. Block number will be added to our model as a random effect variable. 

**BMB**: see comments above about allometric coefficients as "relative size". Also: when you say "block number will be added as a random effect", it's clear that the block number is a *grouping variable*.  What will be allowed to vary among blocks? (Intercept only, or other traits/interactions as well?)

Pesevski, M. (2021). Influence of environmental variation on sexual dimorphism in Drosophila morphology among adaptively diverged populations and in an inter-specific comparative context .


***Where everything is located***

The original data set "MP_SpeciesStarvation_Clean.csv" is located in the main Dpro_CD_SD repository
