Created preliminary linear model that makes a very simplified visualization of data, that will be replaced by a more advanced mixed model.

We could calculate PC1 and then divide all other measurements by PC1
Alternatively, we could just use the thorax measurement itself as the divisor

Calculated principal components analysis for male and female trait data

Looked at loadings to see what component contributed most to observed variation

It looks like PC1 (size) contributes to the large majority of the variation, with very little actually being attributed to PC2/3/4, so we will use absolute trait size and PC1 as a response. JD: Why not just use PC1?

Predictors will be sex, condition, and interactions.

We will figure out what the random effects and fit them into model as well.

JD: If you've moved on from the ideas in the README, please update the README (you could put older stuff, clearly marked, somewhere in the journal if you like)


Updates as of Apr 1 

Used lm to model the interaction between condition and sex on individual trait size (thorax length and width, tarsus length). The biological question we are trying to answer with these models: How does starvation change the difference in trait size across species (i.e., sexual dimorphism in trait size) We are interested in looking at variation in sexual dimorphism in leg traits that are independent of changes in body size (i.e., relative trait size) due to nutritional manipulation. To try to parse the changes in relative trait size independent of changes in body size, we are including thorax length (as an estimate of body size) as a predictor variable . We are thus using lm to model the effect of the interaction between log2-thorax length, sex and condition and log2- transformed individual leg traits (i.e., log2 tibia length, width, tarsus length). 

Using lm() to model the three way interaction between thorax length, sex, and condition: lm(leg_log_tibL ~ thorax_log_length_mm * sex * condition, data = Dprol_size) 
the intercept is the log2 (tibia length) for high condition females when thorax length is 0 - this does not make sense biologically. It would be best to mean center thorax length, such that the intercept of our new linear model is estimated mean log2(tibia length) high condition females at mean body thorax size. The three way interaction coefficient will be The difference between sexes at high vs low condition for the slope of the relationship between log2(tibia length) and log2(thorax length). 

PCA using variables: tibia length and width, tarsus, and thorax length across both sexes and condition levels. PC1 describes variation in total size and accounts for ~93% of the variation. PC2 accounts for 7% and describes variation due to sex. Code can be found in 'models.R' script. Our plan is to model the effect of the interaction between sex and condition on PC1, as our measure of total size of the flies. The biological question we are answering here is:How does total sexual size dimorphism change (I.e., the size difference between males and females) as a result of. Note that we have not included wing area in this estimate. This decision was made because our focus is on the foreleg traits and how this varies with thorax size (i.e., body size). 

Given that size (PC1) accounts for most (~93%) of the variation in our trait measurements, it might a better idea to evaluate the effect of the interaction between sex and condition on PC1 as a measurement of total size using a linear model. As opposed to trying to look at changes in the slope of individual traitss vs thorax size across sexes and condition levels. 

I would like to incorporate a random intercept into the linear model to account for the variation between flies in each cohort level. 



