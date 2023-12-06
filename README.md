# biomass-modeling

In this project, I am modeling aboveground forest biomass by integrating field data with
Sentinel-2 bands and vegetation indices.

Here, we extracted pixel values of all bands and vegetation indices with radius of 12.61m that aligns with our 
plot area in the field.

Then

we run VSURF package to select the most important variables for our model
We try random forest regression and support vector regression in the study from caret package.



