# biomass-modeling

In this project, aboveground forest biomass and carbon were estimated by integrating field data with Sentinel-2 bands and vegetation indices. 
Here, pixel values of all bands and vegetation indices were calculated and extracted with radius of 12.61m that aligns with the
plot area in the field. 
Then VSURF package was run to select the most important variables for our model.
Random forest regression and support vector regression from caret package were used.
And finally, Aboveground forest biomass and Carbon were predicted.



