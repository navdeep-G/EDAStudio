# A Shiny Application for Exploring Data

- `r-eda` allows a user to quickly explore data on the fly (.csv files) by utilizing the power of ggplot2
- You can do the following:
  - scatter plots
  - dotplots
  - boxplots
  - histograms
  - densities

# CSV Data Input

 - Essentially utilizes `read.csv` -> `read.csv("yourdata.csv",na.strings = c("NA","."))`

#Data Manipulations

 - Change continuous variable to be treated as categorical
 - Change continuous variable to categories with a specified number of bins or by supplying values for the bins start/end
 - Up to six sequential filters for categorical and continuous variables
 - Renaming and reordering of the levels of a categorical variable
 - Combining two categorical variables into one
 - Rounding a numerical variable to a specified number of digits
 
# ggplot2 functionality (facet_grid and facet_wrap)

 - Group, color, size, fill mappings
 - Controlling y and x axis labels, legends and other commonly used theme options.


