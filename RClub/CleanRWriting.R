#Comments w/
#Title
#Author
#Last Edited

#ctrl, shift, R to build sections


# Section 1 ---------------------------------------------------------------

#rm(list = ls()) CLEARS GLOBAL ENVIRONMENT  

require(gapminder)
require(dplyr)

select(gapminder, country, year)