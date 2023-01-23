# StatsBomb Analysis (FIFA World Cup 2022)
## Purpose

The purpose of this project is to further understand the numbers behind the 2022 World Cup as a whole and for individual teams so we can better understand the playing styles and how effective it is against other styles.

The analysis is done within the R programming language. 

## Data Sources

![img](https://dtai.cs.kuleuven.be/sports/static/ee39fa2918398059e9be62c32c1b48c4/74404/statsbomb_logo.png)

*Of course we collect data. Better data than anyone else, in fact. It’s collected to a more detailed spec and with more care, consideration and accuracy than anyone else. It has been crafted by analysts for analysts and has transformed the understanding of sport.*

*But that is just the start of how StatsBomb adds value. We also bring our data to life in the most accessible and engaging way with the authoritative analytical tools we’ve built for our customers. We call these tools IQ and they are available for both soccer and football.*

*Together, our data and IQ platforms allow soccer and football teams to get an edge on the competition in terms of player recruitment and scouting, performance analysis and opposition evaluation. We help our customers leave nothing to chance. We are the data champions.* 

~ StatsBomb Website

**To Learn More About StatsBomb, Head To:** https://statsbomb.com/

## Installation Of StatsBombR In RStudio

StatBomb's former data scientist Derrick Yam created StatsBombR, an R package dedicated making using of StatsBomb data in R much easier. It can be found on Github at the following link, along with much more information on its uses. There are lots of helful functions within it that you should get to know.

https://github.com/statsbomb/StatsBombR

1. To install the package in R, you'll need to install the "devtools" package, which can be done by running the following line of code:

   ```R
   install.packages("devtools")
   install.packages("remotes")
   remotes:install_version("SSDMTools", "1.1-221")
   ```

2. Then, to install StatsBombR itself, run:

   ```R
   devtools:install_github("statsbomb/StatsBombR")
   library(StatsBombR)
   ```

   

