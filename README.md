# bc
This is an R package used to plan corn breeding cross experiments, including create layout and calculate delays.

## Installation
```r
install.packages(c("shiny", "shinythemes", "openxlsx", "devtools"))
install_github("hwdsda/bc")
```
## Data
A sample dataset **sampleData** contains the right data input format to be used. 
 * `P1`             : The first parent. 
 * `P1_SHD`         : GDUSHD of the first parent. 
 * `P1_SLK`         : GDUSLK of the first parent. 
 * `P2`             : The second parent. 
 * `P2_SHD`         : GDUSHD of the second parent. 
 * `P2_SLK`         : GDUSLK of the second parent. 
 * `Number_of_Cross`: Number of cross need to make for those two parents. 
 
## Run the shiny app
```r
library(bc)
write.csv(sampleData, ".../sampleData.csv", row.names=F)

bCross()
```
## Background
Breeding crosses are used in corn breeding to create new populations and integrate diversity between two parents. In a realistic breeding cross experiment, there are multiple crosses need to be make. Some are sharing one of the two crossing parents. Optimizing the layout of those crosses can minimize the field and pollination resources need to be used. Furthermore, in order to ensure female silk catches male pollen, delayed planting on one of the two parents is actually performed. In summary, two tasks need two be completed:  
   * Create the field layout based on female to male ratio in a crossing block. Designation of female and male parents could be fixed or flexible.  
   * Calculate delays based on female's GDUSLK and male's GDUSHD.
   
Growing Degree Units (GDUs) are a common index used to recorde the timing for corn to shed and silk. 

![GDU](https://latex.codecogs.com/gif.latex?\textup{GDU}&space;=&space;\frac{\textup{Daily&space;Max&space;Air&space;Temperature}&plus;&space;\textup{Daily&space;Min&space;Temperature}}{2}&space;-&space;50)      
   * when maximum air temperature > 86^o^F, set it at 86^o^F;      
   * when minimum air temperature < 50^o^F, set it at 50^o^F.
