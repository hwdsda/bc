# bc
This is an R package used to plan corn breeding cross experiments, including create layout and calculate delays.

## Installation
```r
install.packages(c("shiny", "shinythemes", "openxlsx", "devtools"))
library(devtools)
install_github("hwdsda/bc")
```
## Data
A sample dataset **sampleData** contains the right data input format used in this R shiny app.

* `P1`: Name of the first parent. 
* `P1_SHD`: GDUs needed to shed for the first parent. 
* `P1_SLK`: GDUs needed to silk for the first parent. 
* `P2`: Name of the second parent. 
* `P2_SHD`: GDUs needed to shed for the second parent. 
* `P2_SLK`: GDUs needed to silk for the second parent. 
* `Number_of_Cross`: Number of cross need to be made between those two parents. 
 
## Run the shiny app
```r
library(bc)
write.csv(sampleData, "sampleData.csv", row.names=F)

bCross()
```
## Background
Breeding crosses are used in corn breeding to create new populations and integrate diversity between two parents. In a realistic breeding cross experiment, there are multiple crosses need to be made. Some are sharing one of the two crossing parents. Optimizing the field layout of those crosses can minimize the field and pollination resources. Furthermore, in order to ensure female silk catches male pollen, delayed planting on one of the two parents is actually performed. In summary, two tasks need to be completed:  
   * Create the field layout based on female to male ratio in a crossing block. Designation of female and male parents could be fixed or flexible.  
   * Calculate delays based on female's GDUSLK and male's GDUSHD.
   
Cumulative Growing Degree Units (GDUs) is a common index used to record the timing for corn to shed and silk. 

![GDU](https://latex.codecogs.com/gif.latex?\textup{GDU}&space;=&space;\frac{\textup{Daily&space;Max&space;Air&space;Temperature}&plus;&space;\textup{Daily&space;Min&space;Temperature}}{2}&space;-&space;50)      
   * when maximum air temperature > 86 F, set it at 86 F;      
   * when minimum air temperature < 50 F, set it at 50 F.
