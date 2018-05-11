# bc
This is an R package used to plan corn breeding cross experiments, including layout creation and delays calculation.

## Installation
```r
install.packages(c("shiny", "shinythemes", "openxlsx", "devtools"))
library(devtools)
install_github("hwdsda/bc")
```
## Data
A sample dataset **sampleData** contains the right data input format used in this R shiny app.

* `P1`: Name of the first parent. 
* `P1_SHD`: Accumulated GDUs required to shed pollen for the first parent. 
* `P1_SLK`: Accumulated GDUs required to develop silk for the first parent. 
* `P2`: Name of the second parent. 
* `P2_SHD`: Accumulated GDUs required to shed pollen for the first parent. 
* `P2_SLK`: Accumulated GDUs required to develop silk for the first parent. 
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

Growing Degree Units (GDUs) are used to measure corn growth stages relative to temperature.   
`GDUSLK`: Accumulated Growing Degree Units (GDUs) required for corn to develop silk.
`GDUSHD`: Accumulated Growing Degree Units (GDUs) required for corn to shed pollen.

![GDU](https://latex.codecogs.com/gif.latex?\textup{GDU}&space;=&space;\frac{\textup{Daily&space;Max&space;Air&space;Temperature}&plus;&space;\textup{Daily&space;Min&space;Temperature}}{2}&space;-&space;50)      
   * when air temperature > 86 ^o^F, set it at 86 ^o^F;      
   * when air temperature < 50 ^o^F, set it at 50 ^o^F.
