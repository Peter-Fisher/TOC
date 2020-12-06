

# TOC curve R Package 0.0.5

* [**TOC curve introduction**](#TOC curve introduction)
* [**Package download and installation**](#Package download and installation)
* [**Improvement**](#Improvement from  [TOC_0.0-5](https://cran.r-project.org/web/packages/TOC/index.html))
* [**Package usage**](#Package usage)
* [**Examples**](#Examples)
* [**References**](#References)

## TOC curve introduction

The **total operating characteristic (TOC)** is a statistical method to compare a Boolean variable versus a rank variable. It is a modified model of **receiver operating characteristic (ROC)** that shows more quantitative information.

TOC can measure the ability of an index variable to diagnose either presence or absence of a characteristic. The diagnosis of presence or absence depends on whether the value of the index is above a threshold. TOC considers multiple possible thresholds. Each threshold generates a two-by-two [contingency table](https://en.wikipedia.org/wiki/Contingency_table), which contains four entries: hits, misses, false alarms, and correct rejections.[[1\]](https://doi.org/10.1080%2F13658816.2013.862623)

The [receiver operating characteristic](https://en.wikipedia.org/wiki/Receiver_operating_characteristic) (ROC) also characterizes diagnostic ability. For each threshold, ROC reveals two ratios, hits/(hits + misses) and false alarms/(false alarms + correct rejections), while TOC shows the total information in the contingency table for each threshold.[[2\]](https://doi.org/10.1007%2Fs10980-013-9984-8) 

TOC is applicable to measure diagnostic ability in many fields including but not limited to: land change science, [medical imaging](https://en.wikipedia.org/wiki/Medical_imaging), [weather forecasting](https://en.wikipedia.org/wiki/Weather_forecasting), [remote sensing](https://en.wikipedia.org/wiki/Remote_sensing), and [materials testing](https://en.wikipedia.org/wiki/Materials_testing).

Wikipedia Link: https://en.wikipedia.org/wiki/Total_operating_characteristic#cite_note-Si-1

## Package download and installation

### Install package from GitHub

Install `devtools`package:

```R
install.packages('devtools')
```

Import `devtools` package:

```R
library('devtools')
```

Install `TOC`package from GitHub:

```R
install_github("Peter-Fisher/TOC")
```

Import `TOC` package:

```
library('TOC')
```

### Install package from local

Download `TOC_0.0.5.tar.gz` from GitHub

Install package from local download:

```R
install.packages('download_path/TOC_0.0.5.tar.gz', repos = NULL, type="source")
```

Import TOC package:

```R
library('TOC')
```

## Improvement from  [TOC_0.0-5](https://cran.r-project.org/web/packages/TOC/index.html)

- Labels of thresholds are now in the right place. 
- Give an option for users to sort the thresholds upward or downward.
- Input parameter `thres` can modify threshold.
- Allow users to plot multiple curves into the same image and easily compare.

## Package usage

### New Input parameters for TOC usage:

| Parameters | Options                                       | Descriptions                                                 |
| ---------- | --------------------------------------------- | ------------------------------------------------------------ |
| nthres     | numeric (default is NULL)                     | An optional integer indicating the number of equal-interval thresholds to be evaluated for the TOC curve. See Details below |
| thres      | vector (default is NULL)                      | An optional numeric vector of thresholds to be evaluated for the TOC curve. See Details below |
| Sort       | ‘DECREASE’ ‘INCREASE’ (default is ‘DECREASE’) | Character string indicating whether thresholds sort 'Decrease' or 'Increase' |
| Thresorder | TRUE FALSE (default is TRUE)                  | Character string indicating whether variables have clear order |
| Ordinal    | TRUE FALSE (default is FALSE)                 | Character string indicating whether variables are ordinal    |
| FirstThres | numeric (default is NULL)                     | Character string indicating whether user want an specified first threshold |
| LastThres  | numeric (default is NULL)                     | Character string indicating whether user want an specified last threshold |
| Increment  | numeric (default is NULL)                     | Character string indicating whether user want to specify the increment of thresholds |

### New Input parameters for TOC curve plot:

| Parameters | Options                       | Descriptions                                                 |
| ---------- | ----------------------------- | ------------------------------------------------------------ |
| addAUC     | TRUE FALSE (default is TRUE)  | An option to show whether to show the AUC on the plot        |
| digitsAUC  | numeric (default is 2)        | To specified the number of digits when showing the AUC       |
| addCC      | TRUE FALSE (default is FALSE) | An option to show whether to show the Correct Corner on the plot |
| AUClableX  | numeric (0-1, default is 0.6) | An option to specified the X Position of AUC labels          |
| AUClableY  | numeric (0-1, default is 0.1) | An option to specified the Y Position of AUC labels          |



## Examples

```R
rocd <- ROC(index1, boolean, mask, NAval=0, FirstThres = 10000, LastThres = 50000, Increment = 5000, sort='DECREASE')
plot(rocd, cex=0.8, posL=4)
```

<img src="imgs\ROC_Example1.png" alt="image-20201205221843174" style="zoom: 80%;" />

```R
tocd <- TOC(Prob_Map2, Change_Map2b, MASK4, NAval=0, FirstThres = 10000, LastThres = 50000, Increment = 5000, sort='DECREASE')
plot(tocd, cex=0.8, posL=4, addCC = TRUE, labelThres = TRUE, digitsAUC = 4)
```

<img src="imgs\TOC_Example1.png" alt="TOC_Example1" style="zoom: 80%;" />

```R
plot(c(tocd1,tocd2), cex=0.8, posL=4,addCC = TRUE, digitsAUC=4)
```

<img src="imgs\TOC_MultiPlot.png" alt="TOC_MultiPlot" style="zoom:80%;" />

## References

[1] Pontius, Robert Gilmore; Si, Kangping (2014). "The total operating characteristic to measure diagnostic ability for multiple thresholds". *International Journal of Geographical Information Science*. **28** (3): 570–583. [doi](https://en.wikipedia.org/wiki/Doi_(identifier)):[10.1080/13658816.2013.862623](https://doi.org/10.1080%2F13658816.2013.862623).

[2]  Pontius Jr, Robert Gilmore; Parmentier, Benoit (2014). "Recommendations for using the Relative Operating Characteristic (ROC)". *Landscape Ecology*. **29** (3): 367–382. [doi](https://en.wikipedia.org/wiki/Doi_(identifier)):[10.1007/s10980-013-9984-8](https://doi.org/10.1007%2Fs10980-013-9984-8)