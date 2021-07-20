---

## **Introduction**

This is a Shiny app for interlaboratory microbiological method validation studies. Please <ins>[visit the deployed app](https://multi-lab.galaxytrakr.org/)</ins> to see it in action.

---

### **General information**

This app implements the random intercept complementary log-log model suggested by Jarvis et al. (2019) to estimate probability of detection (POD) and level of detection (LOD) from a multi-laboratory validation study for a qualitative (binary) microbiological assay. This app also calculates the intra-laboratory correlation coefficient (ICC) to estimate the proportion of total variance attributable to between-laboratory variance.

This app is intended to be an alternative to the tool discussed in Jarvis et al. (2019).

---

### **References**

- Jarvis B, Wilrich C, Wilrich P-T. Estimation of the POD Function and the LOD of a Binary Microbiological Measurement Method from an Interlaboratory Experiment. *Journal of AOAC International*. 2019; 102(5):1617-1623.

---

### **Software needed**

R version 4.0.5 or higher is needed. The following R packages (available on CRAN) are also needed:

- shiny (>= v1.6.0)
- shinydashboard (>= v0.7.1)
- shinyjs (>= v2.0.0)
- shinyWidgets (>= v0.6.0)
- shinyalert (>= v2.0.0)
- shinybusy (>= v0.2.2)
- lme4 (>= v1.1-27)
- ggplot2 (>= v3.3.5)
- openxlsx (>= 4.2.4)
- sessioninfo (>= 1.1.1)
- dplyr (>= 1.0.7)
- tidyr (>= 1.1.3)

---

### **Running the code**

To run this app, place all the files into your R working directory. At the R prompt, type:
`shiny::runApp()`

---

#### *Notes*

* This app runs in Mozilla Firefox and Google Chrome.

---
