---

## **Introduction (v1.9.0)**

This is a Shiny app for interlaboratory microbiological method validation studies. Please <ins>[visit the deployed app](https://multi-lab.galaxytrakr.org/)</ins> to see it in action.

---

### **General information**

This app implements the random intercept complementary log-log model suggested by Jarvis et al. (2019) to estimate probability of detection (POD) and level of detection (LOD) from a multi-laboratory validation study for a qualitative (binary) microbiological assay. This app also calculates the intra-laboratory correlation coefficient (ICC) to estimate the proportion of total variance attributable to between-laboratory variance.

This app is intended to be an alternative to the tool discussed in Jarvis et al. (2019).

---

### **Citation**

To cite this app in a publication, please use:

- Wang SS, Ihrie J (2021). On the estimation of POD and LOD of qualitative microbiological assays from a multi-laboratory validation study. *Journal of AOAC International*, https://doi.org/10.1093/jaoacint/qsab130

### **References**

- Jarvis B, Wilrich C, Wilrich P-T. Estimation of the POD Function and the LOD of a Binary Microbiological Measurement Method from an Interlaboratory Experiment. *Journal of AOAC International*. 2019; 102(5):1617-1623.

---

### **Software needed**

R version 4.3.1 or higher is needed. The following R packages (available on CRAN) are also needed:

- shiny (>= v1.7.4.1)
- shinydashboard (>= v0.7.2)
- shinyjs (>= v2.1.0)
- shinybusy (>= v0.3.2)
- shinyvalidate (>= v0.1.3)
- lme4 (>= v1.1-35.1)
- ggplot2 (>= v3.4.4)
- openxlsx (>= 4.2.5.2)
- sessioninfo (>= 1.2.2)
- dplyr (>= 1.1.2)
- tidyr (>= 1.3.0)

---

### **Running the code**

To run this app, place all the files into your R working directory. At the R prompt, type:
`shiny::runApp()`

---

#### *Notes*

* This app runs in Microsoft Edge, Mozilla Firefox, and Google Chrome browsers.

---
