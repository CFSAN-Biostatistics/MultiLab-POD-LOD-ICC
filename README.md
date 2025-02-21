---

## **Introduction (v1.12.0)**

This is a Shiny app for interlaboratory microbiological method validation studies. Please <ins>[visit the deployed app](https://pub-connect.foodsafetyrisk.org/method-validation/multilab-lod/)</ins> to see it in action.

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

R version 4.2.3 or higher is needed. The following R packages (available on CRAN) are also needed:

- shiny (v1.8.1.1)
- shinydashboard (v0.7.2)
- shinyjs (v2.1.0)
- shinybusy (v0.3.3)
- shinyvalidate (v0.1.3)
- Matrix (v1.6-3)
- lme4 (v1.1-35.3)
- ggplot2 (v3.5.1)
- openxlsx (v4.2.5.2)
- sessioninfo (v1.2.2)
- dplyr (v1.1.4)
- tidyr (v1.3.1)

---

### **Running the code**

To run this app, place all the files into your R working directory. At the R prompt, type:
`shiny::runApp()`

---

#### *Notes*

* This app runs in Microsoft Edge, Mozilla Firefox, and Google Chrome browsers.

---
