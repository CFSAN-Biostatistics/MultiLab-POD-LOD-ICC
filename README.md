# MultiLab-POD-LOD-ICC

## **Introduction**

---

This is a Shiny app for interlaboratory microbiological method validation studies.


### **General information**

---

This app implements the random intercept complementary log-log model suggested by Jarvis et al. (2019) to estimate probability of detection (POD) and level of detection (LOD) from a multi-laboratory validation study for a qualitative (binary) microbiological assay. This app also calculates the intraclass correlation coefficient (ICC) to estimate the proportion of total variance attributable to between-laboratory variance.

This app is intended to be an alternative to the tool available at:
<https://www.wiwiss.fu-berlin.de/fachbereich/vwl/iso/ehemalige/wilrich>
  
<ins>Reference</ins><br>
Jarvis B, Wilrich C, Wilrich P-T. Estimation of the POD Function and the LOD of a Binary Microbiological Measurement Method from an Interlaboratory Experiment. *Journal of AOAC International*. 2019; 102(5):1617-1623.


### **Software needed**

---

R version 3.5 or higher is needed. The following R packages (available on CRAN) are also needed:

- shiny (>= v1.5.0)
- shinydashboard (>= v0.7.1)
- shinyjs (>= v1.1)
- shinyWidgets (>= v0.5.3)
- shinyalert (>= v1.1)
- shinybusy (>= v0.2.0)
- lme4 (>= v1.1-23)
- performance (>= v0.4.8)
- ggplot2 (>= v3.3.2)
- openxlsx (>= 4.1.5)
- sessioninfo (>= 1.1.1)
- aod (>= v1.3.1)


### **Running the code**

---

To run this app, place all the files into your R working directory. At the R prompt, type:
`shiny::runApp()`


---

### *Notes*

* This app runs in Firefox and Chrome.

---
