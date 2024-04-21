# Team-50

## Analysis of Factors that Affect Employee Attrition Rates
Employee attrition leads to various issues, including hidden costs like burnout and lost industry knowledge, as well as measurable expenses such as lost productivity and recruitment costs. Studies indicate that replacing an employee can incur a cost of three to four times their salary, or even higher. To minimize these expenses, companies need to recognize factors like commute times, salary, work-life balance, and job satisfaction that contribute to attrition. By understanding and addressing these factors, companies can develop strategies to retain employees longer, reducing overall personnel costs.

<p align="center">
  <img src="attrition.jpeg" alt="Empty Office" title="Empty Office" style="text-align:center" width="400px">
  <br>
  <em>An empty office with high attrition</em>
</p>

## Dataset Source
> The repo has the file in the Data folder

IBM HR Analytics Employee Attrition & Performance &rarr; Source:[here](https://www.kaggle.com/datasets/pavansubhasht/ibm-hr-analytics-attrition-dataset "Kaggle")

## Guide to Files

### Reports
* [Progress Report](Progress%20Report/team50progressreport.docx)
* [Final Report](Final%20Report/team50finalreport.docx)

### R Codes Walkthrough
* [Exploratory Data File](Code/Exploratory%20Data%20Analysis/EDA%20Notebook.ipynb) | Analysis into the datasets and variable interactions
* [Initial Model (GLM) for Progress Report](Progress%20Report/Attrition_Glm_Interaction_Ben_09032024.R)
* [Initial Model (Random Forest) for Progress Report](Progress%20Report/RandomForestModel.R)
* [Age Group Focused Modelling](Final%20Report/Attrition_Glm_Interaction_Ben_Age_Group_07042024.R) - For Finals
  * Runs were conducted using different age ranges, but the deviations from our initial model were found to be minimal.
* [Category Group Focused Modelling](Final%20Report/Attrition_Glm_Interaction_Ben_Category_Models_07042024.R) - For Finals
  * A regression was performed on the variables based on their category groupings. However, these models did not yield satisfactory predictive results.
* [Cross Fold Focused Modelling](Final%20Report/Attrition_Glm_Interaction_Ben_K-Fold_CV_10042024.R) - For Finals
  * K-fold cross-validation was implemented to enhance the model's performance. This process involved the removal of several variables with lower scores to refine the model's predictive capabilities.  


## How to run files

### Packages Required

| Package   | Description                   |
|-----------|-------------------------------|
| dplyr     | Data manipulation             |
| ggplot2   | Data visualization            |
| cowplot   | Combine ggplots               |
| corrplot  | Correlation plots             |
| car       | Companion to applied regression |
| caret     | Classification and Regression Training |

```R
# Install required packages if not already installed
if (!requireNamespace("package_name", quietly = TRUE)) {
  install.packages("package_name")
}
```


