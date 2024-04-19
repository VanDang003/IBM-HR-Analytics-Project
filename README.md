# Team-50

## Analysis of Factors that Affect Employee Attrition Rates
Employee attrition leads to various issues, including hidden costs like burnout and lost industry knowledge, as well as measurable expenses such as lost productivity and recruitment costs. Studies indicate that replacing an employee can incur a cost of three to four times their salary, or even higher. To minimize these expenses, companies need to recognize factors like commute times, salary, work-life balance, and job satisfaction that contribute to attrition. By understanding and addressing these factors, companies can develop strategies to retain employees longer, reducing overall personnel costs.

<p align="center">
  <img src="attrition.jpeg" alt="Empty Office" title="Empty Office" style="text-align:center" width="400px">
  <br>
  <em>An empty office when more people leave</em>
</p>

## Dataset Source
> The repo has the file in the Data folder

IBM HR Analytics Employee Attrition & Performance &rarr; Source:[here](https://www.kaggle.com/datasets/pavansubhasht/ibm-hr-analytics-attrition-dataset "Kaggle")

## Guide to Files
* [Exploratory Data File](Code/Exploratory%20Data%20Analysis/EDA%20Notebook.ipynb) | Analysis into the datasets and variable interactions
* Initial Model for Progress Report - Random Forest, GLM
* Age Group Focused Modelling - Post Progress Report 
  * Runs were conducted using different age ranges, but the deviations from our initial model were found to be minimal.
* Category Group Focused Modelling - Post Progress Report
  * A regression was performed on the variables based on their category groupings. However, these models did not yield satisfactory predictive results.
* Cross Fold Focused Modelling - Post Progress Report
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


