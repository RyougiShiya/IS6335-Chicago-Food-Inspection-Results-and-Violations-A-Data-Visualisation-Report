CHICAGO FOOD INSPECTION RESULTS AND VIOLATIONS
Source code and data submission

CONTENTS
Individual_Project.twbx  Original packaged Tableau workbook, including its Hyper extract.(Not in this repository due to file size)
Individual_Project.R     R analysis and visualisation script with English comments.
Individual_Project.ipynb Python cleaning and analysis notebook with English comments.
Individual_Project.py    Equivalent Python script exported from the notebook.
data/                    Original CSV, cleaned Excel workbook and ZIP boundaries. (Not in this repository due to file size, but could get URL through individual project report)
requirements.txt         Python packages used by the notebook.

HOW TO USE
First, download raw data through URL in individual project report.
Open a terminal in this extracted folder, install the Python dependencies with
  python -m pip install -r requirements.txt
and launch Jupyter from this folder. Open Individual_Project.ipynb and run cells
in order. The first four cells clean the raw CSV and overwrite the bundled
cleaned workbook. To use the supplied cleaned data without regenerating it,
run the imports cell, then continue from the 'Load the cleaned data' section.
The .py file is an alternative to the notebook; running both is unnecessary.

For R, set the working directory to this extracted folder. Install packages:
  install.packages(c('readxl', 'dplyr', 'tidyr', 'ggplot2', 'scales', 'tibble'))
Open Individual_Project.R and run its sections in order. The script uses the
bundled cleaned Excel file. Figure objects are displayed interactively.

ANALYSIS NOTES
The notebook retains the original exploratory sections, including inspection-type
composition, follow-up linkage, sensitivity checks and violation-code coverage.
The final Python ZIP-map section is exploratory: the report uses the Tableau map.
The map section saves its figures in outputs/ and reuses the bundled boundary file.
Times New Roman is requested by several plots; install it for consistent typography.

PREPARATION AND VALIDATION
Original source files were not modified. Chinese comments, markdown headings and
console messages were translated into English. Absolute local file paths were
replaced with package-relative paths. Notebook outputs were cleared. One R comment
was corrected to describe the existing sample threshold of 50 (not 30).
No analytical formulas, filters, grouping rules or clustering settings were changed.
All Python code cells and the exported script passed syntax checks. The R script
passed parsing. Python syntax-tree comparison confirmed unchanged computational
structure apart from string content. Analyses were not rerun in full for packaging.
