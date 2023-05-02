library(shinyloadtest)

shinyloadtest::record_session()
# Mimick 'typical' user behaviour.

# Next, open a terminal in the 'flow-trendmaster3000' folder. Copy the text
# from the shiny_testing_javascript.txt file into the terminal. Adjust as necessary.

# Once this testing is finished, upload the test-log folder name and run below lines
# to read in the testing results and visualize them.

df <- load_runs('C:/Users/CMADSEN/Downloads/LocalR/flow-trendmaster3000/test-logs-2023-05-01T22_18_06.816Z')
shinyloadtest_report(df)

# Terrible initial results!!

