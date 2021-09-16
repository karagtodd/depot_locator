# Instructions for using the Depot Locator Tool
### Automatically load data from MARTA case study
1. Download R and (optional) R studio as per the directions below
2. Paste the following code in the console and press "enter"
```
install.packages("shiny")
shiny::runGitHub("depot_locator", "freyja-bt", "challenge")
```
3. Click the checkbox "Load files from the MARTA case study" at the top
4. (Optional) Scroll down to the bottom and choose to include residential roads, if desired
5. Click "Submit" at the bottom

## Download R and R Studio
1. Download R from this website for free: https://cran.r-project.org/bin/windows/base/
2. Download R Studio from this website (also for free): https://www.rstudio.com/products/rstudio/download/#download
   * Provides an interface that’s easier to work with

## Check that you have all input files
1. GTFS data
   * calendar.txt
   * trips.txt
   * routes.txt
   * stop_times.txt
   * stops.txt
2. Existing and candidate depot sites
   * A .csv file with data on existing depots. The file should have at least the following columns: 
     * A column titled “id” with the ID #'s of your depots 
        - If the depots do not have ID’s, they can simply be numbered 1, 2, 3, etc.
     * A column titled 'lat' with latitude coordinates of your depots 
     * A column titled 'lon' with longitude coordinates of your depots
   * A .csv file with the candidate properties you’re considering. The file should have at least the following columns:
     * A column with the name or address of the properties 
     * A column with the ID #'s of the properties 
        - If the depots do not have ID’s, they can simply be numbered 1, 2, 3, etc.		
     * A column titled 'lat' with latitude coordinates of the properties
     * A column titled 'lon' with longitude coordinates of the properties

## Run the tool in R Studio
###### Method 1
1. Open R Studio
2. Paste the following lines in the Console (default bottom-left panel) and press "enter":
```
install.packages("shiny")
library(shiny)
runGitHub("depot_locator", "karagtodd")
```
   * This will download and run the app.R script in this repository
   * Follow instructions in the app window to upload data files
   * Progress outputs and any error messages will be printed in the Console
3. When the app is finished running, click "download" to save the .csv file of the new vehicle assignments

###### Method 2
1. Download "app.R" script
2. Open R Studio
2. Open “app.R” script
3. Click “Run App” in the top right corner of the script panel
   * Follow instructions in the app window to upload data files
   * Progress outputs and any error messages will be printed in the Console (bottom left panel)
4. When the app is finished running, click "download" to save the .csv file of the new vehicle assignments

NOTE: The output on cost savings will not be saved. Please copy and paste this text into a separate file for your records. If you forget to save the text, the same values can be easily recalculated from the output in the .csv file.

## Common Errors and Solutions
* Error message: “The following highway types are present in data yet lack corresponding weight_profile values: road, corridor,”
  * This will not stop the code from running. It is just a warning that some travel time estimates will be less accurate than others.
* Error message: “NAs introduced by coercion”
  * Problem: This is likely due to the formatting of your input for hourly wage and/or operating cost. It should not stop the app from running but will probably result in “NA” values in the output text instead of calculating cost savings.
  * Solution: Ensure that only numeric characters are used in your cost input. For example, 15.25 is acceptable, but $15.25 will result in this error message.
* Error message: “Warning in read.table(file = file, header = header, sep = sep, quote = quote,  : incomplete final line found by readTableHeader”
  * Problem: The program is unable to read one of the input files. The problem is most likely in one of the .csv property files.
  * Solution:
    * Open the .csv file in a simple text editor (Notepad, TextEdit, etc.)
    * Place your cursor at the end of the last line and press Enter.
    * Save the file and re-run the app.
