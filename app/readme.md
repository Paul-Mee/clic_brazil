# READ ME

This folder contains all the files required to run the CLIC app locally. There are two main files:

- *getdata_from_DB.R* downloads input data from DropBox and store in the `input_data` folder. 
You will need a token for this code to work. Use the `rdrop2` package to get the token. You
need to run this file prior to running the `app.R` file to ensure you have the most up to date
files in your local computer.
- *app.R* will read the input data and run the CLIC app locally on your machine.

To run the app just download all files and folders to your local computer. *Do not change the folder
names or hierarchy*. Open `app.R` in Rstudio and clic the *Runn App* button on the upper right hand 
side of your screen. The app should load automatically after that.
