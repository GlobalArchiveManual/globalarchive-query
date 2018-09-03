# globalarchive-query
This repository holds two scripts which demonstrate how to download data from GlobalArchive by <i>"Workgroup"</i> and by <i>"Project"</i>.

### First you will need to sSet your working directory
Before running the script create the main folder where you will be working out of. You can do this in File Explorer or in the Files Pane in RStudio.
The script will then create two subdirectories. The first is a <i>"Downloads"</i> folder, where all files downloaded from GlobalArchive will sit. The second is a <i>"Tidy data"</i> folder, where the final .csvs will be saved once all the data has een combined.

The script will also delete any files that are already in your <i>"Downloads"</i> folder (if you have used this working directory before). To ensure that only the most recent files are downloaded. The API function can not change any files that have been altered on GlobalArchive, it can only replace files with the same name.

### Add your API user token
If you have been set an API token add it, or use the demonstration user API.

### Change the API search to match your needs
There are a number of different ways to search. The two scripts in this repository demonstrate how to download data by <i>"Workgroup"</i> and by <i>"Project"</i> but you can also download data using these queries:

### Run the query and process campaign files
Once you have set the API and API search you are ready to download the data. These will be downloaded into your <i>"Downloads"</i> folder. The file structure of GlobalArchive is maintained. Within your <i>"Downloads"</i> folder there will be <i>Project</i> folders, and within these will be the individual campaigns.

### Combine downloads into final files
The example scripts include functions to combine all the files of one type (info, metadata, points, count and length) from each campaign downloaded into one. These are then used to create the final maxn.csv and length.csv files. These two files are then saved into your <i>"Tidy data"</i> folder.
