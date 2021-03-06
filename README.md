# Download data using GlobalArchive query

See workflow examples https://docs.google.com/presentation/d/15_RGua7r-XKcCo_SzH93iEpvh2QLs5PdLg4vADeuJLM/edit?usp=sharing


This repository will holds scripts which demonstrate how to download data from GlobalArchive by <i>"Workgroup"</i> and by <i>"Project"</i>.

NOTE: The <i>get_campaigns_by_project</i> example only uses campaigns that have been analysed using EventMeasure whereas the <i>get_campaigns_by_workgroup</i> example will include both EventMeasure and generic data.

### Check and format stereo-video data
In addtion we have provide a script to CHECK MaxN and Length data against a life.history sheet
AND 
a script to FORMAT data for further analysis, including adding in zeros where appropriate. 


### Set your working directory
Before running the script create the main folder where you will be working out of. You can do this in File Explorer or in the Files Pane in RStudio.
The script will then create two subdirectories. The first is a <i>"Downloads"</i> folder, where all files downloaded from GlobalArchive will sit. The second is a <i>"Tidy data"</i> folder, where the final .csv files will be saved once all the data has been combined.

<b>The script will also delete any files that are already in your <i>"Downloads"</i> folder if you have used this working directory before. This is to ensure that only the most recent files are downloaded. Do not use this folder to save anything other than the downloaded files from GlobalArchive to avoid losing data. Data deleted using the unlink() function is unrecoverable!</b>

### Add your API user token
The demonstration user API is provided in the script. To obtain your personal API, when logged into GlobalArchive click on your username in the top right corner, then choose API token from the dropdown. Paste your new API into the script.

### Change the API search to match your needs
There are a number of different ways to search. The two scripts in this repository demonstrate how to download data by <i>"Workgroup"</i> and by <i>"Project"</i> but you can also download data using these queries:

#### Search for all campaigns matching pattern ( % = wildcard)
~~~~
q='{"filters":[{"name":"name","op":"like","val":"%_PointAddis_stereoBRUVs"}]}'
~~~~

#### Search for specific campaign by name
~~~~
q='{"filters":[{"name":"name","op":"eq","val":"2011-09_Barrow.PDS_stereoBRUVs"}]}'
~~~~

#### Search for all campaigns by user's email
~~~~
q='{"filters":[{"name":"user","op":"has","val":{"name":"email","op":"eq","val":"brooke.gibbons@uwa.edu.au"}}]}'
~~~~

#### Search for all campaigns from Collaboration with wildcard search (%=wildcard, ilike=case insensitive)
~~~~
q='{"filters":[{"name":"workgroups","op":"any","val":{"name":"name","op":"ilike","val":"nsw%bruvs"}}]}'
~~~~

#### Get all campaigns that my user account has access to
~~~~
q=""
~~~~

### Run the query and process campaign files
Once you have set the API and API search you are ready to download the data. These will be downloaded into your <i>"Downloads"</i> folder. The file structure of GlobalArchive is maintained. Within your <i>"Downloads"</i> folder there will be <i>Project</i> folders, and within these will be the individual campaigns.

### Combine downloads into final files
The example scripts include functions to combine all the files of one type (info, metadata, points, count and length) from each campaign downloaded, into one dataframe. These are then used to create the final maxn.csv and length.csv files. These two files are saved into your <i>"Tidy data"</i> folder.
