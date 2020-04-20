# "s2LAI" - Calculate Leaf Area Index from Sentinel-2 Imagery

A companion R Shiny application for the publication Cohrs, C.W., R.L. Cook, J.M. Gray, and T.J. Albaugh. (2020). Sentinel-2 Leaf Area Index Estimation for Pine Plantations in the Southeastern United States. Remote Sensing. under-review

### A **live version** can be accessed via https://www.treepoet.com/s2lai.

![s2LAI - Example Image](https://static.wixstatic.com/media/eae594_b3ca0228d2aa4755ac542b797152bcf4~mv2.png)

### Features Fully Implemented:
1) Upload Sentinel-2 Level-2A Band 4 & 8
2) Upload shapefiles, representing Area of Interest (AOI), e.g. either for single of multiple forest stands/sites; explore tabular contents.
3) Calculate LAI for either the entire scene, or just clipped to the uploaded AOI.
4) Export/Download LAI Output, clipped to the AOI that was uploaded by the user or the entire scene.

### Features Still To Be Implemented:
1) Calculate the weighted, mean LAI for each individual observation/polygon within the uploaded AOI dataset; dynamically join to table.
2) Add an "Export as CSV" button to download the tabular contents of uploaded AOI, with LAI calculations included for each row/observation/polygon.
3) Add a "Save as New Shapefile" button to download the updated shapefile.
4) Rendered plot interactivity (e.g. dynamically updating values as user moves cursor across the plot)
A stand-alone, local version is planned.

Please credit **Chris Cohrs** (treepoet.com) if you use or share this work. 

All inquiries can be sent to cohrs.xyz@gmail.com. 
