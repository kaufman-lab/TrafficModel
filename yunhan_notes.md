Hi Logan,
 
Sorry that there is not much documentation about estimating traffic volume on roads. All the necessary data are already on the release of the github repo TrafficModel (https://github.com/kaufman-lab/TrafficModel/releases/tag/v1.0.1). I am attaching some useful scripts for this procedure. 
 
To summarize, you need to 
Get the road network by road class (A1-A3) for your target region. This can be done using getLine.R, where I used the Chicago region as an example. As there are tens of millions of roads in the US, the road network data are stored by regions categorized by zip codes. You need to first identify the range of zip code for your study area and then download the corresponding file(s). You might also need a shapefile that delineates the boundary for your study region, as you might not need the road network for the entire zip codes chunks. 
 

Predict the traffic volume on all the road segments by road class, using pre-fitted models (also from the release). And the scripts also help to generate data files into the format suitable as RLINE input.
 
(Optional) You may also visualize the predicted traffic volumes for sanity check.
 
Please let me know if you have any further questions. Sorry that I do not have enough time to write up detailed readme file or instructions on this procedure. It would be great if you could formulate one as you go through the process. I am also available for a brief meeting if needed.
 
Best,
Yunhan