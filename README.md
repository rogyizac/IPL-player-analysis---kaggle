# IPL-player-analysis-kaggle
Analysis made on IPL dataset for seasons 2015 - 2017 to identify valuable players. 
Analysis was made on players who played in any of the seasons from 2015 - 2017.

Datasets : Ball_By_Ball.csv, Player.csv
Note : Please use the datasets uploaded in the repository since some changes were made to the dataset columns.

The MVPI(Most valuable player Index) for each player was calculated for each player and RFE(Recursive Feature Elimination) method with Random forest was used to identify important components of the MVPI.

Finally three indices were recalculated from the results obtained from RFE for cumulative years 2017 to 2015, 2017 to 2016 and 2017 alone. The indices are,
Batting index
Bowling index
Allround index

These indices were ranked by their decreasing order and finally the result obtained talks about the best batsmen, best bowlers, all rounders for the cumulative years.

Insights from this approach : Allrounders were identified from players who were only tagged as a bowler or a batsmen.

Just run the whole code and finally take a look at the dataset df_ipl_mvpi...

This is an intial approach to find the best players. Other inputs from the given datasets can be used to find underlying factors affecting a players value which can be crucial to match wins..
