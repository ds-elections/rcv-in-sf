## Ranked Choice Voting: San Francisco

**Project Abstract**

FairVote is a nonpartisan organization with the goal of improving ‘the democratic process in America.’ One of their major initiatives is advocating for the implementation of ranked choice voting. Our project is interested in assisting one of their research fellows, Theo Landsman, with his work on the effects of ranked choice voting (RCV). To do so, we consider San Francisco Bay Area municipalities that have RCV ballot image data available.

The first process we have completed is wrangling and tidying the raw ballot image data from San Francisco County's November 8th, 2016 election for District Supervisors for Districts 1, 3, 5, 9, and 11 by using a master look up file to parse out voter information. Each row of the ballot image contains information such as the voter's precinct and the voter's ranking of the candidates for the race. We then produced visualizations displaying how several voter variables may impact ranked choice voting. 

Our output from this project is mainly a program that takes in a "California style" ballot image used in the elections we considered and outputs round-by-round election results, and some analysis of basic demographic data in relation to complexity and undervote rates of elections. Further work will include refining this program and expanding on the demographic analysis, as well as generalizing a program to create a dynamic Sankey diagram of a ranked choice election. A Sankey diagram for the SF Board of Supervisors District 1 election is included in this repository.

**Contributors**

Jay Lee

Mia Leung 

Matthew Yancheff

Profs. Andrew Bray & Paul Gronke, Case Studies: Statistical Analysis

