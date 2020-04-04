# OECD Policy-Impact-Tracker
## Monitoring tool for tracking the legislative agenda in the United Kingdom
This repository is meant to act as a proof of concept for the development of a more robust policy impact tracker for the OECD. It currently has two parts:
1. The **first** is the an R script (labeled Plain_Text.r) containing the functions to create a text processor which takes plain text files as input and returns clean and formatted text data ready for analysis.
2. The **second** is an rmd file which uses the functions in the script to identify mentions of the OECD in Hansard transcripts.

To build a more robust monitoring tool two additional pieces need to be developed and integrated:
1. The website TheyWorkForYou provides an API that allows you to download all new available Hansards on a daily basis. By integrating this API to the rmd file, one would simply need to run the rmd file once a day to retrieve the new Hansard transcripts and scrape them for OECD mentions.
2. Once the OECD mentions are identified on a daily basis, additional NLP analysis should be performed on each document containing a mention to determine the topic, sentiment and type of mention (either reference or recommendation). The code to perform such an analysis could also be integrated into the rmd file.

### Additional resources
TheyWorkForYou API: 
* http://parser.theyworkforyou.com/hansard.html 
* https://www.theyworkforyou.com/api/ 

