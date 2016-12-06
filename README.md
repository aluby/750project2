# 750project2

![alt text](./images/m_obama_pretty.pdf)

## Directory Information
The following assumes your working directory is where this README file is located. This should include the following:
- folder nyt-collection-text: used for random testing and also can be used for examples
- folder other-text-files: contains the default stopwords file, the default sentiment dictionary, and the transcript from Michelle Obama's 2016 DNC speech
- folder test-files: contains files used for testing
- color_by_sentiment.R: function to color the word cloud using basic sentiment analysis
- text_processing_functions.R: functions used in the text processing phase of making a word cloud
- vis_calculation_functions.R: functions used to make the word cloud visualization
- word_cloud_driver.R: script to run the word_cloud function from command line
- word_cloud_tests.R: test suite for all functions

## Examples
word_cloud('other-text-files/m_obama.txt', draw_bounding_boxes = FALSE, draw_pretty = TRUE, vscale = .75, hscale = .95)
