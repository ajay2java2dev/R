# Project 4: Movie Recommendation 
(https://piazza.com/class/kjvsp15j2g07ac?cid=398)

- Dataset : https://grouplens.org/datasets/movielens/

	- The dataset contains about 1 million anonymous ratings of approximately 3,900 movies made by 6,040 MovieLens users who joined MovieLens in 2000.
	- EDA 1 - https://liangfgithub.github.io/Rcode_W13_Movie_EDA.nb.html
	- EDA 2 - https://liangfgithub.github.io/Rcode_W13_Movie_RS.nb.html

### Submission ( 6 pts each)
    ○ An R/Python Markdown file in HTML or a link to such a file, containing all necessary code to reproduce the reported results. No page limit. 
    ○ Web link to a movie recommendation app, e.g., Shiny app, built by your team. You can share with us the link to your source code, or submit a copy of your code as one zip file on Coursera.


## Submission 1 : HTML Markdown 
### System I: recommendation based on genres.	
    Suppose you know the user's favorite genre. How would you recommend movies to him/her?    
    Propose two recommendation schemes along with all necessary technical details. 
    
    For example, you can recommend the top-five most popular movies in that genre, then you have to define what you mean by "most popular". Or recommend the top-five highly-rated movies in that genre; again need to define what you mean by highly-rated. (Will the movie that receives only one 5-point review be considered highly-rated?) Or recommend the most trendy movies in that genre; define how you measure trendiness.
	

### System II: collaborative recommendation system.	
    1. Review and evaluate at least two collaborative recommendation algorithms, for example user-baesd, item-based, or SVD.
    2. Provide a short introduction of those algorithms, then pick a metric, e.g., RMSE, to evaluate the prediction performance of the algorithms, over 10 iterations. 
    3. In each iteration, create a training and test split of the MovieLens 1M data, train a recommender system on the training data and record prediction accuracy/error on the test data. Report the results via a graph or a table. 
			
    • There is no accuracy benchmark; the purpose here is to demonstrate that you know how to build a collaborative recommender system.
    • You can decide the percentage for training/test data.
    • It'll be great if you want to tune the model parameters, but it's fine to just fix them at some values. Include a description of the meaning of each parameter and the value you have used throughout the simulation study.

    Include the necessary technical details. For example, suppose you study the user-based or item-based CF. 
        • Will you normalize the rating matrix? If so, which normalization option you use?
        • What's the nearest neighborhood size you use?
        • Which similarity metric you use?
        • If you say prediction is based on a "weighted average", then explain what weights you use.
        • Will you still have missing values after running the algorithm? If so, how you handle those missing values?
        
    If you use an SVD-based approach, you cannot simply say that the method is based on the ordinary matrix SVD since the ordinary SVD is not applicable to a matrix with missing values. Instead, provide the objective function the algorithm aims to optimize. If the objective function has some tuning parameters, specify the values you use, e.g., what's the dimension of the latent features.
	
  ## Submission 2: Movie Recommendation App
	Shiny App: Build a shiny app (or any other app) with at least one algorithm from Systems I and one algorithm from Systems II. 

	For the algorithm from Systems I, your app needs to take the input from a user on his/her favorite genre. 
    For the algorithm from Systems II, your app needs to provide some sample movies and ask the user to rate. 
    
    The two systems do not have to share any interface; please allow users to select which system to use.
    
    We will test your app. 4pt, if it works; 2pt for design. For example, an app like [this] will receive 6pt.

## Resources
    You can use others' code, as long as you cite the source. 

        - Github for the nice Book Recommender Systems mentioned above where you can also find his Kaggle report.   [https://github.com/pspachtholz/BookRecommender] 

        - Rcode for Week 13 	[Rcode_W13_Movie_RS](https://liangfgithub.github.io/Rcode_W13_Movie_RS.nb.html) [Rcode_W13_Movie_EDA](https://liangfgithub.github.io/Rcode_W13_Movie_EDA.nb.html)

        - Comparing State-of-the-Art Collaborative Filtering Systems 	[Link]

        - Search "recommender system" on Kaggle, and of course, Google.	


## Links and Resources:
0. Must read: https://cran.r-project.org/web/packages/recommenderlab/vignettes/recommenderlab.pdf
1. Movie Images: https://piazza.com/class_profile/get_resource/kjvsp15j2g07ac/knb8omj987h739
2. Lecture Notes: https://piazza.com/class_profile/get_resource/kjvsp15j2g07ac/knmiw9ucu6654i
3. Stanford Videos (start here): 
    3.1 Intro: https://youtu.be/giIXNoiqO_U?list=PLLssT5z_DsK-h9vYZkQkYNWcItqhlRJLN
    3.2 Content based: https://youtu.be/9siFuMMHNIA?list=PLLssT5z_DsK-h9vYZkQkYNWcItqhlRJLN
    3.3 Collabarative System: https://youtu.be/9AP-DgFBNP4?list=PLLssT5z_DsK-h9vYZkQkYNWcItqhlRJLN , https://youtu.be/YW2b8La2ICo?list=PLLssT5z_DsK-h9vYZkQkYNWcItqhlRJLN 
    3.4 Vectorization Low Rank: https://youtu.be/5R1xOJOFRzs?list=PLLssT5z_DsK-h9vYZkQkYNWcItqhlRJLN
    3.5 Mean normalization: https://youtu.be/Am9fhp2Q91o?list=PLLssT5z_DsK-h9vYZkQkYNWcItqhlRJLN
