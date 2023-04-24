// PREPARATORY STEPS

//  2. cd change working directory
//  3. open the dataset "Prediction-data.dta" 

/* here's an example to open the dataset in Windows using the 'use' command 
   alternatively, click File > Open 
*/
use "..\data\Prediction-data.dta", clear


// DATA EXPLORATION

//  4. Have a look at the data. 

summarize


// How many participants die? 

tab dead


// What proportion are female?

tab sex 


// What ages are these participants?

summ age



// RANDOMLY SPLIT DATA INTO TRAINING AND VALIDATION PARTS

//  5. Create a variable S = 0/1 that separates the data into two equal halves:

set seed 1111
gen rvar = runiform()
sort rvar
gen S = 0
replace S = 1 if _n > 1000



// FIT MODEL IN TRAINING DATA AND PREDICT RISKS

//  6. For the S=0 dataset, estimate the model using logistic regression. 
//     Include all measured variables as predictors.

logistic dead c.age i.sex c.sbp c.bmi if S==0



//  7. Predict the risk of deah for all individuals 
//     (i.e. those with S=0 and those with S=1).

predict m2pr

bysort dead: summ m2pr

graph box m2pr, over(dead)



// VALIDATION

//  8. Draw an ROC curve in the S=0 dataset

roctab dead m2pr if S==0, graph specificity


// What is the AUC? Interpret this number. 


// Now repeat for the S=1 dataset.

roctab dead m2pr if S==1, graph specificity


// Are the two ROC curves very different?


//  9. Create a Hosmer-Lemeshow goodness of fit table for the S=0 
//	  and the S=1 datasets.

estat gof if S==0, group(10) table
estat gof if S==1, group(10) table


//  10. Draw a bar graph comparing the predicted and observed risks in the S=0 data.

egen m2prg0 = cut(m2pr) if S==0, group(10)
graph bar (sum) m2pr (sum) dead, over(m2prg0)

// Repeat for the S=1 dataset.

egen m2prg1 = cut(m2pr) if S==1, group(10)
graph bar (sum) m2pr (sum) dead, over(m2prg1)



//  11. If you have time, see if you can write a short do-file to repeatedly split the data,
// calculate the AUC from the S=0 and S=1 datasets and store the results.
