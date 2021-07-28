# MaSk_SuperClass
Matrix Sketching for Supervised Classification with imbalanced classes

The presence of imbalanced classes is more and more common in practical applications and it is known to heavily compromise the learning process. 
We propose to use matrix sketching as an alternative to the standard rebalancing strategies that are based on random under-sampling the majority class or random over-sampling the minority one.

The included function are the following:
- `MaSk_function`: it is the main function. Requires the data matrix and the class labels (coded as 0/1), the desired overall sample size, the desired fraction of units from the minority class (deafult 0.5) and the type of matrix sketching (one among Gaussian, Clarkson-Woodruff, Hadamard). It returns a list with the new rebalanced data and the new class labels.
- `Example_runme`: it is an example file that illustrates how the function works on the classic Fisher's Iris data.
- `Simulation_function`: the code illustrates how to generate data according to the simulation study presented in the paper.
