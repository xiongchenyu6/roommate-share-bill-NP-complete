# roommate-share-bill-NP-complete
[![Build Status](https://travis-ci.org/xiongchenyu6/roommate-share-bill-NP-complete.svg?branch=master)](https://travis-ci.org/xiongchenyu6/roommate-share-bill-NP-complete)

## Objective
To solve the roommate share bill problem

## Sample Question
Alice, Bob, Claire, and David are good friends who share an apartment. They would split the
house bills and other expenses amongst themselves. Since every one of them pays for
something, they would try to figure out what is the easiest way to settle amongst themselves.
They have contracted you to help them with this problem.
Write a command line application that takes in two input text files.
The first text file contains a list of people.

* Alice
* Bob
* Claire
* David
The second text file would contain list of expense transactions.
* Claire paid $100.10 for phone bill.
* Bob paid $55.90 for petrol.
* David paid $170.80 for groceries.
* David paid $33.40 for breakfast.
* Bob paid $85.60 for lunch.
* Claire paid $103.45 for dinner.
* Claire paid $30.80 for snacks.
* Bob paid $70 for house-cleaning.
* David paid $63.50 for utilities.
The output of the program should print the following lines:
* Alice pays $xx.xx to David.
* Bob pays $xx.xx to Claire.

## Text File Preprocessing
1. I will parse the first txt file to List[(Name,0)], 0 stand for balance
2. I will parse and second txt file and time the amount by 100, since data type float is not suitable for money,I will sum the total amount and get List[(Name,subtotal)], and total amount
3. I will apply subtraction total amount / name count to the List[(Name,Subtotal)] to get the List[(Name,Balance)]
4. I will sort the list by Balance.

## Analyse
Compare this problem to most famous subset sum problem, we can see this Roommate problem is as least as hard as Subset Sum.
So there is no polynomal time complexity method exist.

So fisrt I will trying 2-combinations, then 3-combinations etc.

If the sum of subgroup is zero. I will Sort members from based on their balance first. Then the "richest" member pairs with the "poorest" and the transaction is made between them.

Base Singapore Law one room is not allow for more then 10 people, so the exponential time complexity is fast enough.

## Constrain
   Input name list less than 19.

## Code design
   I choose FP paragdigm.
   The Transfer is Wrapper in a Writer Monad for convinent composition.
   Usr Control.Arrow for clean and more readable code style.

## Documentation
   All function has already documented you can generate by Haddock.

## Testing
 The test strategy for the problem. I choose to use Property Test.

 I random generate transactions within the constrain. And compare the sum of the transfers with
 the sum of the sum of positive member. If they are equal then pass the test.

 I generate 100 different test case to ensure the posibility of the correctness.

## How to toy with it

First install the build tool `stack` in your computer
<https://www.haskell.org/platform/>

Then change directories to my file directory
```
stack setup
```
Method 1 (Install my executable)
```
stack build
```

After that the executable should be install in your environment

```
roommate-share-bill-NP-complete
```

You can enjoy the code by this command


Method 2 (Interpret my code only)

if you just want to try and don't want to install the executable file

```
stack ghci
```

Then in the lambda shell

```
main
```
