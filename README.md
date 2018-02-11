# roommate-share-bill-NP-complete

## Question
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

## Preprocessing
1. I will parse the first txt file to List[(Name,0)], 0 stand for balance
2. I will parse and second txt file and time the amount by 100, since data type float is not suitable for money,I will sum the total amount and get List[(Name,subtotal)], and total amount
3. I will apply subtraction total amount / name count to the List[(Name,Subtotal)] to get the List[(Name,Balance)]
4. I will sort the list by Balance
