# Concurrent Banking System
The program defines data type Customer, including name, account number, and account balance. main thread creates ten customers and spawn ten threads for each of them. The process thread selects on the customers randomly and transfer a random amount of money between £10 and £50. When the system reaches 100 transfers, the threads terminate, and the information of all customers are shown in the screen.

Program can be built by the following commands: 
$ stack build 
$ stack run
