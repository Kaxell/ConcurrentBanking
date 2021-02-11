module Main where

import Control.Concurrent
import System.Random
import Data.List
import Control.Parallel

-- | Customer Data Type, it includes name, account number and account balance data
data Customer = Customer {
              name :: String,
              accNumber :: Int,
              accBalance :: Int
            } deriving (Show, Eq)
            
type Name = String
type ThreadName = String
type Customers = [Customer]

-- | Gets Customer Name
customerName :: Customer -> String
customerName (Customer name _ _ ) = name

-- | Gets Customer Account Number
customerAccNumber :: Customer -> String
customerAccNumber (Customer _ accNumber _ ) = show accNumber

-- | Gets Customer Account Balance
customerAccBalance :: Customer -> Int
customerAccBalance (Customer _ _ accBalance ) = accBalance

-- | Updates Customer with new values
updateCustomer :: Customer -> Int -> IO Customer
updateCustomer (Customer name accNumber accBalance) value = do
    let newBalance = accBalance + value
    let customer = Customer name accNumber newBalance
    return customer

-- | Updates Customers account balance
updateAccBalance :: Customer -> Int -> Customer
updateAccBalance (Customer name accNumber accBalance) value = (Customer name accNumber (accBalance + value))

-- | Creates a new customer with given inputs like name and account number and account balance            
createCustomer :: String -> Int -> Int -> IO Customer
createCustomer name accNumber accBalance = do
    let customer = Customer name accNumber accBalance
    return customer

-- | Selects random customer from the customer list 
chooseRandomCustomer :: [Customer] -> Customer -> IO Customer
chooseRandomCustomer customers customerToDelete = do 
    n <- randomIO :: IO Int
    let custList = cropCustomer customerToDelete customers
    let customerCount = length custList    
    let index = mod n (customerCount)
    let selectedCustomer = custList!!index
    return selectedCustomer

-- | Removes an item from the list of customers
cropCustomer :: Customer -> [Customer] -> [Customer]
cropCustomer _ [] = []
cropCustomer x (y:ys) | x == y = cropCustomer x ys
                    | otherwise = y : cropCustomer x ys

-- | Replaces customer in the given list
replaceCustomer :: Int -> a -> [a] -> [a]
replaceCustomer _ _ [] = []
replaceCustomer n newVal (x:xs)
  | n == 0 = newVal:xs
  | otherwise = x:replaceCustomer (n-1) newVal xs


-- | Thread implemantation
process :: ThreadName -> MVar String -> MVar Int -> MVar Customers -> MVar Customer -> IO ()
process threadName counter iteration customers customer = do
    
    thisCustomer <- takeMVar customer  -- for this customer
    customerList <- takeMVar customers
    currentCount <- readMVar iteration -- current transaction #
    
    n <- randomIO :: IO Int
    let round = mod n 100

    if (currentCount >= 100) then
        putMVar counter ("\n\n" ++ "Threads finished with " ++ show currentCount ++ " iterations!" ++"\n\n" ++ "Final customer information is:\n " ++ show customerList) -- finish condition  
    else do        
        n <- randomIO :: IO Int
        let value = 10 + (mod n 40)
        let decvalue = value * (-1)

        count <- takeMVar iteration
        let increment = count + 1
        putMVar iteration (increment)
        
        randomCustomer <- chooseRandomCustomer customerList thisCustomer  -- get a random customer
        updatedCustomer <- updateCustomer thisCustomer value 
        randupdatedCustomer <- updateCustomer randomCustomer decvalue
        
        putStrLn $ threadName ++ show value ++ " Euro " ++ "transaction between " ++ (show (customerName thisCustomer)) ++  " and " ++ (show (customerName randomCustomer))
        
        putMVar customer updatedCustomer
        let index = findIndex (== thisCustomer) customerList
        let justIndex = convertIndex index
        let newCustomerList = replaceCustomer justIndex updatedCustomer customerList
        
        let randindex = findIndex (== randomCustomer) newCustomerList
        let randjustIndex = convertIndex randindex
        let newCustomerList2 = replaceCustomer randjustIndex randupdatedCustomer newCustomerList

        putMVar customers newCustomerList2
        threadDelay 100
        process threadName counter iteration customers customer 



-- | Converts Maybe Int to Int and Handles Error case
convertIndex :: Maybe Int -> Int
convertIndex (Just a) = a 
convertIndex Nothing = 0

-- | Main thread that creates ten customers, spawns ten threads
main :: IO ()
main = do
    
    customer1 <- createCustomer "James" 1 1000
    customer2 <- createCustomer "Michael" 2 1000
    customer3 <- createCustomer "Glyn" 3 1000
    customer4 <- createCustomer "Oran" 4 1000
    customer5 <- createCustomer "Wilma" 5 1000
    customer6 <- createCustomer "Caprice" 6 1000
    customer7 <- createCustomer "Alix" 7 1000
    customer8 <- createCustomer "Quinn" 8 1000
    customer9 <- createCustomer "Amit" 9 1000
    customer10 <- createCustomer "Lacey" 10 1000

    putStrLn $ customerAccNumber customer1 ++ ". " ++ (show customer1)
    putStrLn $ customerAccNumber customer2 ++ ". " ++ (show customer2)
    putStrLn $ customerAccNumber customer3 ++ ". " ++ (show customer3)
    putStrLn $ customerAccNumber customer4 ++ ". " ++ (show customer4)
    putStrLn $ customerAccNumber customer5 ++ ". " ++ (show customer5)
    putStrLn $ customerAccNumber customer6 ++ ". " ++ (show customer6)
    putStrLn $ customerAccNumber customer7 ++ ". " ++ (show customer7)
    putStrLn $ customerAccNumber customer8 ++ ". " ++ (show customer8)
    putStrLn $ customerAccNumber customer9 ++ ". " ++ (show customer9)
    putStrLn $ customerAccNumber customer10++ ". " ++ (show customer10)

    let customers = [customer1, customer2, customer3, customer4, customer5, customer6, customer7, customer8, customer9, customer10 ]
     
    counter <- newEmptyMVar
    iteration <- newEmptyMVar
    customerList <- newEmptyMVar
    putMVar customerList (customers)
    putMVar iteration (0)

    --process "threadA" customersList box  
    box <- newMVar customer1
    forkIO (process "--thread 1-- " counter iteration customerList box) 
    box <- newMVar customer2
    forkIO (process "--thread 2-- " counter iteration customerList box) 
    box <- newMVar customer3
    forkIO (process "--thread 3-- " counter iteration customerList box) 
    box <- newMVar customer4
    forkIO (process "--thread 4-- " counter iteration customerList box) 
    box <- newMVar customer5
    forkIO (process "--thread 5-- " counter iteration customerList box) 
    box <- newMVar customer6
    forkIO (process "--thread 6-- " counter iteration customerList box) 
    box <- newMVar customer7
    forkIO (process "--thread 7-- " counter iteration customerList box) 
    box <- newMVar customer8
    forkIO (process "--thread 8-- " counter iteration customerList box) 
    box <- newMVar customer9
    forkIO (process "--thread 9-- " counter iteration customerList box) 
    box <- newMVar customer10
    forkIO (process "--thread 10-- " counter iteration customerList box) 
    
    finished <- takeMVar counter -- will block the main thread    
    putStrLn $ finished

