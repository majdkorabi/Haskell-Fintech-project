--  This "DataAnalysis.hs" contains Requirement 3, 4, 5, 6, 7
module DataAnalysis (averageAge, averageBalance, averageRiskFloat, displayAverageAge, displayAverageBalance, displayAverageRiskFloat, displayAverageRiskValue, topNCustomers) where
import UserManagement (Customer (riskNote),readCustomersFromFile, age, balance)
import Data.List (sortOn)


-- Requirement 3
-- Function to calculate the average age of customers
averageAge :: [Customer] -> Double
averageAge customers = fromIntegral (sum (map age customers)) / fromIntegral (length customers)
-- Function to display the average age of customers
displayAverageAge :: IO ()
displayAverageAge = do
    customers <- readCustomersFromFile "CustomerData.txt"
    let avgAge = averageAge customers
    putStrLn $ "Average Age of Customers: " ++ show avgAge


-- Requirement 4
-- Function to calculate the average balance of customers
averageBalance :: [Customer] -> Float
averageBalance customers = sum (map balance customers) / fromIntegral (length customers)
-- Function to display the average balance of customers
displayAverageBalance :: IO ()
displayAverageBalance = do
    customers <- readCustomersFromFile "CustomerData.txt"
    let avgBalance = averageBalance customers
    putStrLn $ "Average balance of customers: " ++ show avgBalance


-- Requirement 5
-- Function that maps risk levels to their corresponding float value
riskToFloat :: String -> Float
riskToFloat "High" = 3.0
riskToFloat "Medium" = 2.0
riskToFloat "Low" = 1.0
riskToFloat _ = 0.0  -- Default to 0.0 for other risk levels than the listed above  
-- Function to calculate the average risk in float
averageRiskFloat :: [Customer] -> Float
averageRiskFloat customers = sum (map (riskToFloat . riskNote) customers) / fromIntegral (length customers)
-- Function to display the average risk in float
displayAverageRiskFloat :: IO ()
displayAverageRiskFloat = do
    customers <- readCustomersFromFile "CustomerData.txt"
    let avgRiskFloat = averageRiskFloat customers
    putStrLn $ "Average Risk in Float: " ++ show avgRiskFloat


--Requirement 6
-- Function to map numerical risk values to risk categories
mapToRiskValue :: Float -> String
mapToRiskValue risk
    | risk >= 2.5 = "High"
    | risk >= 1.5 = "Medium"
    | otherwise = "Low"

-- Function to calculate the average risk in Risk Value
averageRiskRiskValue :: [Customer] -> String
averageRiskRiskValue customers =
    let avgRisk = averageRiskFloat customers
    in mapToRiskValue avgRisk

-- Display function for average risk in Risk Value
displayAverageRiskValue :: IO ()
displayAverageRiskValue  = do
    customers <- readCustomersFromFile "CustomerData.txt"
    let avgRisk = averageRiskRiskValue customers
    putStrLn $ "Average Risk in Risk Value: " ++ avgRisk


-- Requirement 7
-- Function to list the top n customers with the highest balance
topNCustomers :: IO ()
topNCustomers = do
    putStrLn "Enter the number of top customers you want to list: "
    n <- getLine
    let numberOfCustomers = read n :: Int
    customers <- readCustomersFromFile "CustomerData.txt"
    let sortedCustomers = reverse (sortOn balance customers)
        topN = take numberOfCustomers sortedCustomers
    putStrLn $ "Listing Top " ++ show numberOfCustomers ++ " Customers with the Highest Balance:"
    mapM_ print topN

