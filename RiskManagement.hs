-- This file contains Requirement 8

module RiskManagement (displayHighRiskCustomers) where
import UserManagement (Customer (..), readCustomersFromFile)

-- Requirement 8
-- Function to display high-risk customers with balances under a given threshold
displayHighRiskCustomers :: IO ()
displayHighRiskCustomers = do
    putStrLn "Enter the threshold balance: "
    thresholdStr <- getLine
    let threshold = read thresholdStr :: Float
    customers <- readCustomersFromFile "CustomerData.txt"
    let highRiskCustomers = filter (\c -> riskNote c == "High" && balance c < threshold) customers
    if null highRiskCustomers
        then putStrLn "No high-risk customers with balances under the threshold found."
        else do
            putStrLn "High-risk customers with balances under the threshold:"
            mapM_ print highRiskCustomers
