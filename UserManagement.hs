-- This file Contains customer information and Displays FinTech Customers Requirement 2

module UserManagement where
import Data.Maybe (mapMaybe)
import Data.List (sortOn)
import Text.Read (readMaybe)

-- Define the Customer data structure
data Customer = Customer
    { customerId :: Int
    , firstName :: String
    , lastName :: String
    , age :: Int
    , email :: String
    , balance :: Float
    , riskNote :: String
    } deriving (Show, Read) 

-- Function to parse a line of the customer data
parseCustomer :: String -> Maybe Customer
parseCustomer line = case words line of
    [cid, fname, lname, ageStr, email, balanceStr, riskNote] ->
        case (readMaybe cid :: Maybe Int, readMaybe ageStr :: Maybe Int, readMaybe balanceStr :: Maybe Float) of
            (Just cId, Just a, Just b) -> Just (Customer cId fname lname a email b riskNote)
            _ -> Nothing
    _ -> Nothing

-- Function to read and parse customer data from a file
readCustomersFromFile :: FilePath -> IO [Customer]
readCustomersFromFile filePath = do
    content <- readFile filePath
    return (read content)

-- Function to list all customers in increasing order of customerID
listAllCustomers :: FilePath -> IO ()
listAllCustomers filePath = do
    customers <- readCustomersFromFile filePath
    let sortedCustomers = sortOn customerId customers
    mapM_ print sortedCustomers
