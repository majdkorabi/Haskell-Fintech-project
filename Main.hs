-- This Main.hs contains user navigation Requirement 1
    module Main where
    import UserManagement (listAllCustomers, Customer, readCustomersFromFile)
    import DataAnalysis (displayAverageAge, displayAverageBalance, displayAverageRiskFloat, displayAverageRiskValue, topNCustomers)
    import RiskManagement (displayHighRiskCustomers)
    
-- Menu display Func.
    menuNav :: IO ()
    menuNav = do
        putStrLn "Choose an option:" -- Requirement 1 is all this user Navigation
        putStrLn "1. List all customers" -- Requirement 2
        putStrLn ">>>>>> Customer Data Analysis"
        putStrLn "2.1 Customer average age" -- Requirement 3
        putStrLn "2.2 Customer average balance" -- Requirement 4
        putStrLn "2.3 Customer average risk in float" -- Requirement 5
        putStrLn "2.4 Customer average risk in enum" -- Requirement 6
        putStrLn "2.5 List top n customer balance" -- Requirement 7
        putStrLn ">>>>>> Risk Analysis"
        putStrLn "3 List High Risk Customers" -- Requirement 8
        putStrLn "4 Exit\n"
        putStrLn "Enter your choice: "
        input <- getLine
        putStrLn ("Your choice was " ++ input )

        if input == "1"
            then do
                putStrLn "Listing all customers!\n"
                listAllCustomers "CustomerData.txt" --Requirement 2
                menuNav
        else if input == "2.1"
            then do 
                putStrLn "Printing customer average age!\n"
                displayAverageAge  --Requirement 3
                menuNav
        else if input == "2.2"
            then do 
                putStrLn "Printing customer average balance!\n"
                displayAverageBalance  --Requirement 4
                menuNav
        else if input == "2.3"
            then do 
                putStrLn "Printing customer average risk in float!\n"
                displayAverageRiskFloat  --Requirement 5
                menuNav
        else if input == "2.4"
            then do 
                putStrLn "Printing customer average risk in enum!\n"
                displayAverageRiskValue  --Requirement 6
                menuNav
        else if input == "2.5"
            then do 
                putStrLn "Listing top n customer balance!\n"
                topNCustomers  --Requirement 7
                menuNav
        else if input == "3"
            then do 
                putStrLn "Listing high risk customers!\n"
                displayHighRiskCustomers  --Requirement 8
                menuNav
        else if input == "4"
            then putStrLn "Exiting"
        else
            do 
            putStrLn "Invalid String, Enter a valid option"
            menuNav


    main :: IO ()
    main = do
        menuNav

        