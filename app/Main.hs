module Main where


import Control.Concurrent
import Control.Concurrent.TChan 

data StudentStatus 
    = ComingToLibrary
    | WaitingForBook
    | TookBook
    | GoesToLab
    | WaitingForComputer
    | TookComputer
    | FreeComputer
    | BringBookToLibrary
    | Done
    deriving (Eq, Show)

data StudentState = StudentState
    { stateNumber:: Int
    , stateStatus:: StudentStatus
    }
    deriving (Eq, Show)

data ComputerRequest
    = ComputerReqGet
    | ComputerReqFree
    | ComputerReqStop
    deriving (Eq, Show)

data ComputerResponse 
    = ComputerResOK
    | ComputerResFailed
    deriving (Eq, Show)

main = do
    books <- getLine >>= return . read
    computers <- getLine >>= return . read
    libMbox <- createLibrary books
    computerMbox <- createComputerClass computers
    return ()

log:: Int-> String-> IO ()
log sId state = putStrLn $! "Student " ++ show sId ++ ": " ++ state

createComputerClass:: Int-> IO (TChan ComputerRequest)
createComputerClass computers = do
    chan <- newTChanIO
    forkIO $ computersThread computers chan


computersThread

studentThread:: StudentStatus-> Int-> LibraryMBox-> ComputersMBox -> IO ()
student ComingToLibrary sId libMbox compMbox = do
    log sId "coming to library"
    student WaitingForBook sId libMbox compMbox
student WaitingForBook sId libMbox compMbox = do
    log sId "waiting for a book"
    getBook libMbox
    student TookBook sId libMbox compMbox
student TookBook sId libMbox compMbox = do
    log sId "took book"
    student GoesToLab sId libMbox compMbox
student GoesToLab sId libMbox compMbox = do
    log sId "goes to lab"
    student WaitingForComputer sId libMbox compMbox
student WaitingForComputer sId libMbox compMbox = do
    log sId "waiting for computer"
    getComputer compMbox
    student TookComputer sId libMbox compMbox
student TookComputer sId libMbox compMbox = do
    log sId "took computer"
    student FreeComputer sId libMbox compMbox
student FreeComputer sId libMbox compMbox = do
    log sId "freeing computer"
    freeComputer compMbox
    student BringBookToLibrary sId libMbox compMbox = do
student BringBookToLibrary sId libMbox compMbox = do
    log sId "giving book back to library"
    returnBook libMbox
    student Done sId libMbox compMbox
student Done sId _ _  = do
    log sId "is done"
    return ()
student state sId _ _ = do
    putStrLn $ "Student " ++ show sId ++ " unhandled state " ++ show state
    return ()

