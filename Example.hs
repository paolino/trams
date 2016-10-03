
import Application

printLogs n = mapM_ (print . fmap toMinutes) . take n 

-- example 

p0 = Params (\_ _ -> (10,20)) [(StopName 1,200),(StopName 2, 50),(StopName 3,150),(StopName 4,100)] 10 (\(Count n) -> 3*n) (StopName 1,60)
p1 = Params (\_ _ -> (5,10)) [(StopName 1,200),(StopName 2, 200)] 30 (\(Count n) -> 3*n) (StopName 1,60)
    
main = fromParams p0 >>= printLogs 1000 
