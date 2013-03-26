import Debug.Trace

f = \s ->
        case doSomething s of
            (x, s') -> case doSomethingElse s' of
                          (y, s'') -> (3, s'')
                          
doSomething s = trace "doSomething" $ (0, s)
doSomethingElse s = trace "doSomethingElse" $ (3, s)

main = print (f 2)