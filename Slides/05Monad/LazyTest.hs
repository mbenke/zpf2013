import Debug.Trace

f = \s ->
        let (x, s')  = doSomething s
            (y, s'') = doSomethingElse s'
        in (3, s'')

doSomething s = trace "doSomething" $ (0, s)
doSomethingElse s = trace "doSomethingElse" $ (3, s)

main = print (f 2)