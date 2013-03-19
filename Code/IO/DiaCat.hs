module DiaCat where
import Dialogue1

cat :: Dialogue
cat ~(Success : ~((Str userInput) : ~(Success : ~(r4 : _))))
 = [ AppendChan stdout "enter filename\n",
     ReadChan stdin, 
     AppendChan stdout name,
     ReadFile name,
     AppendChan stdout
            (case r4 of
	    	  Str contents -> contents
		  Failure ioerr -> "can’t open file")
   ] where (name : _) = lines userInput

cat2 :: Dialogue
cat2 ~(r1 : ~(r2 : ~(r3 : ~(r4 : _))))
 = AppendChan stdout "enter filename\n" : case r1 of
     Success -> ReadChan stdin : case r2 of 
            (Str userInput) -> let (name:_) = lines userInput in [
               AppendChan stdout name,
               ReadFile name,
               AppendChan stdout
                (case r4 of
	    	   Str contents -> contents
		   Failure ioerr -> "can’t open file")
               ]
            e2 -> error(show e2)
     e1 -> error (show e1)
--   where (name : _) = lines userInput

main = runDialogue cat