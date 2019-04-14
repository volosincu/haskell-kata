import System.IO
import GHC.IO.Handle

rootfs::FilePath
rootfs = "./file"

main = do
     handle <- openFile rootfs ReadMode
     print "Write the number of lines to read: "
     howMany <- getChar
     print "Reading lines."
     contents <- readLines handle 0 ((read . pure :: Char -> Int) howMany)
     print contents
     return ()


readLines :: Handle -> Int -> Int -> IO (String)
readLines = _readLines ""


_readLines :: String -> Handle -> Int -> Int -> IO (String)
_readLines value handle currentLine nLines =
    do isEof <- hIsEOF handle
       if isEof
           then return (value)
           else do
               line <- hGetLine handle
               if (currentLine == nLines) then
                   return (value ++ line)
               else
                  _readLines (value ++ line) handle (currentLine + 1) nLines

