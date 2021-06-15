import Parser ( Parser(parse) )
import Json ( json )
import Xml ( xml )

test :: Show a => Parser a -> [String] -> IO ()
test parser files = mapM ((\r -> do print r; putStrLn "") . parse parser) files >>= print

main :: IO ()
main = do
    let jsons = ["test.json", "test2.json"]
    jfs <- mapM (readFile . ("test/"++)) jsons
    test json jfs

    let xmls = ["test.xml", "test2.xml"]
    xfs  <- mapM (readFile . ("test/"++)) xmls
    test xml xfs
