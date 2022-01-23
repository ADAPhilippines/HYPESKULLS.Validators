import Prelude
import System.Random
import Data.List

main :: IO ()
main = do
    let shuffled = shuffle vrtList vtList $ mkStdGen 123
    verifyShuffle shuffled
    writeFile "shuffled.tsv" $ "VRT\tVT1\tVT2\n" ++ concatMap (\(vrt, vt, vt') -> vrt ++ "\t" ++ vt ++ "\t" ++ vt' ++ "\n") shuffled

verifyShuffle :: [(String, String, String)] -> IO ()
verifyShuffle shuffled = do
    putStrLn $ "Unique VRTs Found: " ++ show (length (nub [ vrt | (vrt, _, _) <- shuffled]))
    putStrLn $ "Same VT pairings: " ++ show (length $ filter (\(_, vt, vt') -> vt == vt') shuffled)
    putStrLn $ unlines $ map f vtList
        where
            f (n, vt)   = "found " ++ vt ++ ": " ++ show n ++ "/" ++ show (c vt) ++ eq n (c vt)
            c' cvt      = length $ filter (\(_, vt, vt') -> vt == cvt || vt' == cvt) shuffled
            c'' cvt     = length $ filter (\(_, vt, vt') -> vt == cvt && vt' == cvt) shuffled
            c cvt       = c' cvt + c'' cvt
            eq  n n'    = if n == n' then " ✅" else " ❌"
            

shuffle :: RandomGen g => [String] -> [(Int, String)] -> g -> [(String, String, String)]
shuffle vrts vts g = shuffle' vrts vts g []
    where
        shuffle' [] _ _ acc         = acc
        shuffle' _ [] _ acc         = acc
        shuffle' vrts' vts' g' acc  = shuffle' vrts'' vts'' g'' ((vrt, vt, vt'):acc)
            where
                ((vrt,vt,vt'), vrts'', vts'', g'') = pair3 vrts' vts' g'


pair3 :: RandomGen g => [String] -> [(Int, String)] -> g -> ((String, String, String), [String], [(Int, String)], g)
pair3 vrts vts g = ((vrt, vt, vt'), vrts', vts'', g''')
    where
        ((_, vt), g')       = getRandom vts g
        vts'                = remove' vt vts
        ((_, vt'), g'')     = getRandom vts' g'
        vts''               = remove' vt' vts'
        (vrt, g''')         = getRandom vrts g''
        vrts'               = remove vrt vrts

getRandom :: RandomGen g => [a] -> g -> (a, g)
getRandom xs g = (xs !! rIdx, g')
    where
        (rIdx, g') = randomR (0, length xs - 1) g


remove :: Eq a => a -> [a] -> [a]
remove x xs = [ x' | x' <- xs, x /= x' ]

remove' :: String -> [(Int, String)] -> [(Int, String)]
remove' vtn vts =   [ (if vtn == vtn' then n - 1 else n, vtn')
                    | (n, vtn') <- vts
                    , vtn' /= vtn || (vtn' == vtn && n > 1)]


vtList :: [(Int, String)]
vtList = 
    [ (226, "HYPESKULLS_VT_AD_C") 
    , (112, "HYPESKULLS_VT_AD_E") 
    , (73, "HYPESKULLS_VT_AD_EE")   
    , (130, "HYPESKULLS_VT_AC_C")   
    , (67, "HYPESKULLS_VT_AC_E") 
    , (30, "HYPESKULLS_VT_AC_EE")   
    , (207, "HYPESKULLS_VT_G_C")   
    , (104, "HYPESKULLS_VT_G_E") 
    , (61, "HYPESKULLS_VT_G_EE")    
    , (160, "HYPESKULLS_VT_K_C")   
    , (82, "HYPESKULLS_VT_K_E") 
    , (39, "HYPESKULLS_VT_K_EE")    
    , (90, "HYPESKULLS_VT_M_C")   
    , (52, "HYPESKULLS_VT_M_E") 
    , (19, "HYPESKULLS_VT_M_EE")    
    , (193, "HYPESKULLS_VT_N_C")   
    , (96, "HYPESKULLS_VT_N_E") 
    , (51, "HYPESKULLS_VT_N_EE")    
    , (147, "HYPESKULLS_VT_P_C")   
    , (74, "HYPESKULLS_VT_P_E") 
    , (35, "HYPESKULLS_VT_P_EE")     
    , (240, "HYPESKULLS_VT_R_C")   
    , (121, "HYPESKULLS_VT_R_E") 
    , (86, "HYPESKULLS_VT_R_EE")     
    , (178, "HYPESKULLS_VT_V_C")   
    , (89, "HYPESKULLS_VT_V_E") 
    , (44, "HYPESKULLS_VT_V_EE")     
    , (111, "HYPESKULLS_VT_Z_C")   
    , (59, "HYPESKULLS_VT_Z_E") 
    , (24, "HYPESKULLS_VT_Z_EE")   
    ]

vrtList :: [String]
vrtList = ["HYPESKULLS_VRT_" ++ pad0 i | i <- [1..1500]]
    where
        pad0 n  = replicate (4 - length (nStr n)) '0' ++ nStr n
        nStr n  = show n


shadowHsList :: [String]
shadowHsList = ["HYPESKULL" ++ pad0 i ++ "_SH" | i <- [1..50]]
    where
        pad0 n  = replicate (4 - length (nStr n)) '0' ++ nStr n
        nStr n  = show n