
--zipper idea: have your parser go from a zipper to a zipper...
--{-# LANGUAGE OverloadedStrings #-}
--Q: how much does having caesura constraint help?
--use the non-determinisn to allow for guessing about greek diphthongs etc. try including caesuras, eithert monad transformer, make it deal
--with partial parsing so that streams will work, use parser on euler79, make it learn stress paterns of latin, making small non determinism
--extend: e.g. eitherP x y, when in a part of bigger thing z, makes one z with x and one with y, ELISION, fix qu/th problem: hard
--fix lookahead, make position making rules more complete
--make it unsupervised: learn more about unsupervised
import Text.Show.Pretty
import Control.Lens
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Except
import Control.Monad.Trans.List
import Control.Monad.Trans.Writer
import Control.Monad.Identity
import Control.Applicative
import Data.List
import Control.Arrow
--import Control.Lens
import Network.HTTP.Conduit (simpleHttp)
import qualified Data.ByteString.Lazy.Char8 as L
import Text.XML.HXT.Core
import Text.HandsomeSoup
import qualified Data.Text as T
import Data.Char
import Data.Monoid
import qualified Data.Map as M
import Database.HDBC.Sqlite3 (connectSqlite3)
import Database.HDBC
--Parsers are directly functions here, into a monad transformer. MaybeT for failure, WriterT for monoidal action in binding,
--ListT for nondet.
type Parser a b = [a] -> ExceptT [Char] (WriterT [b] (ListT Identity)) [a]
data Syllable = Long | Short | Unknown deriving (Show,Eq,Ord)
data Foot =  Spondee | Dactyl deriving (Show, Eq,Ord)
type UberM a b = ExceptT (WriterT [b] (ListT Identity) [a])

instance Monoid SqlValue where 
  mempty = toSql ""
  mappend a b = toSql $ (fromSql :: SqlValue -> String) a ++ (fromSql :: SqlValue -> String) b


--ugly code to get inside data structures and stuff...cause I don't quite get how to do it properly yet...
uberLens f = bung . (fmap $ fmap f) . unbung
isSuc = (any isBar) . unbung
isBar x = case x of
        (Right _ ,_) -> True
        otherwise -> False
unbung = runIdentity . runListT . runWriterT . runExceptT
bung x = ExceptT $ WriterT $ ListT $ Identity x
parsed (Right [] , _) = True 
parsed _ =  False

--parser combinator library:

--basic function to make a parser: takes a criterion and a potential wrapper
genP :: (a->b) -> (a -> Bool) -> Parser a b
genP g f y = if null y then bung [(Left "Empty list.",[])] else if (f $ head y) then bung [(Right $ tail y, [g $ head y])] 
        else bung [(Left "No match.",[])] 
sequenceP :: [Parser a b] -> Parser a b
sequenceP x = (foldr1 (<=<) (reverse x))
bothP :: Parser a b -> Parser a b -> Parser a b
bothP a b = liftA2 (\x y -> bung $ filter isBar ((unbung x) ++ (unbung y))) a b
--andP :: Parser a b -> Parser a c -> Parser a b
--andP x y = (\z -> if (isSuc (x z)) || (isSuc $ y z) then x z else x z)
anyOfP :: Eq a => [a] -> Parser a a
anyOfP y = genP id (\x -> x `elem` y)
times :: Int -> Parser a b -> Parser a b
times x y = (notAny:((iterate (y <=<) y))) !! x
more :: Parser a b -> Parser a b
more x = try (notAny:((iterate (x <=<) x)))
atLeastP :: Int -> Parser a b -> Parser a b
atLeastP x y = try $ drop (x-1) ((iterate (y <=<) y))
try :: [Parser a b] -> Parser a b
try (y:ys) = (\z -> if not $ isSuc $ (head ys) z then y z else (try ys) z) 
alt :: Parser a b -> Parser a b -> Parser a b
alt x y = \z -> let w = (x z) in if isSuc (w) then w else y z
isNot :: Parser a b -> Parser a b -> Parser a b
isNot x y = (\z -> if isSuc $ x z then bung [(Left "Not",[])] else y z) 
alignP :: ([b]->[c]) -> Parser a b -> Parser a c
alignP f x = (\z -> uberLens f $ x z)
nameP :: c -> Parser a b -> Parser a ([b],c)
nameP name x = alignP (\y -> [(y, name)]) x 
lookahead :: Parser a b -> Parser a b
lookahead x = (\z -> if isSuc $ x z then bung [(Right z, [])] else bung [(Left "No lookahead match", [])])
notAny :: Parser a b
notAny = (\z -> bung [(Right z,[])])
space :: Parser Char Char
space = anyOfP " "
anyP x = alt x notAny

vowels = "aeiouy"
sibilants = "mnr"
plosives = "ptkc"
consonants = "bcdfghjklmnpqrstvwxz"
punctuation = ".,;:?!-"
letters = vowels ++ consonants

initialglide = sequenceP [anyOfP "iu",lookahead $ anyOfP vowels]
diphthong :: Parser Char Char
diphthong = foldl1 alt [(anyOfP "i") <=< (anyOfP "aeou"), 
        (anyOfP "eu") <=< (anyOfP "ao"), (anyOfP "u") <=< (anyOfP "e") ]

conson :: Parser Char Char
conson = foldr1 alt [ (anyOfP "u") <=< (anyOfP "q"), (anyOfP "h") <=< (anyOfP "pt") , (anyOfP consonants)] --, initialglide]
elision :: Parser Char Char
elision = alignP ('J':) $ sequenceP [alt diphthong $ anyOfP vowels, alt (anyOfP "m") notAny, lookahead $ sequenceP [space, alt (anyOfP "h") notAny, anyOfP vowels, isNot (lookahead $ anyOfP vowels) (notAny)]]
nucleus :: Parser Char Char
nucleus = sequenceP [anyOfP vowels]
onset = sequenceP [alt (sequenceP [space, anyOfP "i", lookahead $ anyOfP vowels]) notAny, alt space notAny, more conson]

hasdiphthong = sequenceP [onset, diphthong, more conson]
makesposition = sequenceP [onset, nucleus, isNot (sequenceP [bothP (anyOfP plosives) (sequenceP [anyOfP "t", anyOfP "h"]), anyOfP sibilants]) notAny, 
        conson, alt (atLeastP 1 (foldr1 alt [(anyOfP "u") <=< (anyOfP "q"), 
                conson])) (lookahead $ sequenceP [space, alt conson initialglide])]


finalvowel = sequenceP [onset, anyP elision, anyOfP "uo", lookahead space]

long :: Parser Char Char
long = sequenceP [foldr1 alt [hasdiphthong, makesposition, finalvowel], anyP elision]
unknown :: Parser Char Char
unknown = isNot long $ sequenceP [onset,
        nucleus, more conson, anyP elision]

maybelong = nameP Long $ alt (long) (unknown)
resolvable = bothP (maybelong) ((nameP Short unknown) <=< (nameP Short unknown))
--keeps caesura rule: but should add late caesura acceptance
coda1 = nameP "C1" $ sequenceP [times 2 $ (resolvable <=< maybelong), nameP Long $ alt (sequenceP [onset,elision]) (sequenceP [alt long unknown,lookahead space]) ]
coda2 = nameP "C2" $ sequenceP [times 2 (maybelong <=< resolvable), nameP Short unknown, nameP Short unknown, maybelong , maybelong]
--hepthemimeral caesura
coda3 = nameP "C3" $ sequenceP [times 3 $ (resolvable <=< maybelong), nameP Long $ sequenceP [alt long unknown, lookahead space] ]
coda4 = nameP "C4" $ sequenceP [times 1 (maybelong <=< resolvable), nameP Short unknown, nameP Short unknown, maybelong , maybelong]
--that weird greek mid dactyllic foot caesura
coda5 = nameP "C5" $ sequenceP [times 2 $ (resolvable <=< maybelong), maybelong, nameP Short $ sequenceP [unknown, lookahead space] ]
coda6 = nameP "C6" $ sequenceP [(maybelong <=< (nameP Short unknown)),(maybelong <=< resolvable), nameP Short unknown, nameP Short unknown, maybelong , maybelong]
hexameter = foldr1 alt [(coda2 <=< coda1), (coda4 <=< coda3), (coda6 <=< coda5)]
--hexameter = [times 4 $ (resolvable <=< maybelong), maybelong, unknown, unknown, maybelong, maybelong]
--parses sequence of longs and shorts into dactyls and spondees
syllables = more (alt (nameP Long long) (nameP Unknown unknown))
dactyl = nameP Dactyl $ (genP id (==Short)) <=< (genP id (==Short)) <=< (genP id (==Long))
spondee = nameP Spondee $ (genP id (==Long)) <=< (genP id (==Long))
metre = sequenceP [times 5 $ alt dactyl spondee, spondee]
scan = more maybelong

process = id &&& ((getMetre &&& ((return :: a -> [a]) . look)) . hexameter)

scansion = (\x -> if null x then [] else head x) . fmap ((foldr mappend []) . fst . unzip . snd) . listTolist  . filter parsed . unbung . hexameter

listTolist :: [a] -> [a]
listTolist x 
	|length x == 1 = x
	|otherwise = []

url = "http://www.thelatinlibrary.com/vergil/aen1.shtml"
url2 = "http://www.thelatinlibrary.com/vegius.html"
library = aeneid ++ eclogues ++ georgics ++ met ++ persius ++ ennius ++ statius
--totalNumberOfLines : select count (*) from lines l where not l.scansion = "[]"

virgil = aeneid ++ eclogues ++ georgics
aeneid = ["http://www.thelatinlibrary.com/vergil/aen" ++ show x ++ ".shtml" | x <- [1..12]]
eclogues = ["http://www.thelatinlibrary.com/vergil/ec" ++ show x ++ ".shtml" | x <- [1..10]]
georgics = ["http://www.thelatinlibrary.com/vergil/geo" ++ show x ++ ".shtml" | x <- [1..4]]
lucretius = ["http://www.thelatinlibrary.com/lucretius/lucretius" ++ show x ++ ".shtml" | x <- [1..5]] 
catullus = ["http://www.thelatinlibrary.com/catullus.shtml"]
persius = ["http://www.thelatinlibrary.com/persius.html"]
met = ["http://www.thelatinlibrary.com/ovid/ovid.met" ++ show x ++ ".shtml" | x <- [1..15]]
ennius = ["http://www.thelatinlibrary.com/enn.html"]
horace = sermones ++ epist ++ arspo
latiniliad = ["http://www.thelatinlibrary.com/ilias.html"]
flaccus =  ["http://www.thelatinlibrary.com/valeriusflaccus" ++ show x ++ ".html" | x <- [1..8]]
juvenal =  ["http://www.thelatinlibrary.com/juvenal/" ++ show x ++ ".shtml" | x <- [1..16]]

vida = ["http://www.thelatinlibrary.com/vida.html"]
silviusitalicus = ["http://www.thelatinlibrary.com/silius/silius" ++ show x ++ ".shtml" | x <- [1..15]]
sermones = ["http://www.thelatinlibrary.com/horace/serm"  ++ show x ++ ".shtml" | x <- [1..2]]
epist = ["http://www.thelatinlibrary.com/horace/epist"  ++ show x ++ ".shtml" | x <- [1..2]]
arspo = ["http://www.thelatinlibrary.com/horace/arspoet.shtml"]
lucan =  ["http://www.thelatinlibrary.com/lucan/lucan" ++ show x ++ ".shtml" | x <- [1..10]]
statius = thebaid ++ achilleid ++ silvae
thebaid = ["http://www.thelatinlibrary.com/statius/theb" ++ show x ++ ".shtml" | x <- [1..10]]

achilleid = ["http://www.thelatinlibrary.com/statius/silvae" ++ show x ++ ".shtml" | x <- [1..5]]
silvae = ["http://www.thelatinlibrary.com/statius/achilleid" ++ show x ++ ".shtml" | x <- [1..2]]

cluny = ["http://www.thelatinlibrary.com/bernardcluny2.html"]

analyse u = do
    a <- simpleHttp u
    let doc = readString [withParseHTML yes, withWarnings no] $ L.unpack a
    links <- runX $ doc >>> (css "body") //> getText
    return $ map ((' ':) . map toLower . (filter (not . (`elem` "\160,:;.?'\"\n"))) )  
      . (drop 1) . ((filter ((>17) . length))) $ links
comb [x] = Right $ snd x
comb [] = Left "No solutions."
comb x = Left "Too many solutions."
comb2 :: [a] -> Either String a
comb2 [x] = Right x
comb2 [] = Left "No solutions."
comb2 y = Left "Too many solutions."
look = comb . (filter parsed) . unbung

getMetre = join . (comb2 . (fmap ((fmap (snd . unzip)) . look . metre)) . 
        nub . fmap (concat . (fmap (snd . unzip)) . fst . unzip . snd)) . (filter parsed) . unbung


ls = ["arma virumque cano troiae qui primus ab oris", 
        "stulte quid est somnus gelidae nisi mortis imago",       
        "musa mihi causas memora quo numine laeso" ,
        "in nova fert animus mutatas dicere formas",
        "invitam o regina tuo de litore cessi",
        "sed tuus altus amor barathro fuit altior illo",
        "certabant urbem romam remoramne vocarent",
        "non quivis videt inmodulata poemata iudex",
        "quadrupedante putrem sonitu quatit ungula campum", 
        "aeneadum genetrix, hominum divomque voluptas"]


create :: String -> IO ()
create db = do
  conn <- connectSqlite3 db
  run conn "CREATE TABLE scannedToken (id INTEGER PRIMARY KEY AUTOINCREMENT, token TEXT NOT NULL, weight TEXT, stress TEXT, pos TEXT, line INT, FOREIGN KEY (line) REFERENCES lines(id))" []
  run conn "CREATE TABLE scannedSyllable (id INTEGER PRIMARY KEY AUTOINCREMENT, syl TEXT NOT NULL, prom TEXT, stress TEXT, pos INT, line INT, FOREIGN KEY (line) REFERENCES lines(id))" []
  run conn "CREATE TABLE lines (id INTEGER PRIMARY KEY AUTOINCREMENT, author TEXT NOT NULL, position TEXT NOT NULL, line TEXT, scansion TEXT)" []
  commit conn
  disconnect conn

insertion :: String -> String -> String -> String -> IO ()
insertion db author position line = do
  conn <- connectSqlite3 db
  run conn "INSERT INTO lines (author, position,line,scansion) VALUES (?,?,?,?)" (map toSql [author,position, line, (show $ scansion line)])
  [[p]] <- quickQuery' conn "SELECT last_insert_rowid()" []
  if not (null (munge line)) then do 
    statement <- prepare conn  "INSERT INTO scannedSyllable (syl, prom, stress, pos, line) VALUES (?, ?, ?, ?, ?)" 
    executeMany statement $ map (++[p]) (munge line)
    statement2 <- prepare conn  "INSERT INTO scannedToken (token, weight, stress, pos, line) VALUES (?, ?, ?, ?, ?)" 
    executeMany statement2 $ map (++[p]) (wordsOf line) 
    else return ()
  commit conn
  disconnect conn



--derives words from syllables: keeps length
combine (a:b) = (unzip $ a : (fst z)) : (combine $ snd z)
  where z = (break ((== ' ') . head . fst) b)
combine x = [unzip x]

--derives word stress from syllables
--you need to fix the first syllable of word
--doesn't work at all: fix it then try queries
stressOf = reverse . stressOf' . reverse
stressOf' (a:b:c:d) 
  | (head . fst) a == ' ' = (a,"H") : (stressOf' (b:c:d))
  | (head . fst) b == ' ' = [(a,"L"), (b,"H")] ++ (stressOf' (c:d))
  | otherwise = if (snd b) == Long then [(a,"L"), (b,"H"), (c,"L")] ++ (stressOf' (d)) else [(a,"L"), (b,"L"), (c,"H")] ++ (stressOf' (d))
stressOf' (a:b:[]) = if (head . fst) a == ' ' then [(a,"H"),(b,"H")] else [(a,"L"),(b,"H")]
stressOf' (a:[]) = [(a,"L")]
stressOf' [] = []

breaks :: Show a => a -> IO ([[SqlValue]], [[SqlValue]])
breaks n = do
  conn <- connectSqlite3 "hexameters.db"
  p <- quickQuery' conn ("select s.syl,l.line,l.author from lines l join scannedSyllable s on (l.id = s.line) where s.pos = " ++(show n)++ " and s.syl like \" %\" group by s.syl") []
  q <- quickQuery' conn ("select l.id, l.line from lines l join scannedSyllable s on (l.id = s.line) where s.pos = " ++(show n)++ " and not s.syl like \" %\"") []
  commit conn
  disconnect conn
  return (p,q)



--gives all the caesuras and bridges in a line
distribution = fmap (zip [0..] . map perc) $ mapM ( test) [0..23]
  where test n = fmap ((length .  fst) &&& (length . snd)) (breaks n) 
perc (x,y) = (((fromIntegral x) / 38146) * 100, ((fromIntegral y) / 38146) * 100)
--pairwise covariance of caesuras
correlation c d = do; x <- a; y <- b; return $ length $ intersect (fst x) (fst y)
  where (a,b) = (breaks c, breaks d)
--baseline distribution of syllables
baseline =  fmap (zip [0..]) $ mapM foo [0..23]
  where foo = fmap length . (sylCount "hexameters.db") 

--to do: write script to compare results to the baseline as a percentage...

stresses = fmap (zip [0..]) $ mapM (fmap ((length .  fst) &&& (length . snd)) . stressTest "hexameters.db") [0..23]

normStress = do
  x <- stresses
  y <- baseline
  let z = zip (map snd x) (map snd y)
  return $ (zip [0..23] . map (\((i,j),k) -> ((fromIntegral (i)) / (fromIntegral (k+1)), (fromIntegral (j)) / (fromIntegral (k+1))  ))) z

stressTest :: FilePath -> Int -> IO ([[SqlValue]],[[SqlValue]])
stressTest db num = do
  conn <- connectSqlite3 db
  p <- quickQuery' conn ("select s.syl, l.line, l.id from lines l join scannedSyllable s on (l.id = s.line)"
    ++ " where (s.pos = " ++ show num ++ " and s.stress = \"H\")" ) []
  q <- quickQuery' conn ("select s.syl, l.line, l.id from lines l join scannedSyllable s on (l.id = s.line)"
    ++ " where (s.pos = " ++ show num ++ " and s.stress = \"L\" )" ) []
  commit conn
  disconnect conn
  return (p,q)

sylCount db num = do
  conn <- connectSqlite3 db
  r <- quickQuery' conn ("select s.syl, l.line, l.id from lines l join scannedSyllable s on (l.id = s.line)"
    ++ " where (s.pos = " ++ show num ++ ")") []
  commit conn
  disconnect conn
  return r

wordPos db num = do
  conn <- connectSqlite3 db
  r <- quickQuery' conn ("select s.token, l.line from lines l join scannedToken s on (l.id = s.line)"
    ++ " where (s.pos = " ++ show num ++ " and s.stress like \"H%\") and s.weight like \"LongShort%\" ") []
  commit conn
  disconnect conn
  return r

--number of elisions
wevs = do
  conn <- connectSqlite3 "hexameters.db"
  r <- quickQuery' conn ("select count (s.syl) as c, l.line,l.author from scannedSyllable s join lines l on (l.id = s.line)"
    ++ " where (s.syl like \"%j%\") group by s.line order by c") []
  commit conn
  disconnect conn
  return r


mevs num = do
  conn <- connectSqlite3 "hexameters.db"
  r <- quickQuery' conn ("select s.stress,s.pos from scannedSyllable s where s.line = \"" ++ show num ++ "\"") []
  commit conn
  disconnect conn
  return r

elisions :: FilePath -> Int -> IO [[SqlValue]]
elisions db num = do
  conn <- connectSqlite3 db
  p <- quickQuery' conn ("select s.syl, l.line, l.id from lines l join scannedSyllable s on (l.id = s.line)"
    ++ " where (s.pos = " ++ show (num-2) ++ " and s.prom = \"Long\" and s.syl like \"%j%\" )") []
  q <- quickQuery' conn ("select s.syl, l.line, l.id from lines l join scannedSyllable s on (l.id = s.line)"
    ++ " where (s.pos = " ++ show (num-1) ++ " and s.prom = \"Short\" and s.syl like \"%j%\" )") []
  commit conn
  disconnect conn
  return (p++q)

wordSearch db = do
  conn <- connectSqlite3 db
  --p <- quickQuery' conn ("select l.line, s.syl from lines l join scannedSyllable s on (l.id = s.line)"
     -- ++ " where (s.pos = 10 and s.syl like \" %\") ") []
  --q <- quickQuery' conn ("select l.line, s.syl from lines l join scannedSyllable s on (l.id = s.line)"
     -- ++ " where (s.pos = 11 and s.syl like \" %\") ") []
    -- ++ " where (s.token = \" " ++ word ++ "\") ") []
        --and s.weight like \"LongLong%\") " )[]
    --elision
  p <- quickQuery' conn ("select l.author,l.line, s.token from lines l join scannedToken s on (l.id = s.line)"
    ++ " where s.pos = 8" ) []
  --q <- quickQuery' conn ("select s.stress from lines l join scannedToken s on (l.id = s.line) where s.stress = \"HL\" ") []

  commit conn
  disconnect conn
  return p

loadDatabase db x = do
    ((analyse x) >>= (mapM_ (insertion db x "1")))


conv' :: (([Char], Syllable),[Char]) -> [SqlValue]
conv' ((x,y),z) = [toSql x, (toSql . show) y, toSql z]

    --seems to cut off syllables
munge x = map (\(x,y) -> x ++ [y]) $ zip (map conv' $ stressOf $ scansion x) (map (toSql . show) $ init $ scanl (+) 0 $ map (\(x,y) -> f y) $ scansion x)
  where f z = if z == Long then 2 else 1

--merge :: 
wordsOf =  map  ((\(x,y) -> x++[y]) . (map (foldr1 mappend) . transpose .  map init &&& last . head)) . groupBy merge . munge
--wordsOf = map (map (foldr1 mappend) . transpose) . groupBy merge . munge
--(init &&& tail)
merge a b = f b
    where f x = head ((fromSql :: SqlValue -> String) (head x)) /= ' '

problem = " italiam fato profugus laviniaque venit" -- really you need both consonant and vocalic guess for post consonantal i
problem2 = "hic currus fuit hoc regnum dea gentibus esse" --"it ho" makes position
problem3 = "hunc ego sopitum somno super alta cythera"
problem4 = "adparent rari nantes in gurgite vasto"
problem5 = "vi superum saevae memorem iunonis ob iram"
problem6 = "haec secum mene incepto desistere victam"
problem7 = "et genus invisum et rapti ganymedis honores"
problem8 = "progeniem sed enim troiano a sanguine duci"
problem9 =  "talia flammato secum dea corde volutans"
