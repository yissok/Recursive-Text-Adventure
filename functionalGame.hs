
{-
 #######
    #    #    # ######    #    #   ##   #####
    #    #    # #         ##  ##  #  #  #    #
    #    ###### #####     # ## # #    # #    #
    #    #    # #         #    # ###### #####
    #    #    # #         #    # #    # #
    #    #    # ######    #    # #    # #
-}

main=game


type Location = String
type Map      = [(Location,Location)]

accessible :: Location -> [Location]
accessible l = [ b | (a,b) <- theMap , a == l ] ++
               [ a | (a,b) <- theMap , b == l ]

{-
 #     #
 #  #  #  ####  #####  #      #####      ####   ####  #    # ##### ###### #    # #####  ####
 #  #  # #    # #    # #      #    #    #    # #    # ##   #   #   #      ##   #   #   #
 #  #  # #    # #    # #      #    #    #      #    # # #  #   #   #####  # #  #   #    ####
 #  #  # #    # #####  #      #    #    #      #    # #  # #   #   #      #  # #   #        #
 #  #  # #    # #   #  #      #    #    #    # #    # #   ##   #   #      #   ##   #   #    #
  ## ##   ####  #    # ###### #####      ####   ####  #    #   #   ###### #    #   #    ####
-}

type Object   = String
type Contents = [(Location,[Object])]

findContents :: Location -> Contents -> [Object]
findContents _ [] = []
findContents l ((k,xs):con)
    | l == k    = xs
    | otherwise = findContents l con

addContents :: Location -> [Object] -> Contents -> Contents
addContents _ _ [] = []
addContents l ys ((k,xs):con)
    | k == l    = (k, xs ++ ys) : con
    | otherwise = (k,xs) : addContents l ys con

    {-
 #####  ###### #    #  ####  #    # ######
 #    # #      ##  ## #    # #    # #
 #    # #####  # ## # #    # #    # #####
 #####  #      #    # #    # #    # #
 #   #  #      #    # #    #  #  #  #
 #    # ###### #    #  ####    ##   ######
    -}

removeOne :: Eq a => [a] -> a -> [a]
removeOne    []  _ = []
removeOne (x:xs) y
    | x == y    = xs
    | otherwise = x : removeOne xs y

removeAll :: Eq a => [a] -> [a] -> [a]
removeAll xs    []  = xs
removeAll xs (y:ys) = removeAll (removeOne xs y) ys

removeContents :: Location -> [Object] -> Contents -> Contents
removeContents _ _ [] = []
removeContents l ys ((k,xs):rest)
    | k == l    = (k, removeAll xs ys) : rest
    | otherwise = (k,xs) : removeContents l ys rest

    {-
  #####
 #     #  ####  #####  #####
 #       #    # #    #   #
  #####  #    # #    #   #
       # #    # #####    #
 #     # #    # #   #    #
  #####   ####  #    #   #
    -}

merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys)
    | x <= y    = x : merge xs (y:ys)
    | otherwise = y : merge (x:xs) ys

msort :: Ord a => [a] -> [a]
msort []  = []
msort [x] = [x]
msort xs  = merge (msort (take n xs)) (msort (drop n xs))
  where
    n = div (length xs) 2

equal :: [Object] -> [Object] -> Bool
equal xs ys = msort xs == msort ys


{-
  #####
 #     #   ##   #    # ######
 #        #  #  ##  ## #
 #  #### #    # # ## # #####
 #     # ###### #    # #
 #     # #    # #    # #
  #####  #    # #    # ######
-}

type Game  = (Location,[Object],Contents)

empty :: Game
empty =  ("",[],[])


type Event = Game -> Game

data MyGame = MyGame Location [Object] Contents

start :: Game
start = ("Nintendo Land",[],people)

mystart :: MyGame
mystart =  MyGame  "Nintendo Land"  [] people

instance Show MyGame where
    show = showGame

showGame :: MyGame -> String
showGame (MyGame l xs a) = "You are in " ++ l ++ ". You can travel to\n" ++ (enumerate 1 ys) ++ "With you are\n" -- ++ (enumerate 1 a)
  where
    ys = accessible l

go :: MyGame -> Int ->  MyGame
go (MyGame l xs a) x = MyGame ((accessible l)!!(x-1)) xs a

{-
 #######
 #       #    # ###### #    # #####  ####
 #       #    # #      ##   #   #   #
 #####   #    # #####  # #  #   #    ####
 #       #    # #      #  # #   #        #
 #        #  #  #      #   ##   #   #    #
 #######   ##   ###### #    #   #    ####
-}

receive :: [Object] -> Event
receive xs (loc,objs,cnts) = (loc,xs++objs,cnts)

deposit :: [Object] -> Event
deposit xs (loc,objs,cnts) = (loc,objs,addContents loc xs cnts)

remove :: [Object] -> Event
remove xs (loc,objs,cnts) = (loc,removeAll objs xs,removeContents loc xs cnts)


takeOutFromBring :: Location -> [Object] -> Contents -> Contents
takeOutFromBring _ _ []=[]
takeOutFromBring loc1 br ((loc2,objs):xy)
    | (loc1==loc2) = ((loc2,removeAll objs br):xy)
    | otherwise                = (loc2,objs):(takeOutFromBring loc1 br xy)


bring :: [Object] -> Event
bring br (loc,objs,cnts) =  (loc,objs++br,takeOutFromBring loc br cnts)


leave :: [Object] -> Event
leave br (loc,objs,cnts) =  (loc,removeAll objs br,addContents loc br cnts)

depositAtLocation :: [Object] -> Location -> Event
depositAtLocation ob loc (gloc,gobjs,gcnts) = (gloc,gobjs,chooseRightPlace ob loc gcnts)
chooseRightPlace :: [Object] -> Location -> Contents -> Contents
chooseRightPlace ob loc ((cloc,cobjs):cs)
    | loc==cloc = (cloc,ob++cobjs):cs
    | otherwise = (cloc,cobjs):chooseRightPlace ob loc cs

removeFromLocation :: [Object] -> Location -> Event
removeFromLocation ob loc (gloc,gobjs,gcnts) = (gloc,gobjs,removeRightPlace ob loc gcnts)
removeRightPlace :: [Object] -> Location -> Contents -> Contents
removeRightPlace ob loc ((cloc,cobjs):cs)
    | loc==cloc = (cloc,[]):cs
    | otherwise = (cloc,cobjs):removeRightPlace ob loc cs
{-
######
#     # #   ##   #       ####   ####  #    # ######
#     # #  #  #  #      #    # #    # #    # #
#     # # #    # #      #    # #      #    # #####
#     # # ###### #      #    # #  ### #    # #
#     # # #    # #      #    # #    # #    # #
######  # #    # ######  ####   ####   ####  ######



-}


asdf :: IO ()
asdf = putStrLn "Hello, World!"

data Dialogue = End     String  Event
              | Choice  String  [( String , Dialogue )]





dialogue :: Game -> Dialogue -> IO Game
dialogue g (End s e) = do
  putStrLn (s)
  return (e g)
dialogue g (Choice s d) = do
  putStrLn (s)
  putStrLn (enumerate 1 (map fst d))
  str<-getLine
  if (str=="X")||(str=="x")||(str=="Q")||(str=="q")||(str=="Quit")||(str=="quit")||(str=="Exit")||(str=="exit")
    then return g
    else do
      let k=read str :: Int
      dialogue g (snd (d !! (k-1)))





{-
    #
   # #    ####  ##### #  ####  #    #  ####
  #   #  #    #   #   # #    # ##   # #
 #     # #        #   # #    # # #  #  ####
 ####### #        #   # #    # #  # #      #
 #     # #    #   #   # #    # #   ## #    #
 #     #  ####    #   #  ####  #    #  ####
-}

type Action = ([Object],Dialogue)

findDialogue :: [Object] -> Dialogue
findDialogue []=End "Nothing to say." (doNothing ["Nothing"])
findDialogue o = findActions o actions

doNothing :: [Object] -> Event
doNothing _ g = g

findActions :: [Object] -> [Action] -> Dialogue
findActions [] _ = End "obj empty." (doNothing ["Nothing"])
findActions _ [] = End "actions empty." (doNothing ["Nothing"])
findActions obj1 ((obj2,dial):acts)
    | areListsEqual obj1 obj2 []      = dial
    | otherwise                       = findActions obj1 acts


areListsEqual :: Eq a => [a] -> [a] -> [a] -> Bool
areListsEqual [][][]=True
areListsEqual _ [][]=False
areListsEqual [] _[]=False
--areListsEqual [][] _=False
areListsEqual (x:xs) (y:ys) nScart
    | x==y                            = areListsEqual xs ys []                         -- 3) if you are lucky, this is just removing elements in order (left to right in both lists)
    | length (x:xs)<=length (nScart)  = False                -- 2) ...Until all the elements have been tested
    | otherwise                       = areListsEqual (rotate (x:xs)) (y:ys) (x:nScart) -- 1) try all combinations until (see above comment)

rotate :: Eq a => [a] -> [a]
rotate (x:xs)= reverse(x:reverse xs)--make the list move to the left and having the first element to go to the end every time




actions :: [Action]
actions =
 [ (["Mario"] , Choice "I need to save the Princess."
     [("Sure." , End "Let's go." (bring ["Mario"]))
     ,("Not right now." , End "Ok." (leave ["Mario"]))
     ])
 , (["Mario","Peach"] , Choice "Save me, Mario!"
    [("Sure." , End "Thank you for bringing me my hero. Now I can conveniently leave this hat behind." (deposit ["Baseball Cap"] . remove ["Mario","Peach"]))
    ,("Not right now." , End "Mario, pls." id)])
 , (["Master Chief"] , Choice "I want to marry Cortana. Can you escort us to the Church of Halo?"
     [("Sure." , End "Let's go." (bring ["Master Chief"]))
     ,("Not right now." , End "Ok." (leave ["Master Chief"]))
     ])
 , (["Cortana"] , Choice "I must go with Master Chief."
     [("Sure." , End "Let's go." (bring ["Cortana"]))
     ,("Not right now." , End "Ok." (leave ["Cortana"]))
     ])
 , (["Master Chief","Priest"] , End "I can't marry you without your bride-to-be." id)
 , (["Cortana","Priest"] , End "I can't marry you without your husband-to-be." id)
 , (["Cortana","Master Chief","Priest"] , End "What a beautiful wedding said the bridesmaid to the waiter. But what a shame, that there's some child lurking nearby." (remove ["Cortana","Master Chief","Priest"] . deposit ["Clementine (hiding)"]) )
 , (["Baseball Cap"] , Choice "It's a bit grubby, shall I take it?"
     [("Sure." , End "Let's go." (bring ["Baseball Cap"]))
     ,("Not right now." , End "Ok." (leave ["Baseball Cap"]))
     ])
 , (["Clementine (hiding)"] , End "I'm scared. Where are my parents?" id)
 , (["Baseball Cap", "Clementine (hiding)"] , Choice "Give the girl the hat?"
    [("Sure." , End "I feel safe." (receive ["Clementine"] . remove ["Clementine (hiding)","Baseball Cap"]))
    ,("Not right now." , End "" id)
    ])
 , (["Clementine"] , Choice "Will you help me find my parents?"
     [("This way." , End "Thanks!" (bring ["Clementine"]))
     ,("Not today." , End "Oh, okay." (leave ["Clementine"]))
     ])
  , (["Clementine","Lee"] , Choice "GIVE ME BACK CLEMENTINE!"
     [("Sure...", End "" (remove ["Lee","Clementine"] . receive ["Zombie Lee"]))
     ])
  , (["Lee"] , End "Clem? Clem, where are you?!" id)
  , (["Zombie Lee"] , Choice "Uuurrurrhgghghhghgg."
     [("This way."  , End "" (bring ["Zombie Lee"]))
     ,("Not today." , End "" (leave ["Zombie Lee"]))
     ])
  , (["Rochelle"] , End "Girl, you should pray there aren't no Zombies around." id)
  , (["Rochelle", "Zombie Lee"] , End "What?! A zombie? You've left me for dead!" (deposit ["Pikachu"] . remove ["Rochelle","Zombie Lee"]))
  , (["Chell"] , Choice "I've just got a volunteering position at Aperture Science. Can you help me find it? I'm not good with directions."
     [("This way."  , End "" (bring ["Chell"]))
     ,("Not today." , End "" (leave ["Chell"]))
    ])
  , (["Chell","Portal Gun"] , End "This is your fault. It didn't have to be like this. I'm not kidding, now! Turn back, or I will kill you! I'm going to kill you, and all the cake is gone! You don't even care, do you? This is your last chance! ." (remove ["Chell","Portal Gun"] . depositAtLocation ["Ash"] "Pallet Town" . removeFromLocation ["Team Rocket"] "Pallet Town" ))
  , (["Team Rocket"] , End "Oh, prepare for trouble, that's what they should do. And make it double, we're grabbing Pikachu." id)
  , (["Pikachu"] , Choice "Pika-Pika"
     [("*throw pokeball*"  , End "" (bring ["Pikachu"]))
     ,("Nope." , End "" (leave ["Pikachu"]))
     ])
  , (["Ash", "Pikachu"] , End "You win." (\_ -> ("Pallet Town",[],[])))
  , (["Portal Gun"]     , End "What I want for christmas" id)
  , (["Pikachu","Team Rocket"] , End "You lose. Bring Pikachu to his rightful owner." (\_ -> ("Pallet Town",[],[])))

 ]

{-
  #####
 #     #   ##   #    # ######    #       ####   ####  #####
 #        #  #  ##  ## #         #      #    # #    # #    #
 #  #### #    # # ## # #####     #      #    # #    # #    #
 #     # ###### #    # #         #      #    # #    # #####
 #     # #    # #    # #         #      #    # #    # #
  #####  #    # #    # ######    ######  ####   ####  #
-}


number :: String -> [Int]
number []=[]
number s= convOneByOne (words s)--read "123456" :: Int

convOneByOne :: [String] -> [Int]
convOneByOne []=[]
convOneByOne (x:xs)=(read x :: Int):convOneByOne xs

intToChar :: Int -> [Char]
intToChar a
    |a==0 = "0"


findSeeableObjects :: Location -> Contents -> [Object]
findSeeableObjects _ []=[]
findSeeableObjects l ((loc,ob):cns)
    | l==loc   = ob
    |otherwise = findSeeableObjects l cns

readMaybe :: String -> Maybe Int
readMaybe s = case reads s of
              [(x, "")] -> Just x
              _ -> Nothing

areTheyAllNumbersAndSpaces :: String -> Bool
areTheyAllNumbersAndSpaces []=True
areTheyAllNumbersAndSpaces (st:sts)
    | st==' '   = areTheyAllNumbersAndSpaces sts
    | otherwise = (Nothing /= (readMaybe (st:[]))) && (areTheyAllNumbersAndSpaces sts)

loop :: Game -> IO Game
loop (l,ys,cnts) =
  let
    xs = accessible l
    in do
  putStrLn ("You are in "++l)
  putStrLn ("You can travel to")
  putStr ( (enumerate 1 (xs)))
  putStrLn ("With you are")
  putStr ( enumerate (length xs+1) ys)
  putStrLn ("You can see")
  putStr ( enumerate (length xs + length ys+1) (findSeeableObjects l (cnts)))
  str <- getLine


  if (str=="X")||(str=="x")||(str=="Q")||(str=="q")||(str=="Quit")||(str=="quit")||(str=="Exit")||(str=="exit")
    then return (l,ys,cnts)
    else do
        --
        let (k:ks)=number str
        if (areTheyAllNumbersAndSpaces str)
        then do
          if k>(length xs)
            then do
              rep<-  (dialogue (l,ys,cnts) (findDialogue (getObjectAtIndex (length xs) (k:ks) (l,ys,cnts) )  ))
              loop(rep)
            else do
              loop (atIndex xs k,ys,cnts)
          loop (atIndex xs k,ys,cnts)

        else
          loop (l,ys,cnts)
          
game :: IO ()
game = do
  loop start
  return ()


getObjectAtIndex :: Int -> [Int] -> Game -> [Object]
getObjectAtIndex _ [] _ =[]
getObjectAtIndex xsLen (k:ks) (l,ys,cnts)
    | (length (k:ks)==1)&&(k>(xsLen+length ys))&&(k<(xsLen+length ys+length cnts)) = (getStringObjN (k-xsLen-1-length ys) (findSeeableObjects l (cnts)))
    | (length (k:ks)==1)&&(k>xsLen) &&(k<xsLen+length ys+2) = (getStringObjN (k-xsLen-(length ys )) ys)
    | (length (k:ks)> 1)&&(k>xsLen) &&(k<xsLen+length ys+1) = (getStringN (k-xsLen-1) ys):getObjectAtIndex xsLen ks (l,ys,cnts)
    |otherwise =[]

getStringObjN :: Int -> [String] -> [String]
getStringObjN n xs=xs!!n:[]

getStringN :: Int -> [String] -> String
getStringN n xs=xs!!n



{-
    #
   # #   #    # #    # # #      #   ##   #####  #   #    ###### #    # #    #  ####
  #   #  #    #  #  #  # #      #  #  #  #    #  # #     #      #    # ##   # #    #
 #     # #    #   ##   # #      # #    # #    #   #      #####  #    # # #  # #
 ####### #    #   ##   # #      # ###### #####    #      #      #    # #  # # #
 #     # #    #  #  #  # #      # #    # #   #    #      #      #    # #   ## #    #
 #     #  ####  #    # # ###### # #    # #    #   #      #       ####  #    #  ####
-}

exitWords = ["X","x","Q","q","Exit","exit","Quit","quit"]

enumerate :: Int -> [String] -> String
enumerate n xs = unlines [ "  " ++ show i ++ ". " ++ x | (i,x) <- zip [n..] xs ]

atIndex :: [String] -> Int -> String
atIndex xs k =  (xs !! (k-1))

{-
  ####    ##   #    # ######    #####    ##   #####   ##
 #    #  #  #  ##  ## #         #    #  #  #    #    #  #
 #      #    # # ## # #####     #    # #    #   #   #    #
 #  ### ###### #    # #         #    # ######   #   ######
 #    # #    # #    # #         #    # #    #   #   #    #
  ####  #    # #    # ######    #####  #    #   #   #    #
-}




people :: Contents
people = [ ("Macon"            , ["Lee"] )
         , ("Pallet Town"      , ["Team Rocket"] )
         , ("Princess Castle"  , ["Rochelle","Peach"] )
         , ("Aperture Science" , ["Portal Gun"])
         , ("Church of Halo"   , ["Priest"])
         , ("Nintendo Land"    , ["Chell","Cortana","Mario","Master Chief"])
         ]

theMap :: Map
theMap =
  let
    b = "Macon"
    c = "Pallet Town"
    k = "Princess Castle"
    l = "Aperture Science"
    t = "Church of Halo"
    w = "Nintendo Land"
  in [(c,l), (c,w), (w,t), (t,k), (k,b)]