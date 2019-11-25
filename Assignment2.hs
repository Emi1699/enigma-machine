{-- !!!IMPORTANT!!!
-- it is assumed that inputs for all the functions will be supplied correctly (i.e. Int as Int, Char as Char, etc., and all UPPERCASE)
--       !!!
-- AUTHOR: BULIGA FANEL EMANUEL
--}
module Assignment2 where
    import AssignmentHelp
    import Data.Char

    type Rotor = Cipher
    type Reflector = [(Char, Char)]
    type Offsets = (Int, Int, Int)
    type Steckerboard = Reflector -- although in reality they are a bit different, they're the same thing in terms of functionality
    type Crib = (String, Cipher)
    type IndexList = [(Int, (Char, Char))] -- one crib's letters and their positions
    type Menu = [Int]
    type Index = Int
    type Indexes = [Index]

    data Enigma = SimpleEnigma Rotor Rotor Rotor Reflector Offsets | 
                 SteckeredEnigma Rotor Rotor Rotor Reflector Offsets Steckerboard

    my_enigma = SimpleEnigma rotor1 rotor2 rotor3 reflectorB (0, 0, 0) -- instance of Enigma (simple)
    my_enigma2 = SteckeredEnigma rotor1 rotor2 rotor3 reflectorB (0, 0, 0) steckerboard2 -- instance of Enigma (steckered)

    -- given a character and an Enigma, returns the encoded character
    enigmaEncode :: Char -> Enigma -> Char
    -- SimpleEnigma function
    enigmaEncode c (SimpleEnigma lr mr rr reflector (ol, om, or)) = reverseEncode c6 rr orNew
     where (olNew, omNew, orNew) = advanceRotors (ol, om, or)
           -- go through the rotors, from right to left
           c1 = (encode c rr orNew)
           c2 = (encode c1 mr omNew)
           c3 = (encode c2 lr olNew)
           -- swap letter according to the reflector's wiring
           c4 = (reflect c3 reflector)
           -- go through the rotors, from left to right
           c5 = (reverseEncode c4 lr olNew)
           c6 = (reverseEncode c5 mr omNew)

    -- SteckeredEnigma function
    -- it is assumed that a steckerboard can have up to 10 pairs of letters, with 6 letters remaining unpaired
    enigmaEncode c (SteckeredEnigma lr mr rr reflector (ol, om, or) steckerboard) = reflect c8 steckerboard
     where (olNew, omNew, orNew) = advanceRotors (ol, om, or)
           -- swap letter according to the steckerboard's wiring
           c1 = reflect c steckerboard
           -- right -> left
           c2 = encode c1 rr orNew
           c3 = encode c2 mr omNew
           c4 = encode c3 lr olNew
           -- swap
           c5 = reflect c4 reflector
           -- left -> right
           c6 = reverseEncode c5 lr olNew
           c7 = reverseEncode c6 mr omNew
           c8 = reverseEncode c7 rr orNew

    -- given a message and an Enigma, returns the encoded message
    enigmaEncodeMessage :: String -> Enigma -> String
    enigmaEncodeMessage [] _ = []
    -- SimpleEnigma
    enigmaEncodeMessage (y:ys) (SimpleEnigma lr mr rr reflector (ol, om, or)) =
          enigmaEncode y (SimpleEnigma lr mr rr reflector (ol, om, or)) : 
                    enigmaEncodeMessage ys (SimpleEnigma lr mr rr reflector (olNew, omNew, orNew))
          where (olNew, omNew, orNew) = advanceRotors (ol, om, or)

    -- SteckeredEnigma
    enigmaEncodeMessage (y:ys) (SteckeredEnigma lr mr rr reflector (ol, om, or) steckerboard) =
          enigmaEncode y (SteckeredEnigma lr mr rr reflector (ol, om, or) steckerboard) : 
                    enigmaEncodeMessage ys (SteckeredEnigma lr mr rr reflector (olNew, omNew, orNew) steckerboard)
          where (olNew, omNew, orNew) = advanceRotors (ol, om, or)
    -- both of these methods have been tested with the message posted on Blackboard

    -- given a crib, returns the longest menu
    -- there might be multiple menus of same length, but this function only returns one of them
    longestMenu :: Crib -> Menu
    longestMenu crib = foldr (\f x -> if length f > length x then f else x) [] (getAllMenus crib)
    -- tested on the crib given as example in the assignment file

    -- encodes 1 character based on one rotor's configuration and its corresponding offset
    -- this method encapsulates the functionality of each rotor
    encode :: Char -> Rotor -> Int -> Char
    encode c (x:xs) offset
     | (pc + offset) `mod` 26 == pca `mod` 26 = chr ((ph - offset) `mod` 26 + 65)
     | otherwise = encode c xs offset
     where pca = alphaPos (currentAlphabetChr (x:xs))
           ph = alphaPos x
           pc = alphaPos c

    -- after going through the reflector, the signal must go back through the rotors
    -- this is equivalent to encoding the letter backwards
    reverseEncode :: Char -> Rotor -> Int -> Char
    reverseEncode _ [] _ = '\00' --could have used Maybe Char as return type, but that meant altering code that works fine for not that much benefit
    reverseEncode c (x:xs) offset
     | q `mod` 26 == p = chr ((pca - offset) `mod` 26 + 65)
     | otherwise = reverseEncode c xs offset
     where p = alphaPos x
           q = alphaPos c + offset
           pca = alphaPos (currentAlphabetChr (x:xs))

    -- performs a letter-swap, given a character and either a reflector (R) or a steckerboard (S)
    -- R and S are basically the same thing, except for a few unpaired letters (the second line in the function takes care of this)
    reflect :: Char -> Reflector -> Char
    reflect c [] = c
    reflect c ((x, y):z)
     | c == x = y
     | c == y = x
     | otherwise = reflect c z

    -- changes offsets accordingly before encoding 1 character
    advanceRotors :: Offsets -> Offsets
    advanceRotors (ol, om, or) =
     if or == 25 then
      if om == 25 then
       if ol == 25 then (0, 0, 0)
        else (ol + 1, 0, 0)
       else (ol, om + 1, 0)
      else (ol, om, or + 1)

    -- returns all menus at a given index
    getMenusAt :: Index -> Crib -> IndexList -> [Menu]
    getMenusAt i crib il
     | notElem (getEncCharAt i il) (fst crib) = [[i]]
     | otherwise = addXToEach i menus
      where newIndexList = deleteIndex i il
            possibleMoves = getPossibleMoves i crib
            menus = buildMenusAt possibleMoves crib newIndexList 

    -- this is where different menus from an index are concatenated into a single list
    buildMenusAt :: Indexes -> Crib -> IndexList -> [Menu]
    buildMenusAt [] _ _ = [[]]
    buildMenusAt (x:xs) crib il = (getMenusAt x crib il) ++ (buildMenusAt xs crib il)

    -- given a crib, returns all possible menus
    getAllMenus :: Crib -> [Menu]
    getAllMenus crib = [menus | x <- [0..k], menus <- getMenusAt x crib il]
     where il = makeIndexList crib
           k = length (fst crib)

    --return a list of tuples containing all the letters in the specified crib and their corresponding positions in that crib (starting at 0)
    makeIndexList :: Crib -> IndexList
    makeIndexList (plain, encoded) = zip [0..k] (zip plain encoded)
     where k = length plain

    -- adds inputed integer at the start of each given menu
    -- this function creates the 'links' between indexes in a menu
    addXToEach :: Int -> [Menu] -> [Menu]
    addXToEach _ [] = []
    addXToEach i (x:xs) = (i:x) : (addXToEach i xs)

    -- given an index i and a crib, returns all possible links between i and other indexes
    getPossibleMoves :: Index -> Crib -> Indexes
    getPossibleMoves i crib = [ix | ix <- [0..k], getEncCharAt i il == getPlainCharAt ix il]
     where k = length (fst crib)
           il = makeIndexList crib

    -- given an index i and an IndexList, returns the character at position i in a crib's encoded message
    getEncCharAt :: Index -> IndexList -> Char
    getEncCharAt _ [] = '\00'
    getEncCharAt i (x:xs)
     | i == fst x = snd (snd x)
     | otherwise = getEncCharAt i xs

    -- given an index i and an IndexList, returns the character at position i in a crib's plain message
    getPlainCharAt :: Index -> IndexList -> Char
    getPlainCharAt _ [] = '\00'
    getPlainCharAt i (x:xs)
     | i == fst x = fst (snd x)
     | otherwise = getPlainCharAt i xs

    -- deletes the entry at the specified index from an IndexList
    deleteIndex :: Index -> IndexList -> IndexList
    deleteIndex i il = [x | x <- il, fst x /= i]

    -- returs the letter of the alphabet that corresponds to the currently visited character of a cipher
    -- for example if our cipher is rotor1's configuration, this function returns 'C' when given all the characters in the cipher starting at 'M'
    -- used both when encoding and decoding
    currentAlphabetChr :: Cipher -> Char
    currentAlphabetChr cipher = chr (91 - length cipher)

    -- testing purposes --
    steckerboard = [('B','Y'),
                    ('A','R'),
                    ('D','U'),
                    ('C','H'),
                    ('F','Q'),
                    ('E','S'),
                    ('I','L'),
                    ('G','P'),
                    ('K','X'),
                    ('J','N')]
                    -- V, O, M, Z, T, W are unpaired, so our reflect function should return them unchanged
                    -- ('V','O'), -
                    -- ('M','Z'),
                    -- ('T','W')]

    steckerboard2 = [(' ', ' ')]
    myList = [a | a <- [1, 2, 3, 4], a `elem` [1, 7, 8, 2]]
    myCrib = ("WETTERVORHERSAGEBISKAYA", "RWIVTYRESXBFOGKUHQBAISE")
    myIndexList = makeIndexList myCrib
