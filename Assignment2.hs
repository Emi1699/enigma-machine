

module Assignment2 where
    import AssignmentHelp

    import Data.Char

    type Rotor = Cipher
    type Reflector = [(Char, Char)]
    type Offsets = (Int, Int, Int)

    data Enigma = SimpleEnigma Rotor Rotor Rotor Reflector Offsets

    my_enigma = SimpleEnigma rotor1 rotor2 rotor3 reflectorB (0, 0, 0) -- instance of Enigma

    enigmaEncode :: Char -> Enigma -> Char
    enigmaEncode c (SimpleEnigma lr mr rr reflector (ol, om, or)) = reverseEncode c6 rr
     where (olNew, omNew, orNew) = advanceRotors (ol, om, or)
           c1 = (encode c rr orNew)
           c2 = (encode c1 mr omNew)
           c3 = (encode c2 lr olNew)
           c4 = (reflect c3 reflector)
           c5 = (reverseEncode c4 lr)
           c6 = (reverseEncode c5 mr)

    enigmaEncodeMessage :: String -> Enigma -> String
    enigmaEncodeMessage [] _ = []
    enigmaEncodeMessage (x:xs) (SimpleEnigma lr mr rr reflector (ol, om, or)) = 
     enigmaEncode x (SimpleEnigma lr mr rr reflector (ol, om, or)) : enigmaEncodeMessage xs (SimpleEnigma lr mr rr reflector (olNew, omNew, orNew))
     where (olNew, omNew, orNew) = advanceRotors (ol, om, or)

    -- encodes 1 character based on one rotor's configuration and its corresponding offset
    -- this method basically captures the functionality of one single rotor
    encode :: Char -> Rotor -> Int -> Char
    encode character cipher offset
     | (alphaPos character + offset) `mod` 26 == 
            (alphaPos (currentAlphabetChr cipher)) `mod` 26 = chr ((((alphaPos (head cipher)) - offset) `mod` 26) + 65)
     | otherwise = encode character (tail cipher) offset

    -- does the opposite of encode 
    reverseEncode :: Char -> Rotor -> Char
    reverseEncode c (x:xs) 
     | c == x = currentAlphabetChr (x:xs)
     | otherwise = reverseEncode c xs

    -- performs a letter-swap, given a character and a reflector
    reflect :: Char -> Reflector -> Char
    reflect c ((x, y):z)
     | c == x = y
     | c == y = x
     | otherwise = reflect c z

    -- advances offsets accordingly after 1 character is encoded
    -- there might be a better way to write this, but it works for now
    advanceRotors :: Offsets -> Offsets
    advanceRotors (ol, om, or) =
     if or == 25 then
      if om == 25 then
       if ol == 25 then (0, 0, 0)
        else (ol + 1, 0, 0)
       else (ol, om + 1, 0)
      else (ol, om, or + 1)

    -- returs the letter of the alphabet that corresponds to the currently visited character of a cipher
    -- for example if our cipher is rotor1's configuration, this function returns 'C' when we are at letter 'M' in the cipher
    currentAlphabetChr :: Cipher -> Char
    currentAlphabetChr cipher = chr (91 - length cipher)
