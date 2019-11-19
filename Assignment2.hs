

module Assignment2 where
    import AssignmentHelp

    import Data.Char -- this has been imported in AssignmentHelp, but for some reason I get errors if I don't import it here as well

    type Rotor = Cipher
    type Reflector = [(Char, Char)]
    type Offsets = (Int, Int, Int)
    type Steckerboard = Reflector --although in reality they are a bit different, they are the same thing in terms of functionality

    data Enigma = SimpleEnigma Rotor Rotor Rotor Reflector Offsets | 
                 SteckeredEnigma Rotor Rotor Rotor Reflector Offsets Steckerboard

    my_enigma = SimpleEnigma rotor1 rotor2 rotor3 reflectorB (0, 0, 0) -- instance of Enigma

    enigmaEncode :: Char -> Enigma -> Char
    enigmaEncode c (SimpleEnigma lr mr rr reflector (ol, om, or)) = reverseEncode c6 rr orNew
     where (olNew, omNew, orNew) = advanceRotors (ol, om, or)
           --
           c1 = (encode c rr orNew)
           c2 = (encode c1 mr omNew)
           c3 = (encode c2 lr olNew)
           --
           c4 = (reflect c3 reflector)
           --
           c5 = (reverseEncode c4 lr olNew)
           c6 = (reverseEncode c5 mr omNew)

    enigmaEncodeMessage :: String -> Enigma -> String
    enigmaEncodeMessage [] _ = []
    enigmaEncodeMessage (y:ys) (SimpleEnigma lr mr rr reflector (ol, om, or)) =
          enigmaEncode y (SimpleEnigma lr mr rr reflector (ol, om, or)) : 
                    enigmaEncodeMessage ys (SimpleEnigma lr mr rr reflector (olNew, omNew, orNew))
          where (olNew, omNew, orNew) = advanceRotors (ol, om, or)

    -- encodes 1 character based on one rotor's configuration and its corresponding offset
    -- this method encapsulates the functionality of one single rotor
    -- right to left
    encode :: Char -> Rotor -> Int -> Char
    encode character cipher offset
     | (alphaPos character + offset) `mod` 26 == 
            (alphaPos (currentAlphabetChr cipher)) `mod` 26 = chr ((((alphaPos (head cipher)) - offset) `mod` 26) + 65)
     | otherwise = encode character (tail cipher) offset

    -- used to encode the letter after the letter goes through the reflector
    reverseEncode :: Char -> Rotor -> Int -> Char
    reverseEncode _ [] _ = '\00' 
    reverseEncode c (x:xs) offset
     | q `mod` 26 == p = chr (((alphaPos (currentAlphabetChr (x:xs)) - offset) `mod` 26) + 65)
     | otherwise = reverseEncode c xs offset
     where p = alphaPos x
           q = alphaPos c + offset

    -- performs a letter-swap, given a character and either a reflector or steckerboard
    -- also works if given a steckerboard instead of a reflector (they are basically the same thing, except for 6 unpaired letters)
    reflect :: Char -> Reflector -> Char
    reflect c [] = c -- this line accounts for the fact that in a steckerboard not all letters are paired
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

    -- returs the letter of the alphabet that corresponds to the currently visited character of a cipher
    -- for example if our cipher is rotor1's configuration, this function returns 'C' when we are at letter 'M' in the cipher
    -- might not make sense at first, but it is pretty intuitive once you understand how (reverse-)encoding is done
    currentAlphabetChr :: Cipher -> Char
    currentAlphabetChr cipher = chr (91 - length cipher)
