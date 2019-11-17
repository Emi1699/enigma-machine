

module Assignment2 where
	import AssignmentHelp

	import Data.Char

	type Rotor = Cipher
	type Reflector = [(Char, Char)]
	type Offsets = (Int, Int, Int)

	data Enigma = SimpleEnigma Rotor Rotor Rotor Reflector Offsets

	my_enigma = SimpleEnigma rotor1 rotor2 rotor3 reflectorB (0, 0, 0)

	-- enigmaEncode :: Char -> Enigma -> Char
	-- enigmaEncode c (SimpleEnigma lr mr rr ref (ol, om, or))
	-- 	| 
	-- 	| otherwise = 


	-- encodes 1 character based on one rotor's cipher and its corresponding offset
	-- this method captures the functionality of one single rotor
	encode :: Char -> Rotor -> Int -> Char
	encode character rotor offset
		| (alphaPos character + offset) `mod` 26 == (alphaPos (currentAlphabetChr rotor)) `mod` 26 = chr ((((alphaPos (head rotor)) - offset) `mod` 26) + 65)
		| otherwise = encode character (tail rotor) offset


	advanceRotors :: Offsets -> Offsets
	advanceRotors (ol, om, or) =
		if or == 25 then
			if om == 25 then
				if ol == 25 then (0, 0, 0)
				else (ol + 1, 0, 0)
			else (ol, om + 1, 0)
		else (ol, om, or + 1)

	currentAlphabetChr :: String -> Char
	currentAlphabetChr cipher = chr (91 - length cipher)
