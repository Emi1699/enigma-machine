

module Assignment2 where
	import AssignmentHelp

	type Rotor = Cipher
	type Reflector = [(Char, Char)]
	type Offsets = (Int, Int, Int)

	data Enigma = SimpleEnigma Rotor Rotor Rotor Reflector Offsets

	my_enigma = SimpleEnigma rotor1 rotor2 rotor3 reflectorB (0, 0, 0)

	enigmaEncode :: Char -> Enigma -> Char
	enigmaEncode c (SimpleEnigma lr mr rr ref (ol, om, or))
		| 
		| otherwise = 


	encode :: Cipher -> Int -> Char -> Char
	encode cipher offset character
		| (alphaPos character - offset) `mod` 26 == (alphaPos (currentAlphabetChr cipher)) `mod` 26 = head cipher 
		| otherwise = encode (tail cipher) offset character


	advanceRotors :: Offsets -> Offsets
	advanceRotors (ol, om, or) =
		if or == 25 then
			if om == 25 then
				if ol == 25 then (0, 0, 0)
				else (ol + 1, 0, 0)
			else (ol, om + 1, 0)
		else (ol, om, or + 1)
