

module Assignment2 where
	import AssignmentHelp

	type Rotor = Cipher
	type Reflector = [(Char, Char)]
	type Offsets = (Int, Int, Int)

	data Enigma = SimpleEnigma Rotor Rotor Rotor Reflector Offsets

	enigmaEncode :: Char -> Enigma -> Char
	enigmaEncode = 
		| 
		| otherwise = 


	encode :: Cipher -> Int -> Char -> Char
	encode cipher offset character
		| (alphaPos character - offset) `mod` 26 == (alphaPos (currentAlphabetChr cipher)) `mod` 26 = head cipher 
		| otherwise = encode (tail cipher) offset character