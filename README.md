# passrhyme

Passrhyme is a rhyming password generator.  To build it in Mac OS X (with homebrew), run:
```
brew install openssl
stack build --extra-include-dirs=/usr/local/opt/openssl/include --extra-lib-dirs=/usr/local/opt/openssl/lib
```
(Those flags are needed the first time you build it because we depend on HsOpenSSL as one of our sources of secure random bits, and those are the locations where homebrew puts the openssl library.)

Then you an run it like this:

```
stack exec passrhyme-exe
```


Note: you need to run the executable from the root directory of this repository so that passrhyme can find the CMU prounouncing
dictionary and the 10k word list.

Note also: the examples below are drawn from the best of four or five tries.

## Example Execution

```
Enter a string of dots and dashes to select your meter.
(- means a stressed sylable and . means an unstressed sylable.)
Press Enter (blank input) for iambic tetrameter (.-.-.-.-)
.-.-.-.-.

Sample #1:
amelia lowered graduated
dispersion duncan celebrated

Selecting the rhyme gave us 15.235565522936467 bits of surprize.
Given those rhyme words, the remainder of the phrase had 46.41529838093291 bits of surprize.
For a total surprize of: 61.65086390386938 bits.


Sample #2:
develops charters resignation
defenders storage designation

Selecting the rhyme gave us 15.235565522936467 bits of surprize.
Given those rhyme words, the remainder of the phrase had 46.41529838093291 bits of surprize.
For a total surprize of: 61.65086390386938 bits.


Sample #3:
endowment cluster formulation
antarctic wealthy alteration

Selecting the rhyme gave us 15.235565522936467 bits of surprize.
Given those rhyme words, the remainder of the phrase had 46.41529838093291 bits of surprize.
For a total surprize of: 61.65086390386938 bits.


Sample #4:
nirvana epson population
transistor merlin augmentation

Selecting the rhyme gave us 15.235565522936467 bits of surprize.
Given those rhyme words, the remainder of the phrase had 46.41529838093291 bits of surprize.
For a total surprize of: 61.65086390386938 bits.


Sample #5:
exchanged belong evacuation
vanessa pointing computation

Selecting the rhyme gave us 15.235565522936467 bits of surprize.
Given those rhyme words, the remainder of the phrase had 43.43260354790077 bits of surprize.
For a total surprize of: 58.66816907083724 bits.
```
