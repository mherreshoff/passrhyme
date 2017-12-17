# passrhyme

Passrhyme is a rhyming password generator.  It is written in Haskell and builds using stack.  To acquire stack, follow the
instructions at: https://docs.haskellstack.org/en/stable/README/

## Mac OS X Building instructions
To build it in Mac OS X (with homebrew), run the following commands (in the root directory of the repository)
```
brew install openssl
stack build --extra-include-dirs=/usr/local/opt/openssl/include --extra-lib-dirs=/usr/local/opt/openssl/lib
```
(Those flags are needed the first time you build it because we depend on HsOpenSSL as one of our sources of secure random bits, and those are the locations where homebrew puts the openssl library.)

Then you can run it like this:

```
stack exec passrhyme-exe
```

You also need to run the executable from the root directory of this repository so that passrhyme can find the CMU
prounouncing dictionary and the word list.

# Example Outputs

## Iambic Tetrameter
```
Enter a string of dots and dashes to select your meter.
(- means a stressed syllable and . means an unstressed syllable.)
Press Enter (blank input) for iambic tetrameter (.-.-.-.-)


Sample #1:
portrayed secure digest vignette
comprised mcleod unleashed cadet

Selecting the rhyme gave us 14.250520490567572 bits of surprise.
Given those rhyme words, the remainder had 63.38724775654216 bits of surprise.
For a total surprise of: 77.63776824710973 bits.


Sample #2:
contain rebuilt conclude quartet
fulfilled ukraine tulane octet

Selecting the rhyme gave us 14.250520490567572 bits of surprise.
Given those rhyme words, the remainder had 63.38724775654216 bits of surprise.
For a total surprise of: 77.63776824710973 bits.


Sample #3:
succeeding factions quarterback
dorado buses paperback

Selecting the rhyme gave us 14.250520490567572 bits of surprise.
Given those rhyme words, the remainder had 48.52999834601879 bits of surprise.
For a total surprise of: 62.78051883658637 bits.


Sample #4:
mackay perceived phenomenon
dimension rowan pentagon

Selecting the rhyme gave us 14.250520490567572 bits of surprise.
Given those rhyme words, the remainder had 45.393356056092216 bits of surprise.
For a total surprise of: 59.64387654665979 bits.


Sample #5:
sergei adair champagne michele
rotations booster danielle

Selecting the rhyme gave us 14.250520490567572 bits of surprise.
Given those rhyme words, the remainder had 55.958623051280476 bits of surprise.
For a total surprise of: 70.20914354184805 bits.
```

## Custom Meter Example
```
$ stack exec passrhyme-exe
Enter a string of dots and dashes to select your meter.
(- means a stressed syllable and . means an unstressed syllable.)
Press Enter (blank input) for iambic tetrameter (.-.-.-.-)
-..-..-

Sample #1:
publishes rubin mccain
dorothy rio campaign

Selecting the rhyme gave us 12.774581180893598 bits of surprise.
Given those rhyme words, the remainder had 49.78442632576881 bits of surprise.
For a total surprise of: 62.55900750666241 bits.


Sample #2:
congresses gorgeous portrayed
treaty disciple brigade

Selecting the rhyme gave us 12.774581180893598 bits of surprise.
Given those rhyme words, the remainder had 49.78442632576881 bits of surprise.
For a total surprise of: 62.55900750666241 bits.


Sample #3:
artist commences surveys
palaces robson arrays

Selecting the rhyme gave us 12.774581180893598 bits of surprise.
Given those rhyme words, the remainder had 49.78442632576881 bits of surprise.
For a total surprise of: 62.55900750666241 bits.


Sample #4:
baker deductions marquee
reckless thereafter esprit

Selecting the rhyme gave us 12.774581180893598 bits of surprise.
Given those rhyme words, the remainder had 49.78442632576881 bits of surprise.
For a total surprise of: 62.55900750666241 bits.


Sample #5:
fetal mutations descent
extra pathetic consent

Selecting the rhyme gave us 12.774581180893598 bits of surprise.
Given those rhyme words, the remainder had 49.78442632576881 bits of surprise.
For a total surprise of: 62.55900750666241 bits.
```
