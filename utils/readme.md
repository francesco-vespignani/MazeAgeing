XXX in the following is the block name

The XXX.txt files have been (manually) insetred into the shyini interface, addind codes and respecting order (for getting meaningful item numbers) separately for each block.

https://pnslhg-giulia-calignano.shinyapps.io/repetitionmaze_distractorgenerator/

The json output (long format) is saved as XXX_giulia is processed by 

./rmArr.py XXX_giulia N

this program removes singleton arrays in resps and add N to the current (automatic)  item numbering and produces the file

XXX_giulia_unnest.json

This latter file is opened with a text editor and checked/modified in order to havo no real italian word (or frequent acronyms)  and to check that random response would not be trivial, avoiding that all words are on the same side for any repetizion,  that subsequent repetitions do not have same pattern,  that for long sentences there is never 5 subsequent "same sides" trials.

  

