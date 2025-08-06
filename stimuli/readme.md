# Stimuli for maze repetition task to stduy language progessing in ageing

This folders stores both lists and sentence list for single blocks.

## Lists

Lists are json files organized as in the following example,each element is a different listand the software can either pick a specific list or randomly choose among them.

Each list has a "practice" element and an array of blocks.  Each elements refers to a link to a specific raw file stored on github, or elesewhere with the format specifiyed below, this liks should be always public avalilable to be loaded by the presentation software.  In the case you want to refer to a specific version (and not to the current version in the main tread)  you can change the link to refer to a blob in a spcific tread and version:  go on the file in github interface and select action copy permanent link,  it should be something like `https://github.com/francesco-vespignani/MazeAgeing/blob/81671121584b310627a72b9ccb11ec859a28e323/stimuli/blockB.json`, see github documentation.

Example:

```json
{ 
    "listA": {
        "practice": "https://raw.githubusercontent.com/francesco-vespignani/MazeAgeing/refs/heads/main/stimuli/blockP.json",
        "blocks": [
            "https://raw.githubusercontent.com/francesco-vespignani/MazeAgeing/refs/heads/main/stimuli/block1.json",
            "https://raw.githubusercontent.com/francesco-vespignani/MazeAgeing/refs/heads/main/stimuli/block2.json",
            "https://raw.githubusercontent.com/francesco-vespignani/MazeAgeing/refs/heads/main/stimuli/blockA.json"
        ]
    },
    "listB": {
        "practice": "https://raw.githubusercontent.com/francesco-vespignani/MazeAgeing/refs/heads/main/stimuli/blockP.json",
        "blocks": [
            "https://raw.githubusercontent.com/francesco-vespignani/MazeAgeing/refs/heads/main/stimuli/block1.json",
            "https://raw.githubusercontent.com/francesco-vespignani/MazeAgeing/refs/heads/main/stimuli/block2.json",
            "https://raw.githubusercontent.com/francesco-vespignani/MazeAgeing/refs/heads/main/stimuli/blockB.json"
        ]
    }
}
```
## Blocks


Blocks file names should always start with "blcok" and are json files organized as an array of the following elements:

```JSON
{
    "item": "101",
    "cond": "AAT",
    "reps": [
      {
        "target": "IL GALLO CANTA.",
        "distra": "OP LNDEO FENOM.",
        "side": "1 1 0"
      },
           {
        "target": "IL GALLO CANTA.",
        "distra": "GE PPILO DMNEO.",
        "side": "0 1 0"
      },
           {
        "target": "IL GALLO CANTA.",
        "distra": "MO OGMID CEVOM.",
        "side": "0 0 1"
      }    
    ]
  }
```

item is itame number,  cond is condition and reps is an array with an arbirìtrary numer of repetition of the same sentence (named as target) that will be presented word by word with the corresponding distracters (distra filed). The element "side" specifyies on which side of the screen (0 left, 1 right) the correct word is expected to be presented. 

