# CSVFC
Comma seperated values flash cards  
![Screenie](demo.png)

## Files
```
.
├── CSVFC.cabal   -- Describes the project
├── LICENSE       -- The default licence from Stack
├── README.md     -- This file
├── Setup.hs      -- A Stack file?
├── demo.png      -- The amazing Screenie!!
├── lib
│   └── CSVFC.hs  -- The core functions of CSVFC
├── src
│   └── main.hs   -- The UI wrapper for the core CSVFC
├── stack.yaml    -- Stack config
├── study         -- Untracked folder filled with my flashcards
│   └── ...
└── test.csv      -- An example csv Q&A file format
└── tests         -- The testing directory
    └── tests.hs

4 directories, 10 files
```

## Run
Before running you will have to manually edit the filepath varialbe in src/main.hs   

After downloading [Stack](https://docs.haskellstack.org/en/stable/README/#how-to-install), issue the following commands.  
`stack setup`  
`stack build`  
`stack exec CSVFC`   
Note: Haskell GHC will be download for you and managed for you by Stack.  

## File format
One question answer pair per line.  
Only one comma allowed in each line. (Might or might not sitll be true... TBD)  
I do very litter error handling so it might blow up :)  
You can add a comment if the first char in the line is `#`  
For more examples see `test.csv`.  
