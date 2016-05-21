# CSVFC
comma seperated values flash cards

note: i haven't even read how to parse csv files (see file format)

a quick (as in about an hour to make) and dirty way that i'm using to study flash cards

## Run
you will have to manually edit the file path

press `q` or `Q` to quit and any other key to ge the next random question

after downloading stack, issue the following commands

`stack setup`

`stack build`

`stack exec CSVFC`

## File format
one question answer pair per line

only one comma allowed in each line

i currently don't do any error handling

eg. `awesome question 1, awesome answer 1`
