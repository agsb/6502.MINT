empty_:
    .asciiz ";"

backsp_:
    .asciiz "\\c@0=0=(1_\\c\\+`\b \b`);"

reedit_:
    .asciiz "\\e\\@\\Z;"

edit_:
    .asciiz "`?`\\K\\N`> `\\^A-\\Z;"

list_:
    .asciiz "\\N26(\\i@\\Z\\c@0>(\\N))\\N`> `;"

printStack_:
    .asciiz "`=> `\\P\\N\\N`> `;"        

toggleBase_:
    .asciiz "\\b@0=\\b!;"

