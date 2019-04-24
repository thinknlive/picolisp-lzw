# picolisp-lzw Simple lzw compression

```
#{
initialize TABLE[0 to 255] = code for individual bytes
CODE = read next code from encoder
STRING = TABLE[CODE]
output STRING
while there are still codes to receive:
  CODE = read next code from encoder
  if TABLE[CODE] is not defined: // needed because sometimes the
    ENTRY = STRING + STRING[0] // decoder may not yet have entry!
  else:
    ENTRY = TABLE[CODE]
  output ENTRY
  add STRING+ENTRY[0] to TABLE
  STRING = ENTRY
}#

#{
initialize TABLE[0 to 255] = code for individual bytes
STRING = get input symbol
while there are still input symbols:
  SYMBOL = get input symbol
  if STRING + SYMBOL is in TABLE:
    STRING = STRING + SYMBOL
  else:
    output the code for STRING
    add STRING + SYMBOL to TABLE
    STRING = SYMBOL
  output the code for STRING
}#
```

