# hcol - Haskell file reformatter to text with columns

Study example how to reformat/fold text/lines...

```
SYNTAX: col [options...]
  -h, -?        --help                 print this help
  -c[NCOLS]     --cols[=NCOLS]         columns number (default: 2)
  -s[NSPACES]   --spaces[=NSPACES]     spaces number (default: 4)
  -w[WIDTH]     --width[=WIDTH]        column width (default: 20)
  -p[PAGESIZE]  --pagesize[=PAGESIZE]  page size in lines (default: 25)
```

Example of call:

```
cat some_file.txt | runghc col.hs -w40 > columns.txt
```
