# dmp2mml

dmp2mml converts DefleMask .DMP preset files into
[ctrmml](https://github.com/superctr/ctrmml) FM instrument syntax.

## Compiling

### Requirements

This project uses [Stack](https://docs.haskellstack.org/en/stable/README/)
for building. You'll also need a C compiler and linker, which are used by Stack
for setup and compilation.

### Building

Run:

```
$ stack install
```

to build and install to `~/.local/bin`.

## Usage

Just run:

```
$ dmp2mml the-file.dmp
```

dmp2mml will then print the ctrmml instrument definition:

```
@1 fm
 ; ALG FB
     0  4
 ;  AR DR SR RR SL TL KS ML DT SSG
    31  7  7  0  2 25  3  6  6   0
    31  9  6  0  1 19  2  0  6   0
    31  6  6  0  1 55  3  5  6   0
    31  6  8  8 15 11  2  1  6   0
```

If you want to change the ID, use `-i` / `--id`:

```
$ dmp2mml -i2 the-file.dmp
```

You can also automatically add a description comment with
`-d` / `--description`:

```
$ dmp2mml -i2 -d bass the-file.dmp
@2 fm  ; bass
 ; ALG FB
     0  4
 ;  AR DR SR RR SL TL KS ML DT SSG
    31  7  7  0  2 25  3  6  6   0
    31  9  6  0  1 19  2  0  6   0
    31  6  6  0  1 55  3  5  6   0
    31  6  8  8 15 11  2  1  6   0
```

## Notes

This ignores the LFO and LFO2 parameters in the file.

## FAQ

### I get `Unsupported DMP version <VERSION>, try re-saving it`

dmp2mml supports .DMP files version 0, 9, and 11. If the file is a different
verison, you can open the preset in a recent DefleMask version and re-save it,
which should convert it to version 11.

## Links

- [DMP v11 format reference.](https://deflemask.com/DMP_SPECS.txt)
- [ctrmml reference.](https://github.com/superctr/ctrmml/blob/master/mml_ref.md)
