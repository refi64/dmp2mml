# dmp2mml

dmp2mml converts DefleMask 0.12+ .DMP preset files into
[ctrmml](https://github.com/superctr/ctrmml) FM instrument syntax.

## Compiling

### Requirements

- [Haskell Platform.](https://www.haskell.org/platform/)

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

## Notes

This ignores the LFO and LFO2 parameters in the file.

## FAQ

### I get `Wrong DMP file version, try re-saving it`

Several of DefleMask's own presets are saved in an old format (version 9) for
which documentation isn't available yet. Although I may add support at some
point in the future, at the moment you need to convert them to version 11. This
is easily done by just opening the preset in DefleMask 0.12+ and re-saving it.

## Links

- [DMP v11 format reference.](https://deflemask.com/DMP_SPECS.txt)
- [ctrmml reference.](https://github.com/superctr/ctrmml/blob/master/mml_ref.md)
