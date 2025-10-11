# 🎮 FuNes

FuNes is a Nintendo Entertainment System (NES) emulator, written in Haskell.

> [!WARNING]
> This emulator is for research purposes. Do not use as your daily emulator. You can easily find more mature NES emulators on GitHub!

<img src="https://github.com/user-attachments/assets/3e42fe20-13ac-40bc-85a5-10cf7305a41a" width="400">
<img src="https://github.com/user-attachments/assets/9eeb7891-1711-4cb6-886b-dfa46f360164" width="400">


## :mag: Why does this project exist?

FuNes is an experiment before anything else. The goal was to see if the functional paradigm would apply well to writting virtual machines and emulator, thus being written in Haskell. 

While the goal is to have a working emulator, it is in no way 100% valid. Some features are not (yet) implemented (see [here](https://github.com/Arthi-chaud/FuNes/issues)) and some behaviour may be invalid

### Results of this _little_ experiment

- The different parts of the emulator (CPU, PPU, Bus) are isolated objects and computations on them were designed using Monads. This made testing a real pleasure. 
Moreover, I find the code overal quite elegant (e.g. for the opcodes).

- The typeafety prevented some bit-level mistakes (especially when handling 2-byte addresses and 1-byte data), which is always welcome.

- However, it feels like the functional paradigm didn't bring much else to the table, compared to a regular object-orented approach. Although, it should be noted that, in any case, was the functional paradigm an obstacle in the design and implementation of the emulator

## 📄 How to use

Still wanna try out the emulator? Ok, here's how

### Run

You will need to have Stack and the SDL2 library installed.

```bash
# In the cloned repository
stack build
stack run -- ./my_rom.nes # The path to the rom to use
```
_Note_: `stack install` will install the emulator as `funes-exe`

### Controls

- `Select`: `Space`
- `Start`: `Enter`
- `A`: `A`
- `B`: `S` or `Z`
- D-pad: directional arrows
- Exit: `Q` or `esc`

## Accuracy

- A unit test suite tests the behaviour of the CPU using the [`nestest` suite](https://www.qmtpro.com/~nes/misc/nestest.txt)
- It is regularly tested (manually) using the [`AccuracyCoin`](https://github.com/100thCoin/AccuracyCoin) ROM.

<details>

<summary>Latest results</summary>

Do not be scared of the failing tests, please check out [this video](https://www.youtube.com/watch?v=oYjYmSniQyM) to understand what this ROM checks for.

_Results from Oct. 11 2025_

https://github.com/user-attachments/assets/792677ba-1788-439e-ba40-b2af5105fc37

</details>

## 📚 Resources used

This project wouldn't exist without the following resources. Many thanks to their respective authors 🙏

- [6502 Reference](https://www.nesdev.org/obelisk-6502-guide/reference.html)
- 'Writting NES emulator in Rust' ([GitHub Pages](https://bugzmanov.github.io/nes_ebook/chapter_1.html), [Source Code](https://github.com/bugzmanov/nes_ebook/tree/master))


<details>

<summary>Why FuNes?</summary>

It's simple: `Functional Programming` + `NES` = `FuNes`

The fact that it shares the name with the late french actor _Louis de Funes_ is accidental.

</details>
