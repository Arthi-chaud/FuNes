# 🎮 FuNes

FuNes is a Nintendo Entertainment System (NES) emulator, written in Haskell. It supports:

> [!WARNING]
> This emulator is for research purposes. Do not use as your daily emulator. You can easily find more mature NES emulators on GitHub!

<img src="https://github.com/user-attachments/assets/4cc0b719-e15c-4f97-b307-79a0256c9788" width="400">
<img src="https://github.com/user-attachments/assets/63436e62-c2ef-4e28-bc68-516357b04379" width="400">

## :star: Characteristics

- NTSC
- Audio (although not perfect)
- Mappers
  - NROM

## :wrench: Installation 

To build the emulator, you will need to have [Stack](https://docs.haskellstack.org/en/stable/install_and_upgrade/#install-stack) and the [SDL2 library](https://wiki.libsdl.org/SDL2/Installation) installed.

```bash
# In the cloned repository
stack build
stack run -- ./my_rom.nes # The path to the rom to use
```
_Note_: `stack install` will install the emulator as `funes-exe`

## :video_game: Controls

| NES Controller  | Physical Buttons |
|-----------------|------------------|
|        A        |         A        |
|        B        |      S or Z      |
|      Start      |       Enter      |
|      Select     |       Space      |
| Directional Pad |      Arrows      |

Press `Q` or `esc` to quit.

## :white_check_mark: Testing and accuracy

We use the [`nestest` ROM and trace](https://www.qmtpro.com/~nes/misc/nestest.txt) to test the CPU's behaviour and instructions.

While accuracy is not the ultimate goal, we use the [`AccuracyCoin`](https://github.com/100thCoin/AccuracyCoin) ROM to evaluate the correctness of the emulator.

<details>
<summary>Latest results</summary>

Do not be scared of the failing tests. Please check out [this video](https://www.youtube.com/watch?v=oYjYmSniQyM) to understand what this ROM checks for.

_Results from Nov. 21 2025_
Score: 83 / 131 

<img width="768" height="748" alt="Screenshot 2025-11-21 at 16 27 00" src="https://github.com/user-attachments/assets/38d3ad2a-9993-4ee6-aa2e-fd4fac500f04" />

</details>

## :mag: Why another NES emulator?

FuNes is an experiment before anything else. The goal was to see if the functional paradigm would apply well to writting virtual machines and emulator, thus being written in Haskell. 

While the goal is to have a working emulator, it is in no way 100% compliant. Some features are not (yet) implemented (see [here](https://github.com/Arthi-chaud/FuNes/issues)) and some behaviour may be invalid.

### Results of this _little_ experiment

- The different parts of the emulator (CPU, PPU, Bus) are isolated objects and computations on them were designed using Monads. This made testing a real pleasure. 
Moreover, I find the code overal quite elegant (e.g. for the opcodes).
- The typeafety prevented some bit-level mistakes (especially when handling 2-byte addresses and 1-byte data), which is always welcome.
- However, it feels like the functional paradigm didn't bring much else to the table, compared to a regular object-orented approach. Although, it should be noted that the functional paradigm never felt like an obstacle when designing and implementing the emulator.

## :gear: Technicalities

- Main stateful computations (operations on the CPU, APU, PPU, etc.) are modeled using continuation-passing style (CPS) monads
- Multi-threading for rendering and sound filtering
- Using SDL2

## 📚 Resources used

This project wouldn't exist without the following resources. Many thanks to their respective authors 🙏

- [6502 Reference](https://www.nesdev.org/obelisk-6502-guide/reference.html)
- 'Writing NES emulator in Rust' ([GitHub Pages](https://bugzmanov.github.io/nes_ebook/chapter_1.html), [Source Code](https://github.com/bugzmanov/nes_ebook/tree/master))
- [nesdev.org](https://www.nesdev.org)
- [tetanes](https://github.com/lukexor/tetanes) 
- [TriCNES](https://github.com/100thCoin/TriCNES)


<details>

<summary>Why FuNes?</summary>

It's simple: `Functional Programming` + `NES` = `FuNes`

The fact that it shares the name with the late french actor _Louis de Funes_ is accidental.

</details>
