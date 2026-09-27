![](logo/8-machina.png)

`8 MACHINA` is a CHIP-8 emulator thought in the functional paradigm and written in Haskell.

Getting Started
---------------
The emulator is built with [cabal](https://www.haskell.org/cabal/) and uses [SDL2](https://www.libsdl.org/) for graphics, keyboard and sound. Install GHC and cabal with [GHCup](https://www.haskell.org/ghcup/), then follow the steps for your system.

### Linux and macOS
Install SDL2 and pkg-config with your package manager:

```sh
sudo apt install libsdl2-dev pkg-config   # Debian / Ubuntu
brew install sdl2 pkg-config              # macOS
```

Build and run a game:

```sh
cabal run 8-machina -- games/BRIX
```

### Windows
GHCup installs its own MSYS2 in `C:\ghcup\msys64`. Install SDL2 from its UCRT64 environment:

```sh
C:\ghcup\msys64\usr\bin\bash.exe -lc "pacman -S mingw-w64-ucrt-x86_64-SDL2 mingw-w64-ucrt-x86_64-pkgconf"
```

Point cabal to those libraries in your cabal config (`C:\cabal\config` by default):

```
extra-include-dirs: C:\ghcup\msys64\ucrt64\include
extra-lib-dirs:     C:\ghcup\msys64\ucrt64\lib, C:\ghcup\msys64\ucrt64\bin
extra-prog-path:    C:\ghcup\msys64\ucrt64\bin, C:\ghcup\msys64\usr\bin
```

GHC's bundled toolchain cannot link `SDL2main`, which the emulator does not need. Copy `C:\ghcup\msys64\ucrt64\lib\pkgconfig\sdl2.pc` into a folder of your choice, remove `-lmingw32 -mwindows -lSDL2main` from its `Libs` line and `-Dmain=SDL_main` from its `Cflags` line, and add that folder to the `PKG_CONFIG_PATH` environment variable.

Build and run a game, with `SDL2.dll` on the `PATH`:

```powershell
$env:Path = "C:\ghcup\msys64\ucrt64\bin;$env:Path"
cabal run 8-machina -- games\BRIX
```

Controls
--------
The CHIP-8 keypad is mapped to the left side of the keyboard:

```
Keyboard        CHIP-8
1 2 3 4         1 2 3 C
Q W E R         4 5 6 D
A S D F         7 8 9 E
Z X C V         A 0 B F
```

Other keys are ignored, unless mapped with `--key`. Close the window to quit.

Options
-------
Options go before the ROM path:

```sh
cabal run 8-machina -- --size 1280x640 --foreground 33FF66 --key Left=4 --key Right=6 games/BRIX
```

  - `--size WIDTHxHEIGHT`: initial window size, a multiple of 64x32 such as `640x320` or `1280x640` (default `1024x512`). The window can be resized while playing and snaps to the largest multiple that fits.
  - `--foreground RRGGBB`, `--background RRGGBB`: pixel colors (default `FFFFFF` and `000000`)
  - `--speed N`: instructions per second (default `600`)
  - `--key NAME=HEX`: make a keyboard key press a CHIP-8 key, on top of the default layout. Can be repeated. `NAME` is a letter, a digit, `Keypad0` to `Keypad9`, `Up`, `Down`, `Left`, `Right`, `Space`, `Enter`, `Tab`, `Backspace`, `LeftShift`, `RightShift`, `LeftCtrl` or `RightCtrl`.
  - `--tone HZ`: pitch of the beep (default `440`)
  - `--volume 0-100`: loudness of the beep, `0` turns sound off (default `10`)
  - `--interpreter cosmac|superchip`: original CHIP-8 interpreter to behave like (default `cosmac`). BLINKY needs `superchip`.

Changelog
---------
### Unreleased
  - Separate the emulation loop from SDL behind a Frontend interface (Emulator.hs), so a second front-end can be added without duplicating the loop

### 1.3 (September 2026)
  - Rewrite the emulator core as pure functions, independent of the IO Monad. SDL is only used by the front-end.
  - Rename `--quirks` to `--interpreter`. This breaks the command line, but the project has no known users, so it ships as a minor version.
  - Draw the screen once per frame instead of after every drawing instruction
  - Wrap memory addresses at 4096 and treat keys above 0xF as not pressed, instead of crashing

### 1.2 (September 2026)
  - Add `--size`, `--foreground`, `--background`, `--speed`, `--key`, `--tone`, `--volume` and `--quirks` options
  - Make the window resizable
  - Clip sprites at the screen edge and reset `VF` after `AND`, `OR` and `XOR`, as the COSMAC VIP did

### 1.1 (September 2026)
  - Add cabal build configuration
  - Add sound
  - Limit emulation speed to 600 instructions per second
  - Implement clear screen (`00E0`) and wait for key (`Fx0A`) instructions
  - Ignore machine code calls (`0nnn`)
  - Count down delay and sound timers at 60 Hz
  - Quit when the window is closed
  - Ignore unmapped keys instead of crashing
  - Set `VF` after writing arithmetic results
  - Report no borrow when subtracting equal values
  - Report unknown instructions, stack overflow and underflow, and unreadable or oversized ROM files with clear messages
  - Remove debug output

### 1.0 (April 2018)
  - First version, developed during the functional programming course at ITBA (Instituto Tecnológico de Buenos Aires)

Features To Develop
-------------------
  - Use [brick](https://github.com/jtdaugherty/brick) as a second, terminal-based front-end

To Refactor
-----------
  - Make more abstractions in the `CPU.hs` execution of instructions
