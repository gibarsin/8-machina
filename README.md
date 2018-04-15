![](logo/8-machina.png)

`8 MACHINA` is a CHIP-8 emulator thought in the functional paradigm and written in Haskell.

Getting Started
---------------
This section is a work in progress. Being 15 April 2018, the project is 2 weeks old. It is my intention to work on the build-run process with easiness in mind, using [stack](https://github.com/commercialhaskell/stack) as the main developing tool.

Features To Develop
-------------------
  - Add and customize sound
  - Customize display colors
  - Customize display scale
  - Customize keypad mapping
  - Customize emulation speed
  - Use [brick](https://github.com/jtdaugherty/brick) as UI library

To Refactor
-----------
  - Make the main completely independent of the keypad and graphics module
  - Make more abstractions in the `CPU.hs` execution of instructions
  - Make the back end independent of the IO Monad
