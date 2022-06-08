# gba-debug.el
Slightly opinionated debug-helper for debugging GameBoy Advance (GBA) programs/games in Emacs. With it, you don't need to do much manual work at all. No launch.json files, no manual compilation, no manual execution of mGBA etc. It does all of these for you :) (with the help of amazing packages like [dap-mode](https://github.com/emacs-lsp/dap-mode)). Just run `M-x gba-debug-program` in your code, and you are debugging! This is how it looks after it has started with a breakpoint (using SimpleBGScroll example from gba-examples in DevkitPRO):

![screenshot](screenshot.png)


## Dependencies
To use gba-debug.el, you need a few programs installed. These are probably pretty standard in most GBA developers arsenal. 
- make: used to run the compilations.
- [DevkitARM gba-dev](https://devkitpro.org/wiki/Getting_Started): Used to compile, and connect to mGBAs gdb interface.
- [mGBA](https://mgba.io/): Used to emulate the resulting .gba-file/executable.


For Emacs-related dependencies, you should set up `dap-gdb-lldb`, which [is described here](https://emacs-lsp.github.io/dap-mode/page/configuration/#native-debug-gdblldb). If the setup method does not work, you can download the extension directly and unzip the vsix-file (yes, it can be unzipped directly). 


## Assumptions
This extension assumes one thing: that you can build using make, and that the command produces an elf-file and gba-file in the same directory. It should also add debug symbols to the elf-file, which is done with the `-g` flag in the gcc-compiler (and similar for g++ and probably the assembler).

**NOTE:** Experimental support for other build systems and structures are in place, and described below for Rust. 

## Usage
You should set two variables:
- `gba-debug-gdb-path`: This is the complete path, including the executable, to DevkitARMs gdb. Standard path on Unix-based systems is the default: `/opt/devkitpro/devkitARM/bin/arm-none-eabi-gdb`.
- `gba-debug-mgba-path`: This is the complete path, including the executable, to mGBA. Example on Mac OS X (Homebrew install): `/Applications/mGBA.app/Contents/MacOS/mGBA`


After these variables are configured, you should be able to run `M-x gba-debug-program` from any source file in your GBA project :)


### Usage with Rust (experimental)
Some minor experiments have been done using Rust with [the gba crate](https://github.com/rust-console/gba). In addition to the variables above, you have a few others you can configure to make it work for Rust. If you only want to set these variables for Rust projects you can use .dir-locals or just set them with `setq-local` in your Rust mode hook (I recommend using [Rustic](https://github.com/brotzeit/rustic)). 

- `gba-debug-build-command`: If you do not build your project with make, you can override it with this variable. To build with a Rust project with the gba crate, I set it to `cargo +nightly build`.
- `gba-debug-projectfile`: When you are using Rust, you probably don't always have a Makefile handy in the project root. In a Rust project you will probably always have a `Cargo.toml` file.
- `gba-debug-custom-executable-path`: By default, Rust produces its binaries in target/debug and target/release directories. The gba crate with the build command in the example above ends up with an elf-file (with no extension) in target/debug. Example value would be `target/debug/gbaprogram` where `gbaprogram` is your program name.


**Note!** If you still use make to build your project, you won't need to fiddle with the build or projectfiles. If you also make a gba-file with gba-fix and put both the elf and gba-file in the root directory of your project, you don't even need any of these settings! This readme might be extended on how this can be done. 



After setting these, you should be able to use breakpoints and all other debugging goodies from your Rust source file just as you would in your C/C++ sources :) 


If you have any additional languages you would like to try, the Rust example above can probably be used as a starting point. 
