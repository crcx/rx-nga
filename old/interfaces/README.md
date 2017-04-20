# Interface Layers for RETRO 12

Retro has two basic parts: an image file providing the basic language and an interface layer providing a user interface and platform specific I/O functionality. The subdirectories here contain implementations of interface layers for some common systems.

See the individual subdirectories for notes on building them.

## Listener (Linux / macOS / Windows)

The listener is tested on and works with:

- Windows 7
- Linux (ARM, x86, x86-64)
- macOS

## Block Editor

The block editor is still considered experimental and shouldn't be relied on for long term use at this point.

## Ngita-Listener

This is still considered experimental. It's probably the closest thing to RETRO 11 in design (an implementation of the listener that runs on the ngita (nga+ngura-io) implementation).

## iOS

This is a snapshot of an older version of the code used in iOS app. It provides an editor centric environment.
