# montop

## Description

montop is a Haskell program that provides a terminal-based interface for monitoring system processes. It's inspired by the `top` command and allows users to view and manage running processes in real-time.

## Usage Instructions

**Note**: Tested with `GHC 9.6.7 (base-4.18.3.0)` and `cabal 3.12.1.0`

### Build

Navigate to the project’s root directory in a terminal and run `cabal build`

### Run

Navigate to the project’s root directory in a terminal and run `cabal run`

### Test

Navigate to the project’s root directory in a terminal and run `cabal test`

### Controls

* Quit: `q` or `Esc` (when not in filter mode)
* Navigate: `Up` and `Down` arrows
* Paging: `PageUp` and `PageDown` to scroll by pages
* Sort: `s` to sort by column. `d` to toggle between ascending and descending
* Filter/search: `f` to filter. `Enter` to accept filter, `Esc` to cancel.
* Kill process: `x` to send a `SIGTERM` signal to the currently selected process. Sending signals to process owned by other users probably requires running as root
