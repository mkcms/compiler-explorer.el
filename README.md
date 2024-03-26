# compiler-explorer.el #

Emacs package that provides a simple client for [compiler explorer][compiler-explorer] service.

## Usage ##

`M-x compiler-explorer` is the main entry point.  It will ask you for a
language and display source&compilation buffers.  Type something in the source
buffer; the compilation buffer will automatically update with compiled asm
code.  Another buffer displays output of the compiled and executed program.

`M-x compiler-explorer-set-compiler` changes the compiler for current session.

`M-x compiler-explorer-set-compiler-args` sets compilation options.

`M-x compiler-explorer-add-library` asks for a library version and adds it to
current compilation.  `M-x compiler-explorer-remove-library` removes them.

`M-x compiler-explorer-set-execution-args` sets the arguments for the executed
program.

`M-x compiler-explorer-set-input` reads a string from minibuffer that will be
used as input for the executed program.

`M-x compiler-explorer-new-session` kills the current session and creates a new
one, asking for source language.

`M-x compiler-explorer-previous-session` lets you cycle between previous
sessions.

`M-x compiler-explorer-make-link` generates a link for current compilation so
it can be opened in a browser and shared.

`M-x compiler-explorer-layout` cycles between different layouts.

## License ##

```
Copyright (C) 2020-2024 Michał Krzywkowski

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <https://www.gnu.org/licenses/>.
```
<!-- Local Variables: -->
<!-- coding: utf-8 -->
<!-- fill-column: 79 -->
<!-- End: -->

[compiler-explorer]: https://godbolt.org/
