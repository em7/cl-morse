# cl-morse
Morse code translation lib written in Common Lisp.

In version 0.1.0 only letters, spaces and a full stop are supported, more to come. Give me a shout or open an issue if you want to add a new feture to this library, please. Originally this library was created because I was a bit rusty at Common Lisp programming (haven't touched it for years of using Clojure) and to have something to check and try various forms of deployment.

## Usage

Translates text (ignoring case) to morse code and back. The morse code follows a convention that each letter is ended by a forward slash (e.g. 'a' becomes '.-/'). Space is a single '/' and a full stop is two '//'. Which in effect means that letters are separated by a single slash, word by two slashes and sentences by three. When converting a morse code back to text, some ambiguities can happen. This library interprets '////' as '. ' although it could be interpreted as multiple spaces.

Some languages have specific coding for a group of characters, e.g. in Czech, 'ch' is considered a single letter and has a special morse code. This is not supported now.

If an unknown character or code is found, an error is signalled. A restart is provided to continue with the next character.

### Installation

If you have [quicklisp](https://www.quicklisp.org/) installed


```
cd ~/quicklisp/local-projects
git clone https://github.com/em7/cl-morse.git
```

when you run lisp

```
(ql:quickload "cl-morse")
(asdf:test-system :cl-morse)
```

otherwise you can clone the repo wherever ASDF finds it and load it.

### Dependencies

- cl-ppcre (is on quicklisp)
- rove (testing, is on quicklisp)

### Running

Package `cl-morse` exports three symbols:

- condition `unknown-character`
- function `string->morse`
- function `morse->string`

The functions expect a string to convert as a parameter and returns a converted string. A keyword :skip-unknown-chars might be supplied (default nil). If true, skips unknown characters. If false, signals `'unknown-character` when encounters one.

The `'unknown-character` condition defines `:report` and has a reader `unknown-ch` which returns the character which could not be converted.

### Example

see `tests/main.lisp`

```
(cl-morse:morse->string ".-/-.../")
> "ab"

(cl-morse:string->morse "ab")
> ".-/-.../"

(cl-morse:string->morse "aěščřb" :skip-unknown-chars t)
> ".-/-.../"

(handler-case  (cl-morse:string->morse "aěščřb")
  (cl-morse:unknown-character (c) (format t "~a~%" c)))
> Unknown charachter 'ě', could not be converted to morse code.

(handler-bind ((cl-morse:unknown-character
                          #'(lambda (c)
                              (format t "~a~%" c)
                              (invoke-restart 'cl-morse:skip-character))))
           (cl-morse:string->morse "aěščřb"))
           
> Unknown charachter 'ě', could not be converted to morse code.
> Unknown charachter 'š', could not be converted to morse code.
> Unknown charachter 'č', could not be converted to morse code.
> Unknown charachter 'ř', could not be converted to morse code.
> ".-/-.../"
```

