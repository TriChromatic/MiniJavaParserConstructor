MiniJavaParserConstructor
=========================

A program that takes deimeters and outputs a (hopefully) functional charAt parser. This is much faster than .split or any other regex based parsing solution. I made this mostly for fun but also I figured someone might find some use for it. It uses non regex syntax to construct a parser and wraps it in a method.

Delimiter Information
---------------------

Each delimiter argument that you pass into it should NOT be wrapped in `[]`. Eg. `^[a-zA-Z]`. To split off from an existing statement construction add `%`. Eg. `^[a-z]%[A-Z]` will create a parser that parses at not `a-z`, and parses at `A-Z`. You can combine statements and wrap things. An example of this would be to parse a multi character delimeter. If you wanted to parse `ee` you cant do `[ee]`, this would create a parser that would parse at e and e. Do do this you would use the delimiters `[e].[e]` you can even use this for entire words: `[h].[e].[l].[l].[o]` would use the word hello as a delimiter.

Also considering that nature of the above, you can do multi style parsing such as `[abc].[a]` which would split at aa, ab, and ac. If you really wanted to do weird things you could even do somthing like `[hltf].[a].[v].[ea]` which would split at have, lave, tave, fave, hava, lava, and tava.

`*S` denotes a space, and `*N` denotes a new line eg. `[*S]` would be splitting at every space.

For fun you can even try createing a parser with this line: `-r -d [}]%[{]%[c].[a].[s].[e]%[b].[r].[e].[a].[k].[;] -o output.txt` which outputs a parser that can parse indentation depth.

Technical Information
---------------------

Saftey catches are included eg. `if(x - 4 >= 0)` which prevents indexing errors. Obviously the nice thing about what this is is that it outputs properly indented source code which you can further optimize, although, it is already very fast out of the box due to using case, very few objects, and using charAt index style parsing.

All generated code is easy to change due to using enums that store strings. Syntax is also very modifyable due to it being stored the same way code generation strings are stored, though, syntax uses chars and not strings. Infact, if you wanted you could create your own syntax for it and not even modify anything but the enum.

Latest updates added a Token object instead of a normal string being used as a token which has allowed for greater flexabilty, and now, there is a flow control method. The flow control method lets all things opened by `[` be considered tokens and not syntax. It also must be closed by `]`. This allows for chars that are normaly delimiters to be considered tokens and also allows for the ability to have long strings of delimiters eg. `[h]-^.[i]`.

Delimiters Main
---------------

+ `.` is bind, basically a look behind eg. `[h].[i]` means an h one char behind an i.

+ `%` is reset, which resets everything and starts without previous delimiters / options. It is also possible to use as an AND / OR.

+ `^` is not, eg. changes tokenize at `a-z` to tokenize at anything not `a-z`.

+ `+` split only at the end

+ `-` split only at the start

Example Uses
------------

+ Parsing words: `^[a-zA-Z]`
+ Getting CSV tokens: `[,].[\"]%[\"].[,]%[,]%[\"]`
+ Anything else involving parsing
