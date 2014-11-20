MiniJavaParserConstructor
=========================

A program that takes deimeters and outputs a (hopefully) functional charAt parser. This is much faster than .split or any other regex based parsing solution. I made this mostly for fun but also I figured someone might find some use for it.

It basically generates a method that parses based on a non-regex way of specifying delimieters.

Each delimiter argument that you pass into it should be wrapped in []. Eg. [^][a-zA-Z]. To split off from an existing statement construction add [%]. Eg. [^][a-z]%[A-Z] will create a parser that parses at not a-z, and parses at A-Z. You can combine statements and wrap things. An example of this would be to parse a multi character delimeter. If you wanted to parse ee you cant do [ee], this would create a parser that would parse at e and e. Do do this you would use the delimiters [e].[e] you can even use this for entire words: [h].[e].[l].[l].[o] would use the word hello as a delimiter.

Also considering that nature of the above, you can do multi style parsing such as [abc].[a] which would split at aa, ab, and ac. If you really wanted to do weird things you could even do somthing like [hltf].[a].[v].[ea] which would split at have, lave, tave, fave, hava, lava, and tava.

\*S denotes a space, and \*N denotes a new line eg. [*S] would be splitting at every space.
