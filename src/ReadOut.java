/**
 * Created by TriChromatic aka Dylan Eicher on 11/18/14.
 */
public enum ReadOut {
    HELP( "Surround delimiters with [] to mean any within.\n" +
            "[].[] Means next to each other eg. [aeiouy].[e] is any vowel next to e.\n" +
            "^ Denotes not eg. ^[a] would mean anything that's not 'a'.\n" +
            "-[] means in front of a string. eg. string say hello parsed with [h]- would parse to [say ][ello].\n" +
            "[]+ is the same thing but means ending.\n" +
            "You can wrap eg. [[aeiouy].[e]]+ means any vowel next to e at the end of string." ),
    NOPARAM( "Example Usage:\n" +
            "java JPG -d delims -o output.txt\n" +
            "java JPG -f delimsfile.txt output.txt\n" +
            "java JPG -r will add in code to remove null / blank tokens\n" +
            "\n" +
            "For help:\n" +
            "java JPG -h || java JPG --help" );

    private final String TEXT;

    ReadOut( String s ) {
        TEXT = s;
    }

    public String getText() {
        return TEXT;
    }
}
