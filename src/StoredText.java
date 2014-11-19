/**
 * Enum for storing strings of text used in the program.
 * Created by TriChromatic aka Dylan Eicher on 11/18/14.
 */
public enum StoredText {
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
            "java JPG -h || java JPG --help" ),
    INITIALFOR( "List<String> parseText(String text) {\n" +
            "List<String> tokens = new ArrayList<String>();\n" +
            "int index = 0;\nfor(int i = 0; i < text.length(); i++) {\n" ),
    CLEANUP( "ArrayList<String> cleanTokens = new ArrayList<>();\n" +
            "for ( String s : tokens ) {\n" +
            "if ( s != null && s.length() > 0 ) {\n" +
            "cleanTokens.add( s );\n" +
            "}\n" +
            "}\n" +
            "return cleanTokens" );

    private final String TEXT;

    StoredText( String s ) {
        TEXT = s;
    }

    public String txt() {
        return TEXT;
    }
}
