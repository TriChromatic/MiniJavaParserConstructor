/**
 * Enum for storing strings of text used in the program.
 * Created by TriChromatic aka Dylan Eicher on 11/18/14.
 */
public enum StoredText {
    NOPARAM( "Try '-h' or '--help'\nVisit TriChromaticStudios.com for a full syntex and technical overview!\n" ),
    HELP( "Example Usage:\n" +
            "java JPG -d delims -o output.txt\n" +
            "java JPG -f delimsfile.txt -o output.txt\n" +
            "java JPG -r\n" +
            "sanataizes all whitespace from delims\n" +
            "java JPG -rf\n" +
            "sanataizes all whitespace from file pre-parsing\n" +
            "java JPG -rmf\n" +
            "sanatizes all whitespace before parsing except for newlines\n" +
            "\n" +
            "For much more indepth help visit TriChromaticStudios.com\n\n");
    TOKEN_WARNING( "Warning! Token: '%s' is invalid. Token was discarded." );

    private final String TEXT;

    private StoredText( String s ) {
        TEXT = s;
    }

    public String txt() {
        return TEXT;
    }
}
