import java.util.ArrayList;
import java.util.List;

/**
 * Delimiter parser part of the parser generator removes brackets and takes things like a-z and turns to into alphabet.
 * Created by TriChromatic aka Dylan Eicher on 11/17/14.
 */
public class DelimiterParser {
    private final List<String> TOKENS;

    /**
     * Parses and modifys input delims
     *
     * @param delims delimiters to be parsed
     */
    DelimiterParser( String delims ) {
        String delimiters = delims.replace( "a-z", "abcdefghijklmnopqrstuvwxyz" ); //Replace alphabet abbreviation
        delimiters = delimiters.replace( "A-Z", "abcdefghijklmnopqrstuvwxyz".toUpperCase() );
        delimiters = delimiters.replace( "*S", "\u0020" );

        String[] tempTokens = delimiters.split( "[\\[\\]]" ); //Split on brackets and |
        TOKENS = removeNull( tempTokens );
    }

    /**
     * @param tokens array of tokens from split
     * @return a new list of tokens with null tokens removed
     */
    private List<String> removeNull( String[] tokens ) {
        ArrayList<String> list = new ArrayList<>();
        for ( String s : tokens ) {
            if ( s != null && s.length() > 0 ) {
                list.add( s );
            }
        }
        return list;
    }

    /**
     * @return tokens
     */
    public List<String> getTokens() {
        return new ArrayList<>( TOKENS );
    }
}
