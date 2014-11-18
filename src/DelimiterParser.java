import java.util.ArrayList;
import java.util.List;

/**
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
        String[] tempTokens = delimiters.split( "[\\[\\]]" ); //Split on brackets
        this.TOKENS = removeNull( tempTokens );
    }

    private List<String> removeNull( String[] tokens ) {
        ArrayList list = new ArrayList<String>();
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
        return new ArrayList<String>( TOKENS );
    }
}
