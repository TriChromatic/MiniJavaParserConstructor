import java.util.ArrayList;
import java.util.List;

/**
 * Delimiter parser part of the parser generator removes brackets and takes things like a-z and turns to into alphabet.
 * Created by TriChromatic aka Dylan Eicher on 11/17/14.
 */
public class DelimiterParser {
    private final List<Token> TOKENS;

    /**
     * Parses and modifys input delims
     *
     * @param delims delimiters to be parsed
     */
    DelimiterParser( String delims ) {

        /*replaces certain abbreviations and non-enterable chars*/
        String delimiters = delims.replace( "a-z", "abcdefghijklmnopqrstuvwxyz" );
        delimiters = delimiters.replace( "A-Z", "abcdefghijklmnopqrstuvwxyz".toUpperCase() );
        delimiters = delimiters.replace( "*S", "\u0020" );
        delimiters = delimiters.replace( "*N", "\u2424" );

        String[] tempTokens = delimiters.split( "((?<=[\\[\\]])|(?=[\\[\\]]))" ); //Split on brackets

        /*VERY important generate parsetree step.*/
        TOKENS = generateParseTree( removeNull( tempTokens ) );
    }

    /**
     * @return tokens
     */
    public List<Token> getTokens() {
        return new ArrayList<>( TOKENS );
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
     * Generates parse treeish thing.
     *
     * @param tokens tokens
     * @return parse tree
     */
    private List<Token> generateParseTree( List<String> tokens ) {
        boolean open = false;
        List<Token> sortedParse = new ArrayList<>();

        /*Flow open and close delims*/
        final String FLOW_OPEN = "[";
        final String FLOW_CLOSE = "]";

        /*Loops through the tokens and creates tokens based on whether they are syntax or not*/
        for ( String token : tokens ) {
            switch ( token ) {
                case FLOW_OPEN:
                    open = true;
                    break;
                case FLOW_CLOSE:
                    open = false;
                    break;
                default:
                    if ( open ) {
                        sortedParse.add( new Token( token, true ) );
                    } else {
                        for ( int j = 0; j < token.length(); j++ ) {
                            String convert = Character.toString( token.charAt( j ) );
                            if ( Syntax.isSyntax( convert ) ) {
                                sortedParse.add( new Token( convert, false ) );
                            }
                        }
                    }
                    break;
            }
        }

        return sortedParse;
    }
}
