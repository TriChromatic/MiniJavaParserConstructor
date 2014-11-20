/**
 * Syntax enum. For easy syntax additions and check code.
 * Created by TriChromatic aka Dylan Eicher on 11/18/14.
 */
public enum Syntax {
    /*Parsed syntax*/
    BIND( '.' ), NOT( '^' ), START( '-' ), END( '+' ), AND( '&' ), OR( '%' ),

    /*Other syntax. Null type is non syntax*/
    FLOW_O( '[' ), FLOW_C( ']' ), NULL( 'N' );

    private final char SYNTAX;

    private Syntax( char c ) {
        SYNTAX = c;
    }

    /**
     * Checks if a token is a syntax token. For a token to be considered syntax it must be
     * only one char long and be a syntax value dictated in the Syntax enum.
     *
     * @param test String to test if it is a syntax char
     * @return True if it is a syntax string, false if it is not
     */
    public static boolean isSyntax( String test ) {
        /*Syntax must be only 1 char*/
        if ( test.length() == 1 ) {
            final char testChar = test.charAt( 0 );

            /*Here we loop through all the values and see if it is valid as syntax*/
            for ( Syntax temp : Syntax.values() ) {
                if ( testChar == temp.getC() ) {
                    return true;
                }
            }
        }

        return false;
    }

    /**
     * Tests a string for being syntax, if it is, it returns the syntax type. Otherwise, it returns the
     * "null" syntax value.
     *
     * @param test Token to get syntax for
     * @return Returns the type of syntax
     */
    public static Syntax getType( String test ) {
        if ( test != null && isSyntax( test ) ) {
            for ( Syntax temp : Syntax.values() ) {
                if ( temp.getC() == test.charAt( 0 ) ) {
                    return temp;
                }
            }
        }

        return null;
    }

    public char getC() {
        return SYNTAX;
    }

    /**
     * Used to check token syntax enum against another enum.
     *
     * @param test Token to test equals
     * @return True if the tokens syntax is equal to this syntax
     */
    public boolean equalsToken( Token test ) {
        return test.getSyntax().equals( this );
    }
}
