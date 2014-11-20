/**
 * Syntax enum. For easy syntax additions and check code.
 * Created by TriChromatic aka Dylan Eicher on 11/18/14.
 */
public enum Syntax {
    BIND( '.' ), NOT( '^' ), START( '-' ), END( '+' ), AND( '&' ), OR( '%' ), NULL( 'N' );

    private final char SYNTAX;

    Syntax( char c ) {
        SYNTAX = c;
    }

    /**
     * Checks if a token is a syntax token. For a token to be considered syntax it must be
     * only one char long and be a syntax value dictated in the Syntax enum.
     *
     * @param test string to test if it is a syntax char
     * @return true if it is a syntax string, false if it is not
     */
    public static boolean isSyntax( String test ) {
        if ( test.length() == 1 ) {
            final char testChar = test.charAt( 0 );
            for ( Syntax temp : Syntax.values() ) {
                if ( temp.getC() == testChar ) {
                    return true;
                }
            }
        }

        return false;
    }

    public char getC() {
        return SYNTAX;
    }
}
