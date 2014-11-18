/**
 * Created by TriChromatic aka Dylan Eicher on 11/18/14.
 */
public enum Syntax {
    BIND( '.' ), NOT( '^' ), START( '-' ), END( '+' ), AND( '&' ), OR( '%' );

    private final char TYPE;

    Syntax( char c ) {
        TYPE = c;
    }

    public char getType() {
        return TYPE;
    }
}
