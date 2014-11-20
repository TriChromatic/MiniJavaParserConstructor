/**
 * Token sorting made easy. Token class because tokens as strings just shouldn't be a thing.
 * <p/>
 * Created by TriChromatic aka Dylan Eicher on 11/20/14.
 */
public class Token {
    private final boolean isToken;
    private final String token;
    private Syntax mySyntax;

    /**
     * Tokens can be either normal tokens or syntax. That is what the boolean is for.
     *
     * @param token   token string
     * @param isToken if its a token or syntax
     */
    Token( String token, boolean isToken ) {
        this.token = token;
        this.isToken = isToken;

        /*If its specified as not token, it trys to set a syntax value*/
        if ( !isToken ) {
            for ( Syntax syntax : Syntax.values() ) {
                if ( syntax.getC() == token.charAt( 0 ) ) {
                    mySyntax = syntax;
                    break;
                } else {
                    mySyntax = Syntax.NULL;
                }
            }
        }
    }

    public boolean isToken() {
        return isToken;
    }

    public String content() {
        return token;
    }

    public Syntax getMySyntax() {
        return mySyntax;
    }
}
