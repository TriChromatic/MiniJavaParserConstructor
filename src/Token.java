/**
 * Token sorting made easy. Token class because tokens as strings just shouldn't be a thing.
 * Using a token object seemed more appropriate as String was not good for this anymore.
 * Created by TriChromatic aka Dylan Eicher on 11/20/14.
 */
public class Token {
    private final boolean isToken;
    private final String token;
    private Syntax mySyntax;

    /**
     * Tokens can be either normal tokens or syntax. That is what the boolean is for. A token is passed into this
     * along with isToken which allows manual specification of status as a token or syntax. Eg. % passed with true
     * is counted as syntax.
     * <p/>
     * It then uses Syntax.getType to get the type of syntax which is null if it is not valid.
     *
     * @param token   Token string
     * @param isToken If its a token or syntax
     */
    Token( String token, boolean isToken ) {
        this.token = token;
        this.isToken = isToken;
        this.mySyntax = Syntax.getType( this.token );
    }

    public boolean isToken() {
        return isToken;
    }

    public String getToken() {
        return token;
    }

    public Syntax getSyntax() {
        return mySyntax;
    }
}
