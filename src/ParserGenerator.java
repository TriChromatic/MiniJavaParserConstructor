import java.util.List;

/**
 * Parser generator class. Main part of the project.
 * Created by TriChromatic aka Dylan Eicher on 11/17/14.
 */
public class ParserGenerator {
    /*GEN_CODE aka Generated Code*/
    private final String GEN_CODE;
    /*C is for code*/
    private final StringBuilder C;

    /**
     * Generates code from tokens created by delimiter parser
     * Special syntax includes: +, -, ., ^, &
     * This code gets messy but hopefully the comments should help anyone who wants to understand it.
     * I am trying to document my code more and better.
     *
     * @param tokens     tokens to build parser from
     * @param removeNull true if adding code to remove null tokens
     */
    ParserGenerator( List<String> tokens, boolean removeNull ) {
        /*Create StringBuilder with proper starting things and store tokens*/
        C = new StringBuilder( StoredText.INITIALFOR.txt() );

        /*Helper var declaration*/
        String eq = "=="; //Switches between != and ==, Defaults to ==
        String ao = "||"; //Switches between && and ||, Defaults to ||
        int tokenIndex = C.length(); //Beginning index where token stuff should start
        int ifStartIndex = C.length(); //Because index where the last if run was conducted

        for ( int i = 0; i < tokens.size(); i++ ) {
            /*Safe token finding. Token is declared final for safety*/
            final String token = tokens.get( i );
            final char startC = token.charAt( 0 );

            /*Forward token must only be non syntax related*/
            String forwardToken = "";
            if ( i < tokens.size() - 1 ) {
                for ( int x = i + 1; x < tokens.size(); x++ ) {
                    forwardToken = tokens.get( x );
                    if ( !isSyntax( forwardToken ) ) {
                        break;
                    }
                }
            }

            /*Generate ifs, later statements add modifiers*/
            if ( !isSyntax( token ) ) {

                /*Reset x and c, set indexes*/
                appendResetCode();
                ifStartIndex = C.length();
                tokenIndex = ifStartIndex;

                /*Construct if char at statements*/
                if ( isEq( eq ) ) {
                    C.append( "switch(c){\n" );
                    for ( int j = 0; j < token.length(); j++ ) {
                        C.append( String.format( "case '%s':\n", token.charAt( j ) ) );
                        C.append( "tokens.add(text.substring(index, x));\n" );
                        C.append( ( "index = x + 1;\n" ) );
                        C.append( "break;\n" );
                    }
                } else {

                    /*If we use a ^ this is what we do use the not eq and and*/
                    C.append( String.format( "if(c!='%s'", token.charAt( 0 ) ) );
                    if ( token.length() > 1 ) {
                        for ( int j = 1; j < token.length(); j++ ) {
                            C.append( String.format( "%sc!='%s'", ao, token.charAt( j ) ) );
                        }
                    }
                    C.append( "){\n" );
                    C.append( "tokens.add(text.substring(index, x));\n" );
                    C.append( "index = x + 1;\n" );
                    C.append( "}\n" );
                }

                eq = "==";
                ao = "||";

            } else if ( startC == Syntax.END.getC() && isSyntax( token ) ) {

                /*Account for ending notation*/
                C.insert( tokenIndex, "x = text.length() - 1;\nc = text.charAt(x);\n" );
                C.insert( tokenIndex, "index = text.length() - 2;\n" );
                tokenIndex = C.length();

            } else if ( startC == Syntax.START.getC() && isSyntax( token ) ) {

                /*Account for starting notation*/
                C.insert( tokenIndex, ( "x = 0;\nc = text.charAt(x);\n" ) );
                tokenIndex = C.length();

            } else if ( startC == Syntax.BIND.getC() && isSyntax( token ) ) {

                /*Injects if statements into proper index*/
                int tempLength = C.length();
                C.insert( ifStartIndex, String.format( "if(c%s'%s'", eq, forwardToken.charAt( 0 ) ) );
                int tempIndex = ifStartIndex + ( C.length() - tempLength );

                /*Use forward token and insert the statement into the correct position*/
                for ( int j = 1; j < forwardToken.length(); j++ ) {
                    tempLength = C.length();
                    C.insert( tempIndex, String.format( " %s c%s'%s'", ao, eq, forwardToken.charAt( j ) ) );
                    tempIndex = tempIndex + ( C.length() - tempLength );
                }

                /*Insert new x specification to look behind*/
                C.insert( tempIndex, ( "){\nx = x - 1;\nc = text.charAt(x);\n" ) );
                C.append( ( "}\n" ) );
                tokens.remove( forwardToken );

            } else if ( startC == Syntax.NOT.getC() && isSyntax( token ) ) {
                eq = "!=";

            } else if ( startC == Syntax.AND.getC() && isSyntax( token ) ) {
                ao = "&&";

            }
        }

        /*Ending braces and other*/
        if ( removeNull ) {
            appendCleanerCode();
        } else {
            C.append( "}\n}\nreturn tokens;\n}" );
        }

        /*Clean up code and convert to String*/
        GEN_CODE = reformatCode( C ).toString();
    }


    /**
     * @return generated C
     */
    public String getGeneratedCode() {
        return GEN_CODE;
    }

    /**
     * Checks if a token is a syntax token
     *
     * @param token token passed from initializer
     * @return true if token is 1 char long and contains syntax
     */
    private boolean isSyntax( String token ) {
        if ( token.length() == 1 ) {
            for ( Syntax temp : Syntax.values() ) {
                if ( token.charAt( 0 ) == temp.getC() ) {
                    return true;
                }
            }
        }
        return false;
    }

    /**
     * Checks equality operator
     *
     * @param eq equality operator
     * @return true if ==, false otherwise
     */
    private boolean isEq( String eq ) {
        return eq.equals( "==" );
    }

    /**
     * Adds C for cleaning tokens
     */
    private void appendCleanerCode() {
        C.append( StoredText.CLEANUP.txt() );
    }

    /**
     * Adds C for resetting c to x = i
     */
    private void appendResetCode() {
        C.append( "int x = i;\nchar c = text.charAt(x);\n" );
    }

    /**
     * Creates string of tabs
     *
     * @return stacked tabs according to depth
     */
    private String stackTabs( int c ) {
        StringBuilder tab = new StringBuilder();
        for ( int i = 0; i < c; i++ ) {
            tab.append( "\t" );
        }
        return tab.toString();
    }

    /**
     * Calculates depth. Dont even try to look at it. It will just hurt.
     *
     * @return current depth
     */
    private int calculateDepth( String s ) {
        int depth = 0;
        boolean specialMod = true;
        for ( int i = s.length() - 1; i >= 0; i-- ) {
            if ( s.charAt( i ) == '{' ) {
                depth++;
            } else if ( s.charAt( i ) == '}' ) {
                if ( specialMod ) {
                    depth -= 2;
                } else {
                    depth--;
                }
                specialMod = false;
            } else if ( s.charAt( i ) == ':' ) {
                depth++;
            } else if ( i - 6 >= 0 ) {
                if ( s.substring( i - 6, i ).equals( "break;" ) ) {
                    if ( specialMod ) {
                        depth -= 2;
                    } else {
                        depth--;
                    }
                    specialMod = false;
                }
            }
        }

        return depth;
    }

    /**
     * Adds correct whitespace. Do not touch this. I don't want this to go up in flames.
     */
    private StringBuilder reformatCode( StringBuilder codeToRefactor ) {
        StringBuilder formattedCode = new StringBuilder();
        String[] tokens = codeToRefactor.toString().split( "[\n]" );

        for ( String token : tokens ) {
            String tabs = stackTabs( calculateDepth( formattedCode.toString() ) );
            formattedCode.append( String.format( "%s%s\n", tabs, token ) );
        }

        return formattedCode;
    }
}
