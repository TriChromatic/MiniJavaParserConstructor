import java.util.List;

/**
 * Parser generator class. Main part of the project.
 * Created by TriChromatic aka Dylan Eicher on 11/17/14.
 */
public class ParserGenerator {
    private final String GEN_CODE;
    private final StringBuilder code;

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
        /*Create stringbuilder with proper starting things*/
        code = new StringBuilder( String.format( "List<String> parseText(String text) {\n" +
                "%sList<String> tokens = new ArrayList<String>();\n" +
                "%sint index = 0;\n%sfor(int i = 0; i < text.length(); i++) {\n"
                , ta( 1 ), ta( 1 ), ta( 1 ) ) );

        /*Helper var declaration*/
        String eq = "=="; //Switches between != and ==, Defaults to ==
        String ao = "||"; //Switches between && and ||, Defaults to ||
        int tokenIndex = code.length(); //Beginning index where token stuff should start
        int ifStartIndex = code.length(); //Because index where the last if run was conducted

        /*Code generation starts here. Loops through tokens*/
        for ( int i = 0; i < tokens.size(); i++ ) {
            /*Safe token finding. Token is declared final for safety*/
            final String token = tokens.get( i );

            /*Forward token must only be non syntax related*/
            String forwardToken = "";
            if ( i < tokens.size() - 1 ) {
                for ( int x = i + 1; x < tokens.size(); x++ ) {
                    forwardToken = tokens.get( x );
                    if ( !findSyntax( forwardToken ) ) {
                        break;
                    }
                }
            }

            /*Generate ifs, later statements add modifiers*/
            if ( !findSyntax( token ) ) {
                /*Reset x and c, set indexes*/
                charReset();
                ifStartIndex = code.length();
                tokenIndex = ifStartIndex;

                /*Construct if char at statements*/
                if ( isEq( eq ) ) {
                    code.append( String.format( "%sswitch(c){\n", ta( 2 ) ) );
                    for ( int j = 0; j < token.length(); j++ ) {
                        code.append( String.format( "%scase '%s':\n", ta( 3 ), token.charAt( j ) ) );
                        code.append( String.format( "%stokens.add(text.substring(index, x));\n", ta( 4 ) ) );
                        code.append( String.format( "%sindex = x + 1;\n", ta( 4 ) ) );
                        code.append( String.format( "%sbreak;\n", ta( 4 ) ) );
                    }
                } else {
                    /*If we use a ^ this is what we do use the not eq and and*/
                    code.append( String.format( "%sif(c!='%s'", ta( 2 ), token.charAt( 0 ) ) );
                    if ( token.length() > 1 ) {
                        for ( int j = 1; j < token.length(); j++ ) {
                            code.append( String.format( "%sc!='%s'", ao, token.charAt( j ) ) );
                        }
                    }
                    code.append( "){\n" ).append( ta( 3 ) );
                    code.append( "tokens.add(text.substring(index, x));\n" ).append( ta( 3 ) );
                    code.append( "index = x + 1;\n" ).append( ta( 2 ) ).append( "}\n" );
                }

                eq = "=="; //Reset equals
            } else if ( token.charAt( 0 ) == Syntax.END.getType() ) {
                /*Account for ending notation*/
                code.insert( tokenIndex, String.format( "%sx = text.length() - 1;\n%sc = text.charAt(x);\n", ta( 2 ), ta( 2 ) ) );
                tokenIndex = code.length();
            } else if ( token.charAt( 0 ) == Syntax.START.getType() ) {
                /*Account for starting notation*/
                code.insert( tokenIndex, String.format( "%sx = 0;\n%sc = text.charAt(x);\n", ta( 2 ), ta( 2 ) ) );
                tokenIndex = code.length();
            } else if ( token.charAt( 0 ) == Syntax.BIND.getType() ) {
                /*Injects if statements into proper index*/
                int tempLength = code.length();
                code.insert( ifStartIndex, String.format( "%sif(c%s'%s'", ta( 2 ), eq, forwardToken.charAt( 0 ) ) );
                int tempIndex = ifStartIndex + ( code.length() - tempLength );

                /*Use forward token and insert the statement into the correct position*/
                for ( int j = 1; j < forwardToken.length(); j++ ) {
                    tempLength = code.length();
                    code.insert( tempIndex, String.format( " %s c%s'%s'", ao, eq, forwardToken.charAt( j ) ) );
                    tempIndex = tempIndex + ( code.length() - tempLength );
                }

                /*Insert new x specification to look behind*/
                code.insert( tempIndex, String.format( "){\n%sx = x - 1;\n%sc = text.charAt(x);\n", ta( 2 ), ta( 2 ) ) );
                code.append( String.format( "%s}\n", ta( 2 ) ) );
                tokens.remove( forwardToken );
            } else if ( token.charAt( 0 ) == Syntax.NOT.getType() ) {
                eq = "!=";
            } else if ( token.charAt( 0 ) == Syntax.AND.getType() ) {
                ao = "&&";
            }
        }

        /*Ending braces and other*/
        if ( removeNull ) {
            addCleanerCode();
        } else {
            code.append( "}\nreturn tokens;\n}" );
        }

        GEN_CODE = code.toString();
    }


    /**
     * @return generated code
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
    private boolean findSyntax( String token ) {
        if ( token.length() == 1 ) {
            for ( Syntax temp : Syntax.values() ) {
                if ( token.charAt( 0 ) == temp.getType() ) {
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
     * Tab append
     *
     * @param depth number of tabs
     * @return stacked tabs according to depth
     */
    private String ta( int depth ) {
        StringBuilder tab = new StringBuilder();
        for ( int i = 0; i < depth; i++ ) {
            tab.append( "\t" );
        }
        return tab.toString();
    }

    /**
     * Adds code for cleaning tokens
     */
    private void addCleanerCode() {
        code.append( String.format( "%s}\n\n", ta( 1 ) ) );
        code.append( String.format( "%sList cleanTokens = new ArrayList<String>();\n", ta( 1 ) ) );
        code.append( String.format( "%sfor ( String s : tokens ) {\n", ta( 1 ) ) );
        code.append( String.format( "%sif ( s != null && s.length() > 0 ) {\n", ta( 2 ) ) );
        code.append( String.format( "%scleanTokens.add( s );\n", ta( 3 ) ) );
        code.append( String.format( "%s}\n", ta( 2 ) ) );
        code.append( String.format( "%s}\n", ta( 1 ) ) );
        code.append( String.format( "%sreturn cleanTokens;\n", ta( 1 ) ) );
        code.append( "}" );
    }

    /**
     * Adds code for resetting c to x = i
     */
    private void charReset() {
        code.append( String.format( "%sint x = i;\n%schar c = text.charAt(x);\n", ta( 2 ), ta( 2 ) ) );
    }
}
