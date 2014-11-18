import java.util.List;

/**
 * Created by TriChromatic aka Dylan Eicher on 11/17/14.
 */
public class ParserGenerator {
    /**
     * Syntax for parsing the token
     */
    final char[] SYNTAX = { '+', '-', '.', '^', '&' };
    private final String GEN_CODE;

    /**
     * Generates code from tokens created by delimiter parser
     * Special syntax includes: +, -, ., ^, &
     * This code gets messy but hopefully the comments should help anyone who wants to understand it.
     * I am trying to document my code more and better.
     *
     * @param tokens tokens to build parser from
     * @param removeNull true if adding code to remove null tokens
     */
    ParserGenerator( List<String> tokens, boolean removeNull ) {
        /*Create stringbuilder with proper starting things*/
        StringBuilder code = new StringBuilder( "List<String> parseText(String text) {\n" +
                "List<String> tokens = new ArrayList<String>();\n" +
                "int index = 0;\n" +
                "for(int i = 0; i < text.length(); i++) {\n" +
                "\tint x = i;\n" +
                "\tchar c = text.charAt(x);\n" );

        /*Helper var declaration*/
        String eq = "=="; //Switches between != and ==, Defaults to ==
        String ao = "||"; //Switches between && and ||, Defaults to ||
        int beginIndex = code.length(); //Beginning index where token stuff should start
        int beginIf = code.length(); //Because index where the last if run was conducted

        /*Code generation starts here. Loops through tokens*/
        for ( int i = 0; i < tokens.size(); i++ ) {
            /*Safe token finding. Token is declared final for safety*/
            final String token = tokens.get( i );

            /*Forward token must only be non syntax related*/
            String forwardToken = "";
            if ( i < tokens.size() - 1 ) {
                int x = 1;
                while ( findSyntax( tokens.get( i + x ) ) ) {
                    forwardToken = tokens.get( i + x );
                    x++;
                }
            }

            /*Generate ifs, later statements add modifiers*/
            if ( !findSyntax( token ) ) {
                beginIf = code.length(); //Stores where the last if statement began

                /*Construct if char at statements*/
                if ( eq.equals( "==" ) ) {
                    for ( int j = 0; j < token.length(); j++ ) {
                        code.append( "\tif(" );
                        code.append( "c" );
                        code.append( eq );
                        code.append( "'" );
                        code.append( token.charAt( j ) );
                        code.append( "'){\n" );
                        code.append( "\t\ttokens.add(text.substring(index, x));\n" );
                        code.append( "\t\tindex = x + 1;\n" );
                        code.append( "\t}" );

                        if ( token.length() > 1 && j != token.length() - 1 ) {
                            code.append( " else " );
                        } else {
                            code.append( "\n" );
                        }
                    }
                } else {
                    /*If we use a ^ this is what we do use the not eq and and*/
                    code.append( String.format( "\tif(c!='%s'", token.charAt( 0 ) ) );

                    if ( token.length() > 1 ) {
                        for ( int j = 1; j < token.length(); j++ ) {
                            code.append( String.format( "%sc!='%s'", ao, token.charAt( j ) ) );
                        }
                    }
                    code.append( "){\n" );
                    code.append( "\t\ttokens.add(text.substring(index, x));\n" );
                    code.append( "\t\tindex = x + 1;\n" );
                    code.append( "\t}\n" );
                }

                eq = "=="; //Reset equals
            } else if ( token.charAt( 0 ) == SYNTAX[0] ) {
                /*Account for ending notation*/
                code.insert( beginIndex, "\tx = text.length() - 1;\n" +
                        "\tc = text.charAt(x);\n" );
                beginIndex = code.length();
            } else if ( token.charAt( 0 ) == SYNTAX[1] ) {
                /*Account for starting notation*/
                code.insert( beginIndex, "\tx = 0;\n" +
                        "\tc = text.charAt(x);\n" );
                beginIndex = code.length();
            } else if ( token.charAt( 0 ) == SYNTAX[2] ) {
                /*Insert initial thingymabober*/
                int tempLength = code.length();
                code.insert( beginIf, String.format( "\tif(c%s'%s'", eq, forwardToken.charAt( 0 ) ) );
                int tempDex = beginIf + ( code.length() - tempLength );

                /*Use forward token and insert the statement into the correct position*/
                for ( int j = 1; j < forwardToken.length(); j++ ) {
                    tempLength = code.length();
                    code.insert( tempDex, String.format( " %s c%s'%s'", ao, eq, forwardToken.charAt( j ) ) );
                    tempDex = tempDex + ( code.length() - tempLength );
                }

                /*Insert new x specification to look behind*/
                code.insert( tempDex, "){\n\tx = x - 1;\n\tc = text.charAt(x);\n" );
                code.append( "}\n" );
                tokens.remove( forwardToken );
            } else if ( token.charAt( 0 ) == SYNTAX[3] ) {
                eq = "!=";
            } else if ( token.charAt( 0 ) == SYNTAX[4] ) {
                ao = "&&";
            }
        }

        /*Adds the bit for auto null removal*/
        if ( removeNull ) {
            code.append( "}\n" );
            code.append( "List cleanTokens = new ArrayList<String>();\n" );
            code.append( "\tfor ( String s : tokens ) {\n" );
            code.append( "\t\tif ( s != null && s.length() > 0 ) {\n" );
            code.append( "\t\tcleanTokens.add( s );\n" );
            code.append( "\t}\n" );
            code.append( "}\n" );
            code.append( "return cleanTokens;\n" );
            code.append( "}" );
        } else {
            code.append( "}\nreturn tokens;\n" +
                    "}" );
        }

        GEN_CODE = code.toString();
    }


    public String getGeneratedCode() {
        return GEN_CODE;
    }

    private boolean findSyntax( String token ) {
        for ( char test : SYNTAX ) {
            if ( token.charAt( 0 ) == test ) {
                return true;
            }
        }
        return false;
    }
}
