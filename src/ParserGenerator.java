import java.util.List;

/**
 * Created by TriChromatic aka Dylan Eicher on 11/17/14.
 */
public class ParserGenerator {
    private final String GEN_CODE;

    /**
     * Generates code from tokens created by delimiter parser
     *
     * @param tokens
     * @param removeNull
     */
    ParserGenerator( List<String> tokens, boolean removeNull ) {
        /*Create stringbuilder with proper starting things*/
        StringBuilder code = new StringBuilder( "List<String> parseText(String text) {\n" +
                "List<String> tokens = new ArrayList<String>();\n" +
                "int index = 0;\n" +
                "for(int i = 0; i < text.length(); i++) {\n" +
                "\tint x = i;\n" +
                "\tchar c = text.charAt(x);\n" );

        String eq = "=="; //Switches between != and ==
        int beginIndex = code.length(); //Beginning index where token stuff should start
        int beginIf = code.length(); //Because index where the last if run was conducted

        /*Code generation*/
        for ( int i = 0; i < tokens.size(); i++ ) {
            /*Safe token finding*/
            String token = tokens.get( i );
            String forwardToken = "";
            if ( i < tokens.size() - 1 ) {
                forwardToken = tokens.get( i + 1 );
            }

            /*Generate ifs*/
            if ( !token.startsWith( "+" ) && !token.startsWith( "-" )
                    && !token.startsWith( "." ) && !token.startsWith( "^" ) ) {

                beginIf = code.length();

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
                    /*If we use a ^ this is what we do*/
                    code.append( String.format( "\tif(c!='%s'", token.charAt( 0 ) ) );

                    if ( token.length() > 1 ) {
                        for ( int j = 1; j < token.length(); j++ ) {
                            code.append( String.format( "&&c!='%s'", token.charAt( j ) ) );
                        }
                    }
                    code.append( "){\n" );
                    code.append( "\t\ttokens.add(text.substring(index, x));\n" );
                    code.append( "\t\tindex = x + 1;\n" );
                    code.append( "\t}\n" );
                }

                eq = "=="; //Reset equals
            } else if ( token.startsWith( "+" ) ) {
                /*Account for ending notation*/
                code.insert( beginIndex, "\tx = text.length() - 1;\nc = text.charAt(x);\n" );
                beginIndex = code.length();
            } else if ( token.startsWith( "-" ) ) {
                /*Account for starting notation*/
                code.insert( beginIndex, "\tx = 0;\nc = text.charAt(x);\n" +
                        "\"" );
                beginIndex = code.length();
            } else if ( token.startsWith( "." ) ) {
                /*Insert initial thingymabober*/
                int tempLength = code.length();
                code.insert( beginIf, String.format( "\tif(c%s'%s'", eq, forwardToken.charAt( 0 ) ) );
                int tempDex = beginIf + ( code.length() - tempLength );

                /*Use forward token and insert the statement into the correct position*/
                for ( int j = 1; j < forwardToken.length(); j++ ) {
                    tempLength = code.length();
                    code.insert( tempDex, String.format( " || c%s'%s'", eq, forwardToken.charAt( j ) ) );
                    tempDex = tempDex + ( code.length() - tempLength );
                }

                /*Insert new x specification*/
                code.insert( tempDex, "){\n\tx = x - 1;\nc = text.charAt(x);\n" );
                code.append( "}\n" );
                tokens.remove( forwardToken );
            } else if ( token.startsWith( "^" ) ) {
                eq = "!=";
            }
        }

        /*Adds the bit for auto null removal*/
        if ( removeNull ) {
            code.append( "}\n" );
            code.append( "List cleanTokens = new ArrayList<String>();\n" );
            code.append( "for ( String s : tokens ) {\n" );
            code.append( "if ( s != null && s.length() > 0 ) {\n" );
            code.append( "cleanTokens.add( s );\n" );
            code.append( "}\n" );
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
}
