/**
 * This is to store code to use as little as possible and organize it.
 * Created by TriChromatic aka Dylan Eicher on 11/19/14.
 */
public enum StoredCode {
    /*Main code fragments. Start code, null token remove code, return statement*/
    INITIAL_FOR( "List<String> parseText(String text) {\n" +
            "List<String> tokens = new ArrayList<String>();\n" +
            "int index = 0;\nfor(int i = 0; i < text.length(); i++) {\n" ),
    CLEANUP( "ArrayList<String> cleanTokens = new ArrayList<>();\n" +
            "for ( String s : tokens ) {\n" +
            "if ( s != null && s.length() > 0 ) {\n" +
            "cleanTokens.add( s );\n" +
            "}\n" +
            "}\n" +
            "return cleanTokens;\n" +
            "}\n" ),
    INITIAL_NO_FOR( "List<String> parseText(String text) {\n" +
            "List<String> tokens = new ArrayList<String>();\n" +
            "int index = 0;\n" ),
    RETURN_TOKENS( "return tokens;\n}\n" ),

    /*x and c initializer and reset code*/
    INITIAL_X( "int x = 0;\n" ),
    INITIAL_C( "char c;\n" ),
    RESET_C( "c = text.charAt(x);\n" ),
    RESET_X( "x = i;\n" ),

    /*Grabs the token, adds it to the list, sets index forward*/
    INDEXOF( "tokens.add(text.substring(index, x));\nindex = x + 1 + mod;\ni = index;\n" ),

    /*Ending statements*/
    END_START( ") {\n" ),
    END( "}\n" ),

    /*Index / x modification*/
    END_X( "x = text.length() - 1;\n" ),
    START_X( "x = 0;\n" ),
    LOOK_BEHIND( "x = x - 1;\n" ),

    /*If code*/
    IF_FORMAT( "c%s%s%s" ),
    IF_FORMAT_NO_AO( "c%s%s" ),
    IF_START( "if(" ),

    /*Switch code*/
    SWITCH_START( "switch(c) {\n" ),
    CASE( "case %s:\n" ),
    DEFAULT( "default :\n" ),
    BREAK( "break;\n" ),

    /*Modify code*/
    INITIAL_MOD( "int mod = 0;\n" ),
    INC_MOD( "mod++;\n" ),
    RESET_MOD( "mod = 0;\n" ),

    /*Safety Code*/
    IF_CHECK( "if(x - %s >= 0) {\n" );

    private final String CODE;

    private StoredCode( String s ) {
        CODE = s;
    }

    public String code() {
        return CODE;
    }
}
