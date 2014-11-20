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

    /*Certain code should be appended based on whether i is in use*/
    private boolean usingFor;

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
        /*Create StringBuilder with proper starting things and store tokens. If the whole thing is wrapped with
        * a start or end char, we use an initializer with no for loop.*/
        if ( tokens.get( tokens.size() - 1 ).charAt( 0 ) != Syntax.END.getC()
                && tokens.get( tokens.size() - 1 ).charAt( 0 ) != Syntax.START.getC() ) {
            C = new StringBuilder( StoredCode.INITIAL_FOR.code() + StoredCode.INITIAL_X.code()
                    + StoredCode.INITIAL_C.code() + StoredCode.INITIAL_MOD.code() );
            usingFor = true;
        } else {
            C = new StringBuilder( StoredCode.INITIAL_NO_FOR.code() + StoredCode.INITIAL_X.code()
                    + StoredCode.INITIAL_C.code() + StoredCode.INITIAL_MOD.code() );
            usingFor = false;
        }

        /*Helper var declaration*/
        String eq = "=="; //Switches between != and ==, Defaults to ==
        String ao = "||"; //Switches between && and ||, Defaults to ||
        int tokenIndex = C.length(); //Beginning index where token stuff should start
        int swSIndex = C.length(); //Beginning index where the last switch run was conducted
        int bindMod = 0; //Number of bind statements
        int bindIndex = 0; //Index of first bind statement

        for ( int i = 0; i < tokens.size(); i++ ) {

            /*Safe token finding. Token is declared final for safety*/
            final String token = tokens.get( i );
            final char startC = token.charAt( 0 );

            /*Forward token must only be non syntax related*/
            String forwardToken = "";
            if ( i < tokens.size() - 1 ) {
                for ( int x = i + 1; x < tokens.size(); x++ ) {
                    forwardToken = tokens.get( x );
                    if ( !Syntax.isSyntax( forwardToken ) ) {
                        break;
                    }
                }
            }

            /*If its not a syntax token, then we generate the switch case statements*/
            if ( !Syntax.isSyntax( token ) ) {

                /*Reset x and c, set indexes*/
                appendResetCode();
                swSIndex = C.length();
                tokenIndex = swSIndex;

                /*If we use a ^ this is what we do use the not eq and and*/
                C.append( StoredCode.SWITCH_START.code() );

                for ( int j = 0; j < token.length(); j++ ) {
                    C.append( String.format( StoredCode.CASE.code(), (int) token.charAt( j ) ) );
                    if ( isEq( eq ) ) {
                        C.append( StoredCode.INDEXOF.code() );
                    }
                    C.append( StoredCode.BREAK.code() );
                }

                /*Takes into account equality operators.*/
                if ( !isEq( eq ) ) {
                    C.append( StoredCode.DEFAULT.code() );
                    C.append( StoredCode.INDEXOF.code() );
                    C.append( StoredCode.BREAK.code() );
                }

                C.append( StoredCode.END.code() );

                eq = "==";
                ao = "||";

            } else {
                if ( startC == Syntax.END.getC() ) {

                    /*Account for ending notation*/
                    C.insert( tokenIndex, StoredCode.END_X.code()
                            + StoredCode.RESET_C.code() );
                    tokenIndex = C.length();

                } else if ( startC == Syntax.START.getC() ) {

                    /*Account for starting notation*/
                    C.insert( tokenIndex, StoredCode.START_X.code() + StoredCode.RESET_C.code() );
                    tokenIndex = C.length();

                } else if ( startC == Syntax.BIND.getC() ) {

                    /*Set bind index. Also increment bindMod.*/
                    bindIndex = swSIndex;
                    bindMod++;

                    /*Injects if statements into proper index*/
                    int beforeInsert = C.length();
                    C.insert( swSIndex, StoredCode.IF_START.code() );

                    /*Next insert is calculated by taking the number of characters just inserted and adding it
                    * to the previous index point to find the next place the code should be inserted.*/
                    int nextInsert = swSIndex + ( C.length() - beforeInsert );

                    /*Use forward token and insert the statement into the correct position*/
                    for ( int j = 0; j < forwardToken.length(); j++ ) {
                        beforeInsert = C.length();

                        /*Checks to see if its the last statement or not*/
                        if ( j < forwardToken.length() - 1 ) {
                            C.insert( nextInsert, String.format( StoredCode.IF_FORMAT.code(),
                                    eq, (int) forwardToken.charAt( j ), ao ) );
                        } else {
                            C.insert( nextInsert, String.format( StoredCode.IF_FORMAT_NO_AO.code(),
                                    eq, (int) forwardToken.charAt( j ) ) );
                        }
                        nextInsert = nextInsert + ( C.length() - beforeInsert );
                    }

                    /*Insert new x specification to look behind*/
                    C.insert( nextInsert, StoredCode.END_START.code()
                            + StoredCode.LOOK_BEHIND.code()
                            + StoredCode.RESET_C.code()
                            + StoredCode.INC_MOD.code() );

                    tokens.remove( forwardToken );

                } else if ( startC == Syntax.NOT.getC() ) {
                    eq = "!=";
                } else if ( startC == Syntax.AND.getC() ) {
                    ao = "&&";
                } else if ( startC == Syntax.OR.getC() ) {

                    /*Insert Safety Code*/
                    insertSafetyCode( bindIndex, bindMod );

                    /*Finish if / switch by appending braces*/
                    diveMaster( 2 );

                    /*Reset Things*/
                    C.append( StoredCode.RESET_MOD.code() );
                    eq = "==";
                    ao = "||";
                    bindIndex = 0;
                    bindMod = 0;
                }
            }
        }

        /*Insert Safety Code*/
        insertSafetyCode( bindIndex, bindMod );

        /*Append braces if needed*/
        diveMaster( 1 );

        /*Ending braces and other*/
        if ( removeNull ) {
            C.append( StoredCode.CLEANUP.code() );
        } else {
            C.append( StoredCode.RETURN_TOKENS.code() );
        }

        /*Clean up code and convert to String*/
        GEN_CODE = reformatCode( C ).toString();
    }


    /**
     * Returns generated code as a string.
     *
     * @return generated code
     */
    public String getGeneratedCode() {
        return GEN_CODE;
    }

    /**
     * Adds code for resetting c to x = i.
     */
    private void appendResetCode() {
        if ( usingFor ) {
            C.append( StoredCode.RESET_X.code() );
        }
        C.append( StoredCode.RESET_C.code() );
    }

    /**
     * Inserts bindIndex into a proper index as long as bind index has been set
     *
     * @param bindIndex index to insert at
     * @param bindMod   >= modify
     */
    private void insertSafetyCode( int bindIndex, int bindMod ) {
        if ( bindIndex != 0 ) {
            C.insert( bindIndex, String.format( StoredCode.IF_CHECK.code(), bindMod ) );
        }
    }

    /**
     * Adds brackets till specified depth has been reached.
     *
     * @param depth rig for dive!
     */
    private void diveMaster( int depth ) {
        while ( calculateDepth( C.toString() ) != depth ) {
            C.append( StoredCode.END.code() );
        }
    }

    /**
     * Creates string of tabs.
     *
     * @return stacked tabs according to depth
     */
    private String stackTabs( int c ) {
        StringBuilder tab = new StringBuilder();
        for ( int i = 0; i < c; i++ ) {
            tab.append( "    " );
        }
        return tab.toString();
    }

    /**
     * Calculates depth.
     *
     * @param s     String to calculate depth of.
     * @param token Current token
     * @return Depth
     */
    private int calculateDepth( String s, String token ) {
        int depthN = s.split( "[}]|(break;)" ).length;
        int depthP = s.split( "[{]|(case)|(default :)" ).length;
        int depth = depthP - depthN;

        if ( token.matches( "}" ) ) {
            depth--;
        }

        return depth;
    }

    /**
     * Calculates depth.
     *
     * @param s String to calculate depth of
     * @return Depth
     */
    private int calculateDepth( String s ) {
        int depthN = s.split( "[}]|(break;)" ).length;
        int depthP = s.split( "[{]|(case)|(default :)" ).length;

        return depthP - depthN;
    }


    /**
     * Adds correct whitespace. Do not touch this. I don't want this to go up in flames.
     */
    private StringBuilder reformatCode( StringBuilder codeToRefactor ) {
        StringBuilder formattedCode = new StringBuilder();
        String[] tokens = codeToRefactor.toString().split( "\\n" );

        for ( String token : tokens ) {
            String tabs = stackTabs( calculateDepth( formattedCode.toString(), token ) );
            formattedCode.append( String.format( "%s%s\n", tabs, token ) );
        }

        return formattedCode;
    }

    /**
     * Checks equality operator. I have this in here for modularity and consistence.
     *
     * @param eq equality operator
     * @return true if ==, false otherwise
     */
    private boolean isEq( String eq ) {
        return eq.equals( "==" );
    }
}
