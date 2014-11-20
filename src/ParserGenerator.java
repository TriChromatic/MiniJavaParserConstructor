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
     * @param tokens     Tokens to build parser from
     * @param removeNull True if adding code to remove null tokens
     */
    ParserGenerator( List<Token> tokens, boolean removeNull ) {

        /*Create StringBuilder with proper starting things and store tokens. If the whole thing is wrapped with
        * a start or end char, we use an initializer with no for loop. Boolean usingFor tells resetCode whether we
        * are going to reset x to i or not.*/
        boolean usingFor;

        if ( tokens.get( tokens.size() - 1 ).getToken().charAt( 0 ) != Syntax.END.getC()
                && tokens.get( tokens.size() - 1 ).getToken().charAt( 0 ) != Syntax.START.getC() ) {
            C = new StringBuilder( StoredCode.INITIAL_FOR.code() + StoredCode.INITIAL_X.code()
                    + StoredCode.INITIAL_C.code() + StoredCode.INITIAL_MOD.code() );
            usingFor = true;
        } else {
            C = new StringBuilder( StoredCode.INITIAL_NO_FOR.code() + StoredCode.INITIAL_X.code()
                    + StoredCode.INITIAL_C.code() + StoredCode.INITIAL_MOD.code() );
            usingFor = false;
        }

        /*Helper var declaration*/
        OperatorType AO = OperatorType.OR; //AND / OR
        OperatorType EQ = OperatorType.EQ; //EQUALS / NOT EQUALS
        int tokenIndex = C.length(); //Beginning index where token stuff should start
        int swSIndex = C.length(); //Beginning index where the last switch run was conducted
        int bindMod = 0; //Number of bind statements
        int bindIndex = 0; //Index of first bind statement


        for ( int i = 0; i < tokens.size(); i++ ) {

            /*Safe token finding. Token is declared final for safety*/
            final Token token = tokens.get( i );

            /*Forward token must only be non syntax related*/
            Token forwardToken = new Token( null, true );
            if ( i < tokens.size() - 1 ) {
                for ( int x = i + 1; x < tokens.size(); x++ ) {
                    forwardToken = tokens.get( x );
                    if ( forwardToken.isToken() ) {
                        break;
                    }
                }
            }

            /*If its not a syntax token, then we generate the switch case statements*/
            if ( token.isToken() ) {

                /*Reset x and c, set indexes*/
                appendResetCode( usingFor );
                swSIndex = C.length();
                tokenIndex = swSIndex;

                /*If we use a ^ this is what we do use the not eq and and*/
                C.append( StoredCode.SWITCH_START.code() );

                for ( int j = 0; j < token.getToken().length(); j++ ) {
                    C.append( String.format( StoredCode.CASE.code(), (int) token.getToken().charAt( j ) ) );
                    if ( OperatorType.isEq( EQ ) ) {
                        C.append( StoredCode.INDEXOF.code() );
                    }
                    C.append( StoredCode.BREAK.code() );
                }

                /*Takes into account equality operators.*/
                if ( !OperatorType.isEq( EQ ) ) {
                    C.append( StoredCode.DEFAULT.code() );
                    C.append( StoredCode.INDEXOF.code() );
                    C.append( StoredCode.BREAK.code() );
                }

                /*Append end code and reset EQ operator*/
                C.append( StoredCode.END.code() );
                EQ = OperatorType.EQ;
                AO = OperatorType.OR;

            } else {
                if ( Syntax.END.equalsToken( token ) ) {

                    /*Account for ending notation*/
                    C.insert( tokenIndex, StoredCode.END_X.code()
                            + StoredCode.RESET_C.code() );
                    tokenIndex = C.length();

                } else if ( Syntax.START.equalsToken( token ) ) {

                    /*Account for starting notation*/
                    C.insert( tokenIndex, StoredCode.START_X.code() + StoredCode.RESET_C.code() );
                    tokenIndex = C.length();

                } else if ( Syntax.BIND.equalsToken( token ) ) {

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
                    for ( int j = 0; j < forwardToken.getToken().length(); j++ ) {
                        beforeInsert = C.length();

                        /*Checks to see if its the last statement or not as we don't want trailing ||*/
                        if ( j < forwardToken.getToken().length() - 1 ) {
                            C.insert( nextInsert, String.format( StoredCode.IF_FORMAT.code(),
                                    EQ.op(), (int) forwardToken.getToken().charAt( j ), AO.op() ) );
                        } else {
                            C.insert( nextInsert, String.format( StoredCode.IF_FORMAT_NO_AO.code(),
                                    EQ.op(), (int) forwardToken.getToken().charAt( j ) ) );
                        }
                        nextInsert = nextInsert + ( C.length() - beforeInsert );
                    }

                    /*Insert new x specification to look behind*/
                    C.insert( nextInsert, StoredCode.END_START.code()
                            + StoredCode.LOOK_BEHIND.code()
                            + StoredCode.RESET_C.code()
                            + StoredCode.INC_MOD.code() );

                    tokens.remove( forwardToken );

                } else if ( Syntax.NOT.equalsToken( token ) ) {
                    EQ = OperatorType.NEQ;
                } else if ( Syntax.AND.equalsToken( token ) ) {
                    AO = OperatorType.AND;
                } else if ( Syntax.OR.equalsToken( token ) ) {

                    /*Insert Safety Code*/
                    insertSafetyCode( bindIndex, bindMod );

                    /*Finish if / switch by appending braces*/
                    diveMaster( 2 );

                    /*Reset Things*/
                    C.append( StoredCode.RESET_MOD.code() );
                    EQ = OperatorType.EQ;
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
     * Returns generated code as a string. CODE_GEN is class global so C.toString() does not have to be
     * called every time.
     *
     * @return Generated code
     */
    public String getGeneratedCode() {
        return GEN_CODE;
    }

    /**
     * Adds code for resetting 'c'. It checks if we are using a for loop or not. If we are, we also append 'x'
     * reset code.
     */
    private void appendResetCode( boolean usingFor ) {
        if ( usingFor ) {
            C.append( StoredCode.RESET_X.code() );
        }
        C.append( StoredCode.RESET_C.code() );
    }

    /**
     * This code inserts safety check code at the appropriate index with the appropriate comparator.
     * Index is passed to it rather than held in class. Could be changed.
     * <p/>
     * bindMod is the number of inserted if statements eg. [h].[i].[i] would have two if statements with a look
     * behind on each one, therefor x would be going down x - 2 at the final part. If we don't add code to check
     * for safety, we get an index out of bound exception if our x - 2 is less than 0.
     *
     * @param bindIndex Index to insert the code at
     * @param bindMod   Number of binding if statements as
     */
    private void insertSafetyCode( int bindIndex, int bindMod ) {
        if ( bindIndex != 0 ) {
            C.insert( bindIndex, String.format( StoredCode.IF_CHECK.code(), bindMod ) );
        }
    }

    /**
     * Adds brackets till specified depth has been reached. We simply pass C.toString to calculate depth each time
     * the loop runs. We then compare this to our wanted depth (lowest real depth is 0) and keep appending end code
     * until the appropriate depth is reached.
     *
     * @param depth Rig for dive!
     */
    private void diveMaster( int depth ) {
        while ( calculateDepth( C.toString() ) != depth ) {
            C.append( StoredCode.END.code() );
        }
    }

    /**
     * Creates string of tabs. Tabs are delimited as 4 spaces to follow standards, rather than a full tab.
     * Do not change this from StringBuilder to String. Remember that when concatenating Strings its not acceptable
     * to use Strings to do it. Eg. never use myString = myString.concat("bad!"); this is using them out of their
     * appropriate scope.
     *
     * @return Stacked tabs according to depth
     */
    private String stackTabs( int c ) {
        StringBuilder tab = new StringBuilder();
        for ( int i = 0; i < c; i++ ) {
            tab.append( "    " );
        }
        return tab.toString();
    }

    /**
     * This takes a string 's' and a token 'token'. It takes a token because, when calculating tab based depths,
     * a } is two tabs back rather than one tab back. So if we are trying to find the current depth at TotalCode +
     * token, we need to see if token contains a } and properly account for that.
     *
     * @param s     String to calculate depth of
     * @param token Current token
     * @return Code depth
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
     * Calculates depth by taking a string 's' and using a regex to split it on the delimiters below. We use this
     * because its easy, not because its the best, or the fastest. Its also the most readable solution.
     * <p/>
     * Quick note on regex's used: Negative depth is found by splitting at '}' and 'break;' which are the two
     * delimiters used to denote a closing of context used in the generated code. It then uses .length to count
     * the number of tokens found, which in turn is a way of counting the number of delimiters found. The same
     * idea is used for the positive split but with '{', 'case', and 'default :'.
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
     * reformatCode takes a StringBuilder, but, because of StringBuilders limitations it must be turned into a String
     * because StringBuilder does not contain a .split method. While this works, if one can recommend a better way to
     * accomplish this please suggest it.
     * <p/>
     * The idea is that this should loop through each token returned by splitting at every \n or newline char and then
     * pass the code so far into calculateDepth with the current token. This lets us find the current depth, and if
     * the token is '}' adjust the depth appropriately. See calculateDepth javadoc to see why this is. We then use
     * String.format method to properly add the tabs and the token to the formattedCode StringBuilder.
     *
     * @param codeToRefactor StringBuilder of code one wishes to refactor
     * @return Reformatted code as a StringBuilder
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
}
