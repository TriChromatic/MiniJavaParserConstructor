/**
 * Enumerated type for operators
 * Created by TriChromatic aka Dylan Eicher on 11/20/14.
 */
public enum OperatorType {
    EQ( "==" ), NEQ( "!=" ), OR( "||" ), AND( "&&" );

    private final String myOperator;

    private OperatorType( String s ) {
        myOperator = s;
    }

    /**
     * @param test Operator to test
     * @return True if the operator passed in is the EQ operator
     */
    public static boolean isEq( OperatorType test ) {
        return test.equals( EQ );
    }

    /**
     * @return Operator
     */
    public String op() {
        return myOperator;
    }
}
