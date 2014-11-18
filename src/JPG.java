import java.io.File;

public class JPG {

    public static void main( String[] args ) {
        /*Usage for help and checks for proper amounts of arguments*/
        if ( args.length < 1 || args.length > 5 ) {
            System.out.println( ReadOut.NOPARAM.getText() );
            return;
        }

        if ( args[0].equalsIgnoreCase( "-h" ) || args[0].equalsIgnoreCase( "--help" ) ) {
            System.out.println( ReadOut.HELP.getText() );
            return;
        }

        /*Argument variables*/
        boolean returnDelims = false;
        boolean usingFile = false; //If where using an input file
        boolean outputToFile = false; //If where using an output file
        String delimiters = null;
        File delimFile = null;
        File outputFile = null;

        /*Find arguments*/
        for ( int i = 0; i < args.length; i++ ) {
            if ( args[i].equalsIgnoreCase( "-r" ) ) {
                returnDelims = true;
            } else if ( args[i].equalsIgnoreCase( "-f" ) ) {
                delimFile = new File( args[i + 1] );
                if ( !delimFile.exists() || !delimFile.canRead() ) {
                    System.out.println( "Cannot read in delimiter file... Exiting." );
                    return;
                }
                usingFile = true;
            } else if ( args[i].equalsIgnoreCase( "-d" ) ) {
                delimiters = args[i + 1];
            } else if ( args[i].equalsIgnoreCase( "-o" ) ) {
                /*Checks output file and stops program if its not valid*/
                outputFile = new File( args[i + 1] );
                if ( !outputFile.exists() || !outputFile.canWrite() ) {
                    System.out.println( "Output file does not exist or cannot be written to... Exiting." );
                    return;
                }
                outputToFile = true;
            }
        }

        /*Sets up the handler and parses everything. If there is no output file we just display the code*/
        if ( usingFile ) {
            if ( outputToFile ) {
                ParseHandler parseGen = new ParseHandler( delimFile, outputFile, returnDelims );
                parseGen.writeCode();
            } else {
                ParseHandler parseGen = new ParseHandler( delimFile, returnDelims );
                parseGen.displayCode();
            }
        } else {
            if ( outputToFile ) {
                ParseHandler parseGen = new ParseHandler( delimiters, outputFile, returnDelims );
                parseGen.writeCode();
            } else {
                ParseHandler parseGen = new ParseHandler( delimiters, returnDelims );
                parseGen.displayCode();
            }
        }

        System.out.println( "Generation success!" );
    }
}
