import java.io.File;

public class JPG {

    public static void main( String[] args ) {
        //Program run without arguments outputs this
        if ( args.length < 1 || args.length > 5 ) {
            System.out.println( "Example Usage:\n" +
                    "java JPG -d delims -o output.txt\n" +
                    "java JPG -f delimsfile.txt output.txt\n" +
                    "java JPG -r will add in code to remove null / blank tokens\n\n" +
                    "For help:\n" +
                    "java JPG -h || java JPG --help" );
            return;
        }

        //Help
        if ( args[0].equalsIgnoreCase( "-h" ) || args[0].equalsIgnoreCase( "--help" ) ) {
            System.out.println( "Surround delimiters with [] to mean any within.\n" +
                    "[].[] Means next to each other eg. [aeiouy].[e] is any vowel next to e.\n" +
                    "^ Denotes not eg. ^[a] would mean anything that's not 'a'.\n" +
                    "-[] means in front of a string. eg. string say hello parsed with [h]- would parse to [say ][ello].\n" +
                    "[]+ is the same thing but means ending.\n" +
                    "You can wrap eg. [[aeiouy].[e]]+ means any vowel next to e at the end of string." );
            return;
        }

        //Variables for arguments
        boolean returnDelims = false;
        boolean usingFile = false;
        boolean outputToFile = false;
        String delimiters = null;
        File delimFile = null;
        File outputFile = null;

        //Parse arguments
        for ( int i = 0; i < args.length; i++ ) {
            if ( args[i].equalsIgnoreCase( "-r" ) ) { //If we also return found delimiters
                returnDelims = true;
            } else if ( args[i].equalsIgnoreCase( "-f" ) ) { //Delim file / validity check
                delimFile = new File( args[i + 1] );
                if ( !delimFile.exists() || !delimFile.canRead() ) {
                    System.out.println( "Cannot read in delimiter file... Exiting." );
                    return;
                }
                usingFile = true;
            } else if ( args[i].equalsIgnoreCase( "-d" ) ) { //Get delimiters
                delimiters = args[i + 1];
            } else if ( args[i].equalsIgnoreCase( "-o" ) ) { //Output file / validity check
                outputFile = new File( args[i + 1] );
                if ( !outputFile.exists() || !outputFile.canWrite() ) {
                    System.out.println( "Output file does not exist or cannot be written to... Exiting." );
                    return;
                }
                outputToFile = true;
            }
        }

        /*Sets up the handler and parses everything*/
        if ( usingFile ) {
            ParseHandler parseGen = new ParseHandler( delimFile, outputFile, returnDelims );
            if ( outputToFile ) {
                parseGen.writeCode();
            } else {
                parseGen.displayCode();
            }
        } else {
            ParseHandler parseGen = new ParseHandler( delimiters, outputFile, returnDelims );
            if ( outputToFile ) {
                parseGen.writeCode();
            } else {
                parseGen.displayCode();
            }
        }
    }
}
