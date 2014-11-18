import java.io.*;
import java.nio.charset.Charset;
import java.nio.file.Files;
import java.nio.file.Paths;

/**
 * Parser handler takes data from a driver class and then constructs proper delimeterparsers and parsergenerators for
 * the display and write methods.
 * Created by TriChromatic aka Dylan Eicher on 11/17/14.
 */
public class ParseHandler {
    //Everything set to private and final as preferred
    private final String DELIMS;
    private final File OUT_FILE;
    private final boolean REMOVE;
    private final DelimiterParser PARSER;
    private final ParserGenerator GENERATOR;

    /**
     * Initializer that gets delimiters from a user imputed string
     *
     * @param delims     User imputed delimiters
     * @param outputFile File to output generated code to
     * @param removeNull If we should add code to remove null tokens
     */
    ParseHandler( String delims, File outputFile, boolean removeNull ) {
        DELIMS = delims;
        OUT_FILE = outputFile;
        REMOVE = removeNull;

        /*Create the parser and generator. Do both.*/
        PARSER = new DelimiterParser( DELIMS );
        GENERATOR = new ParserGenerator( PARSER.getTokens(), REMOVE );
    }

    /**
     * Initializer that reads in delimiter file
     *
     * @param delims     User imputed delimiters from a file
     * @param outputFile File to output generated code to
     * @param removeNull If we should add code to remove null tokens
     */
    ParseHandler( File delims, File outputFile, boolean removeNull ) {
        /*Attempt to read in the file*/
        String temp = "";
        try {
            temp = readFile( delims.getAbsolutePath(), Charset.defaultCharset() );
        } catch ( IOException e ) {
            e.printStackTrace();
        }

        DELIMS = temp;
        OUT_FILE = outputFile;
        REMOVE = removeNull;

        /*Create the parser and generator. Do both.*/
        PARSER = new DelimiterParser( DELIMS );
        GENERATOR = new ParserGenerator( PARSER.getTokens(), REMOVE );
    }

    /**
     * Initializer with no write to file parameter
     *
     * @param delims     User imputed delimiters
     * @param removeNull If we should add code to remove null tokens
     */
    ParseHandler( String delims, boolean removeNull ) {
        DELIMS = delims;
        REMOVE = removeNull;
        OUT_FILE = null;

        /*Create the parser and generator. Do both.*/
        PARSER = new DelimiterParser( DELIMS );
        GENERATOR = new ParserGenerator( PARSER.getTokens(), REMOVE );
    }

    /**
     * Initializer with no write to file parameter, but a read in file parameter
     *
     * @param delims     User imputed delimiters from a file
     * @param removeNull If we should add code to remove null tokens
     */
    ParseHandler( File delims, boolean removeNull ) {
        /*Attempt to read in the file*/
        String temp = "";
        try {
            temp = readFile( delims.getAbsolutePath(), Charset.defaultCharset() );
        } catch ( IOException e ) {
            e.printStackTrace();
        }

        DELIMS = temp;
        OUT_FILE = null;
        REMOVE = removeNull;

        /*Create the parser and generator. Do both.*/
        PARSER = new DelimiterParser( DELIMS );
        GENERATOR = new ParserGenerator( PARSER.getTokens(), REMOVE );
    }

    /**
     * @param path     Absolute path to a file
     * @param encoding Charset to return file contents eg. UTF-8
     * @return File contents as string
     * @throws IOException
     */
    private String readFile( String path, Charset encoding )
            throws IOException {
        byte[] encoded = Files.readAllBytes( Paths.get( path ) );
        return new String( encoded, encoding );
    }

    /**
     * Writes generated code to file
     */
    public void writeCode() {
        try ( PrintStream out = new PrintStream( new FileOutputStream( OUT_FILE.getAbsoluteFile() ) ) ) {
            out.print( GENERATOR.getGeneratedCode() );
        } catch ( FileNotFoundException e ) {
            e.printStackTrace();
        }
    }

    /**
     * Displays generated code on screen
     */
    public void displayCode() {
        System.out.println( GENERATOR.getGeneratedCode() );
    }
}
