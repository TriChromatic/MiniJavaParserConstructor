import java.io.*;
import java.nio.charset.Charset;
import java.nio.file.Files;
import java.nio.file.Paths;

/**
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
     * @param delims
     * @param outputFile
     * @param removeNull
     */
    ParseHandler( String delims, File outputFile, boolean removeNull ) {
        this.DELIMS = delims;
        this.OUT_FILE = outputFile;
        this.REMOVE = removeNull;

        /*Create the parser and generator. Do both.*/
        this.PARSER = new DelimiterParser( this.DELIMS );
        this.GENERATOR = new ParserGenerator( PARSER.getTokens(), this.REMOVE );
    }

    /**
     * @param delims
     * @param outputFile
     * @param removeNull
     */
    ParseHandler( File delims, File outputFile, boolean removeNull ) {
        /*Attempt to read in the file*/
        String temp = "";
        try {
            temp = readFile( delims.getAbsolutePath(), Charset.defaultCharset() );
        } catch ( IOException e ) {
            e.printStackTrace();
        }

        this.DELIMS = temp;
        this.OUT_FILE = outputFile;
        this.REMOVE = removeNull;

        /*Create the parser and generator. Do both.*/
        this.PARSER = new DelimiterParser( this.DELIMS );
        this.GENERATOR = new ParserGenerator( PARSER.getTokens(), this.REMOVE );
    }

    /**
     * @param path
     * @param encoding
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

    public void displayCode() {
        System.out.println( GENERATOR.getGeneratedCode() );
    }
}
