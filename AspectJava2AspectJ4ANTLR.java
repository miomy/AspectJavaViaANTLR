import org.antlr.stringtemplate.*;
import org.antlr.runtime.*;
import org.stringtemplate.v4.*;
import java.io.*;

public class OBAspect2AspectJ4ANTLR {
    public static void main(String[] args) throws Exception {
        InputStreamReader groupFileR = new InputStreamReader(OBAspect2AspectJ4ANTLR.class.getResourceAsStream("AspectJ.stg"));
        StringTemplateGroup templates = new
            StringTemplateGroup(groupFileR);
        groupFileR.close();

        // read input from stdin
        ANTLRInputStream input = new ANTLRInputStream(System.in);
        // have the lexer read from the input stream
        OBAspectLexer lexer = new OBAspectLexer(input);
        // have the lexer create a stream of tokens
        CommonTokenStream tokens = new CommonTokenStream(lexer);
        // create a parser that reads the stream of tokens as input 
        OBAspectParser parser = new OBAspectParser(tokens);

        parser.setTemplateLib(templates);
        // invoke the parser by calling the function associated with
        // the start symbol 
        //parser.program();

        OBAspectParser.program_return r = parser.program();
        StringTemplate output = (StringTemplate)(r.getTemplate());
        System.out.println(output.toString());
    }
}
