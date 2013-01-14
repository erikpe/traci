package traci.lang.parser;

import java.io.IOException;

import org.antlr.runtime.ANTLRFileStream;
import org.antlr.runtime.ANTLRStringStream;
import org.antlr.runtime.CharStream;
import org.antlr.runtime.CommonTokenStream;
import org.antlr.runtime.RecognitionException;
import org.antlr.runtime.tree.CommonTree;
import org.antlr.runtime.tree.CommonTreeNodeStream;

import traci.lang.interpreter.Interpreter;
import traci.lang.interpreter.node.BlockNode;
import traci.main.Result;
import traci.main.Settings;
import traci.util.Log;
import traci.util.Utilities;

public class ParserRunner
{
    private final Settings settings;
    private final String code;
    private Interpreter interpreter = null;;

    public ParserRunner(final Settings settings, final String code)
    {
        this.settings = settings;
        this.code = code;
    }

    public Interpreter getInterpreter()
    {
        return interpreter;
    }

    public Result run()
    {
        Log.INFO("Parsing input file: '" + settings.inputFilename + "'");
        long start = System.currentTimeMillis();

        CharStream input = null;
        if (code == null)
        {
            try
            {
                input = new ANTLRFileStream(settings.inputFilename);
            }
            catch (final IOException e)
            {
                Log.ERROR("Unable to open input file: '" + settings.inputFilename + "':");
                Log.ERROR(e.getMessage());
                return Result.IO_ERROR;
            }
        }
        else
        {
            input = new ANTLRStringStream(code);
        }

        final TraciLexer lexer = new TraciLexer(input);
        final CommonTokenStream tokens = new CommonTokenStream(lexer);
        final TraciParser parser = new TraciParser(tokens);

        CommonTree tree = null;
        boolean encounteredErrors = false;
        try
        {
            tree = (CommonTree) parser.scene().getTree();
        }
        catch (final RecognitionException e)
        {
            encounteredErrors = true;
            final StringBuilder sb = new StringBuilder();
            if (e.token != null)
            {
                final IncludeLocation location = ((TraciToken) e.token).location;
                sb.append(location.toString()).append('\n');
            }
            sb.append("Parse error: ").append(e.getMessage());
            Log.ERROR(sb.toString());
        }

        for (final ParseError error : lexer.getLexerErrors())
        {
            encounteredErrors = true;
            Log.ERROR(error.includeLocation.toString() + "\nLexer error: " + error.msg);
        }

        for (final ParseError error : parser.getParseErrors())
        {
            encounteredErrors = true;
            Log.ERROR(error.includeLocation.toString() + "\nParse error: " + error.msg);
        }

        if (encounteredErrors)
        {
            return Result.PARSE_ERROR;
        }

        long stop = System.currentTimeMillis();

        Log.INFO("Parsing finished in " + Utilities.millisecondsToString(stop - start));
        Log.INFO("Building interpreter");

        start = System.currentTimeMillis();
        final CommonTreeNodeStream nodes = new CommonTreeNodeStream(tree);
        final TraciTreeWalker walker = new TraciTreeWalker(nodes);
        BlockNode blockNode = null;
        try
        {
            blockNode = walker.block();
        }
        catch (final RecognitionException e)
        {
            encounteredErrors = true;
            final StringBuilder sb = new StringBuilder();
            if (e.token != null)
            {
                final IncludeLocation location = ((TraciToken) e.token).location;
                sb.append(location.toString()).append('\n');
            }
            sb.append("Parse error: ").append(e.getMessage());
            Log.ERROR(sb.toString());
        }
        interpreter = new Interpreter(settings, blockNode);
        stop = System.currentTimeMillis();

        if (encounteredErrors)
        {
            return Result.PARSE_ERROR;
        }

        Log.INFO("Interpreter built in " + Utilities.millisecondsToString(stop - start));

        return Result.SUCCESS;
    }
}
