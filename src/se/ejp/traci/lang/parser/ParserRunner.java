package se.ejp.traci.lang.parser;

import java.io.IOException;

import org.antlr.runtime.ANTLRFileStream;
import org.antlr.runtime.ANTLRStringStream;
import org.antlr.runtime.CharStream;
import org.antlr.runtime.CommonTokenStream;
import org.antlr.runtime.RecognitionException;
import org.antlr.runtime.tree.CommonTree;
import org.antlr.runtime.tree.CommonTreeNodeStream;

import se.ejp.traci.lang.interpreter.Interpreter;
import se.ejp.traci.lang.interpreter.exceptions.InterpreterInternalException;
import se.ejp.traci.lang.interpreter.node.BlockNode;
import se.ejp.traci.main.Result;
import se.ejp.traci.main.options.Settings;
import se.ejp.traci.util.Log;
import se.ejp.traci.util.Utilities;

public class ParserRunner
{
    private final Settings settings;
    private final String code;

    private Interpreter interpreter = null;
    private CharStream input = null;
    private CommonTree tree = null;

    public ParserRunner(final Settings settings, final String code)
    {
        this.settings = settings;
        this.code = code;
    }

    public Interpreter getInterpreter()
    {
        return interpreter;
    }

    private Result getInput()
    {
        final String inputFilename = settings.getInputFilename();

        if (code == null)
        {
            try
            {
                input = new ANTLRFileStream(inputFilename);
            }
            catch (final IOException e)
            {
                Log.ERROR("Unable to open input file: '" + inputFilename + "':");
                Log.ERROR(e.getMessage());
                return Result.IO_ERROR;
            }
        }
        else
        {
            input = new ANTLRStringStream(code);
        }

        return Result.SUCCESS;
    }

    private Result runParser()
    {
        final TraciLexer lexer = new TraciLexer(input);
        final CommonTokenStream tokens = new CommonTokenStream(lexer);
        final TraciParser parser = new TraciParser(tokens);

        boolean encounteredErrors = false;
        try
        {
            tree = (CommonTree) parser.scene().getTree();
        }
        catch (final RecognitionException e)
        {
            encounteredErrors = true;
            Log.ERROR(ParserUtilities.makeErrorMessage(e, "Parse error"));
        }

        for (final ParseError error : lexer.getLexerErrors())
        {
            encounteredErrors = true;
            Log.ERROR(error.fullMsg());
        }

        for (final ParseError error : parser.getParseErrors())
        {
            encounteredErrors = true;
            Log.ERROR(error.fullMsg());
        }

        if (encounteredErrors)
        {
            return Result.PARSE_ERROR;
        }

        return Result.SUCCESS;
    }

    private Result buildInterpreter()
    {
        final CommonTreeNodeStream nodes = new CommonTreeNodeStream(tree);
        final TraciTreeWalker walker = new TraciTreeWalker(nodes);

        BlockNode blockNode = null;
        try
        {
            blockNode = walker.block();
        }
        catch (final RecognitionException e)
        {
            Log.ERROR(ParserUtilities.makeErrorMessage(e, "Tree walker error"));
            return Result.PARSE_ERROR;
        }
        catch (final InterpreterInternalException e)
        {
            Log.ERROR(e.fullMsg());
            return Result.INTERNAL_ERROR;
        }

        interpreter = new Interpreter(settings, blockNode);

        return Result.SUCCESS;
    }

    public Result run()
    {
        Log.INFO("Parsing input file: '" + settings.getInputFilename() + "'");
        long start = System.currentTimeMillis();

        Result result = getInput();
        if (result != Result.SUCCESS)
        {
            return result;
        }

        result = runParser();
        if (result != Result.SUCCESS)
        {
            return result;
        }

        long stop = System.currentTimeMillis();
        Log.INFO("Parsing finished in " + Utilities.millisecondsToString(stop - start));

        Log.INFO("Building interpreter");
        start = System.currentTimeMillis();

        result = buildInterpreter();
        if (result != Result.SUCCESS)
        {
            return result;
        }

        stop = System.currentTimeMillis();
        Log.INFO("Interpreter built in " + Utilities.millisecondsToString(stop - start));

        return Result.SUCCESS;
    }
}
