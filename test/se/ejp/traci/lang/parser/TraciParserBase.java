package se.ejp.traci.lang.parser;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.antlr.runtime.ANTLRFileStream;
import org.antlr.runtime.ANTLRStringStream;
import org.antlr.runtime.CharStream;
import org.antlr.runtime.CommonTokenStream;
import org.antlr.runtime.RecognitionException;
import org.antlr.runtime.tree.CommonTree;

import se.ejp.traci.lang.preprocessor.PreprocessorRunner;
import se.ejp.traci.main.Result;
import se.ejp.traci.main.options.MockSettings;

public class TraciParserBase
{
    protected TraciParser parser = null;
    protected CommonTree parseTree = null;
    protected List<ParseError> parserErrors = null;

    protected void run(final CharStream input) throws RecognitionException
    {
        final TraciLexer lexer = new TraciLexer(input);
        parser = new TraciParser(new CommonTokenStream(lexer));
        parserErrors = new ArrayList<ParseError>();

        try
        {
            parseTree = (CommonTree) parser.scene().getTree();
        }
        finally
        {
            assertFalse(lexer.getLexerErrors().iterator().hasNext());
            for (final ParseError error : parser.getParseErrors())
            {
                parserErrors.add(error);
            }
        }
    }

    protected void runParser(final String code) throws RecognitionException
    {
        run(new ANTLRStringStream(code));
    }

    protected void runParserFile(final String filename) throws RecognitionException, IOException
    {
        run(new ANTLRFileStream(filename));
    }

    protected void runParserPreprocessedFile(final String filename) throws RecognitionException
    {
        final MockSettings settings = new MockSettings();
        settings.setInputFilename(filename);
        final PreprocessorRunner pp = new PreprocessorRunner(settings);
        final Result result = pp.run();
        assertEquals(Result.SUCCESS, result);
        runParser(pp.getProcessedCode());
    }

    protected void assertNoError()
    {
        assertEquals(0, parserErrors.size());
    }

    protected void assertError(final Class<? extends RecognitionException> clazz)
    {
        assertEquals(1, parserErrors.size());
        assertEquals(clazz, parserErrors.get(0).e.getClass());
    }

    protected void assertAllErrors(final Class<? extends RecognitionException> ... errorClasses)
    {
        final Set<Class<? extends RecognitionException>> expectedErrors =
                new HashSet<Class<? extends RecognitionException>>(Arrays.asList(errorClasses));
        final Set<Class<? extends RecognitionException>> encounteredErrors =
                new HashSet<Class<? extends RecognitionException>>();

        for (final ParseError error : parserErrors)
        {
            encounteredErrors.add(error.e.getClass());
        }

        assertEquals(expectedErrors, encounteredErrors);
    }
}
