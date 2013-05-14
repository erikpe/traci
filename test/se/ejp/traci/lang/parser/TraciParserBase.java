package se.ejp.traci.lang.parser;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import org.antlr.runtime.ANTLRFileStream;
import org.antlr.runtime.ANTLRStringStream;
import org.antlr.runtime.CharStream;
import org.antlr.runtime.CommonTokenStream;
import org.antlr.runtime.RecognitionException;
import org.antlr.runtime.Token;
import org.antlr.runtime.tree.CommonTree;

import se.ejp.traci.lang.preprocessor.PreprocessorRunner;
import se.ejp.traci.main.Result;
import se.ejp.traci.main.options.MockSettings;


public class TraciParserBase
{
    protected TraciParser parser = null;
    protected CommonTree parseTree = null;
    protected List<ParseError> parseErrors = null;

    protected void run(final CharStream input) throws RecognitionException
    {
        final TraciLexer lexer = new TraciLexer(input);
        parser = new TraciParser(new CommonTokenStream(lexer));
        parseErrors = new ArrayList<ParseError>();

        try
        {
            parseTree = (CommonTree) parser.scene().getTree();
        }
        finally
        {
            assertTrue(lexer.getLexerErrors().isEmpty());
            parseErrors = parser.getParseErrors();
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
        assertEquals(0, parseErrors.size());
    }

    protected void assertError()
    {
        assertFalse(parseErrors.isEmpty());
    }

    protected void assertError(final Class<? extends RecognitionException> clazz)
    {
        assertError(clazz, null, null);
    }

    protected void assertError(final Class<? extends RecognitionException> clazz, final Integer row, final Integer col)
    {
        boolean foundError = false;

        for (final ParseError error : parseErrors)
        {
            if (!clazz.equals(error.e.getClass()))
            {
                continue;
            }

            final Token token = error.e.token;

            if (row != null)
            {
                if (token instanceof TraciToken)
                {
                    assertEquals(token.getLine(), ((TraciToken) token).location.fileLocation.row);
                }
                if (row.intValue() != token.getLine())
                {
                    continue;
                }
            }

            if (col != null)
            {
                if (token instanceof TraciToken)
                {
                    assertEquals(token.getCharPositionInLine(), ((TraciToken) token).location.fileLocation.col);
                }
                if (col.intValue() != token.getCharPositionInLine())
                {
                    continue;
                }
            }

            foundError = true;
        }

        assertTrue(foundError);
    }
}
