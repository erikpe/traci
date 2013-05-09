package se.ejp.traci.lang.parser;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import org.antlr.runtime.ANTLRFileStream;
import org.antlr.runtime.ANTLRStringStream;
import org.antlr.runtime.CharStream;
import org.antlr.runtime.CommonTokenStream;
import org.antlr.runtime.Token;

import se.ejp.traci.lang.preprocessor.PreprocessorRunner;
import se.ejp.traci.main.Result;
import se.ejp.traci.main.options.MockSettings;

public class TraciLexerBase
{
    protected TraciLexer lexer = null;
    protected List<? extends Token> tokens = null;
    protected List<ParseError> lexerErrors = null;

    private void run(final CharStream input)
    {
        lexer = new TraciLexer(input);
        final CommonTokenStream tokenStream = new CommonTokenStream(lexer);
        tokenStream.fill();
        tokens = tokenStream.getTokens();
        lexerErrors = new ArrayList<ParseError>();
        for (final ParseError error : lexer.getLexerErrors())
        {
            lexerErrors.add(error);
        }
    }

    protected void runLexer(final String code)
    {
        run(new ANTLRStringStream(code));
    }

    protected void runLexerFile(final String filename) throws IOException
    {
        run(new ANTLRFileStream(filename));
    }

    protected void runLexerPreprocessedFile(final String filename)
    {
        final MockSettings settings = new MockSettings();
        settings.setInputFilename(filename);
        final PreprocessorRunner pp = new PreprocessorRunner(settings);
        final Result result = pp.run();
        assertEquals(Result.SUCCESS, result);
        runLexer(pp.getProcessedCode());
    }

    protected void assertNoError()
    {
        assertEquals(0, lexerErrors.size());
    }

    protected void assertErrors(final int numErrors)
    {
        assertEquals(numErrors, lexerErrors.size());
    }

    protected void assertToken(final int tokenNr, final Integer tokenType, final String tokenText, final Integer channel)
    {
        assertToken(tokenNr, tokenType, tokenText, channel, null, null);
    }

    protected void assertToken(final int tokenNr, final Integer tokenType, final String tokenText,
            final Integer channel, final Integer row, final Integer col)
    {
        assertTrue(tokens.size() > tokenNr);

        final Token token = tokens.get(tokenNr);

        if (tokenType != null)
        {
            assertEquals(tokenType.intValue(), token.getType());
        }

        if (tokenText != null)
        {
            assertEquals(tokenText, token.getText());
        }

        if (channel != null)
        {
            assertEquals(channel.intValue(), token.getChannel());
        }

        if (row != null)
        {
            if (token instanceof TraciToken)
            {
                assertEquals(row.intValue(), ((TraciToken) token).location.fileLocation.row);
            }
            assertEquals(row.intValue(), token.getLine());
        }

        if (col != null)
        {
            if (token instanceof TraciToken)
            {
                assertEquals(col.intValue(), ((TraciToken) token).location.fileLocation.col);
            }
            assertEquals(col.intValue(), token.getCharPositionInLine());
        }
    }
}
