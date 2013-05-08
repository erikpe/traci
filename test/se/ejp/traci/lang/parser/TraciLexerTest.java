package se.ejp.traci.lang.parser;

import static org.junit.Assert.assertEquals;

import java.util.ArrayList;
import java.util.List;

import org.antlr.runtime.ANTLRStringStream;
import org.antlr.runtime.CommonTokenStream;
import org.antlr.runtime.NoViableAltException;
import org.antlr.runtime.Token;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

public class TraciLexerTest
{
    TraciLexer lexer = null;
    List<? extends Token> tokens = null;
    List<ParseError> lexerErrors = null;

    @Before
    public void setUp()
    {
    }

    @After
    public void tearDown()
    {
        lexer = null;
        tokens = null;
        lexerErrors = null;
    }

    private void runLexer(final String code)
    {
        lexer = new TraciLexer(new ANTLRStringStream(code));
        final CommonTokenStream tokenStream = new CommonTokenStream(lexer);
        tokenStream.fill();
        tokens = tokenStream.getTokens();
        lexerErrors = new ArrayList<ParseError>();
        for (final ParseError error : lexer.getLexerErrors())
        {
            lexerErrors.add(error);
        }
    }

    @Test
    public void testLexer()
    {
        runLexer("17+23;");
        assertEquals(0, lexerErrors.size());
        assertEquals(5, tokens.size());
    }

    @Test
    public void testNoViableAltException()
    {
        runLexer("17%23;");
        assertEquals(1, lexerErrors.size());
        assertEquals(NoViableAltException.class, lexerErrors.get(0).e.getClass());
        assertEquals('%', lexerErrors.get(0).e.c);
    }
}
