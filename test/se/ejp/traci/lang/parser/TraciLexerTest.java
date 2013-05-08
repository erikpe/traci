package se.ejp.traci.lang.parser;

import static org.junit.Assert.assertEquals;

import java.util.ArrayList;
import java.util.List;

import org.antlr.runtime.ANTLRStringStream;
import org.antlr.runtime.CharStream;
import org.antlr.runtime.CommonTokenStream;
import org.antlr.runtime.NoViableAltException;
import org.antlr.runtime.Token;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

public class TraciLexerTest
{
    CharStream code = null;
    TraciLexer lexer = null;
    CommonTokenStream tokenStream = null;
    List<? extends Token> tokens = null;
    List<ParseError> errors = null;

    @Before
    public void setUp() throws Exception
    {
    }

    @After
    public void tearDown() throws Exception
    {
        code = null;
        lexer = null;
        tokenStream = null;
        tokens = null;
        errors = null;
    }

    private void runLexer(final String strCode)
    {
        code = new ANTLRStringStream(strCode);
        lexer = new TraciLexer(code);
        tokenStream = new CommonTokenStream(lexer);
        tokenStream.fill();
        tokens = tokenStream.getTokens();
        errors = new ArrayList<ParseError>();
        for (final ParseError error : lexer.getLexerErrors())
        {
            errors.add(error);
        }
    }

    @Test
    public void testNoViableAltException()
    {
        runLexer("17%23;");
        assertEquals(1, errors.size());
        assertEquals(NoViableAltException.class, errors.get(0).e.getClass());
        assertEquals('@', errors.get(0).e.c);
    }
}
