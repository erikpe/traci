package se.ejp.traci.lang.parser;

import static org.junit.Assert.assertEquals;

import java.io.IOException;

import org.antlr.runtime.EarlyExitException;
import org.antlr.runtime.NoViableAltException;
import org.antlr.runtime.Token;
import org.junit.Test;

public class TraciLexerTest extends TraciLexerBase
{
    @Test
    public void testLexer()
    {
        runLexer("17+\n23;");
        assertNoError();
        assertEquals(6, tokens.size());
        assertToken(0, TraciLexer.INT,          "17", Token.DEFAULT_CHANNEL, 1, 0);
        assertToken(1, TraciLexer.PLUS_OP,       "+", Token.DEFAULT_CHANNEL, 1, 2);
        assertToken(2, TraciLexer.WS,           "\n", Token.HIDDEN_CHANNEL,  1, 3);
        assertToken(3, TraciLexer.INT,          "23", Token.DEFAULT_CHANNEL, 2, 0);
        assertToken(4, TraciLexer.SEMICOLON,     ";", Token.DEFAULT_CHANNEL, 2, 2);
        assertToken(5, TraciLexer.EOF,       "<EOF>", Token.DEFAULT_CHANNEL, 2, 3);
    }

    @Test
    public void testNoViableAltException()
    {
        runLexer("17%23;");
        assertErrors(1);
        assertEquals(NoViableAltException.class, lexerErrors.get(0).e.getClass());
        assertEquals('%', lexerErrors.get(0).e.c);
    }

    @Test
    public void testFloat()
    {
        runLexer(".23");
        assertNoError();
        assertToken(0, TraciLexer.FLOAT, ".23", Token.DEFAULT_CHANNEL, 1, 0);

        runLexer("2.23.23");
        assertNoError();
        assertToken(0, TraciLexer.FLOAT, "2.23", Token.DEFAULT_CHANNEL, 1, 0);

        runLexer("2.23e-10-5");
        assertNoError();
        assertToken(0, TraciLexer.FLOAT, "2.23e-10", Token.DEFAULT_CHANNEL, 1, 0);

        runLexer(".23E+13.2");
        assertNoError();
        assertToken(0, TraciLexer.FLOAT, ".23E+13", Token.DEFAULT_CHANNEL, 1, 0);

        runLexer("23E13");
        assertNoError();
        assertToken(0, TraciLexer.FLOAT, "23E13", Token.DEFAULT_CHANNEL, 1, 0);

        runLexer("1.2e+");
        assertErrors(1);
        assertEquals(EarlyExitException.class, lexerErrors.get(0).e.getClass());
    }

    @Test
    public void testLexerFile() throws IOException
    {
        runLexerFile("testcode/fibonacci.traci");
        assertNoError();
        assertToken(0, TraciLexer.DEF,  "def", Token.DEFAULT_CHANNEL, 1, 0);
        assertToken(1, TraciLexer.WS,     " ", Token.HIDDEN_CHANNEL,  1, 3);
        assertToken(2, TraciLexer.ID,   "fib", Token.DEFAULT_CHANNEL, 1, 4);
        assertToken(3, TraciLexer.LPAR,   "(", Token.DEFAULT_CHANNEL, 1, 7);
        assertToken(4, TraciLexer.ID,     "n", Token.DEFAULT_CHANNEL, 1, 8);
        assertToken(5, TraciLexer.RPAR,   ")", Token.DEFAULT_CHANNEL, 1, 9);
        assertToken(6, TraciLexer.WS,    "\r", Token.HIDDEN_CHANNEL,  1, 10);
        assertToken(7, TraciLexer.WS,    "\n", Token.HIDDEN_CHANNEL,  1, 11);
        assertToken(8, TraciLexer.LCURLY, "{", Token.DEFAULT_CHANNEL, 2, 0);
    }

    @Test
    public void testLexerWithPreprocessor()
    {
        runLexerPreprocessedFile("testcode/fibonacci.traci");
        assertNoError();
        assertToken(0, TraciLexer.PPLINE, null, Token.HIDDEN_CHANNEL, 1, 0);
        assertToken(1, TraciLexer.WS,     "\n", Token.HIDDEN_CHANNEL);
        assertToken(2, TraciLexer.DEF,   "def", Token.DEFAULT_CHANNEL, 1, 0);
        assertToken(3, TraciLexer.WS,      " ", Token.HIDDEN_CHANNEL,  1, 3);
        assertToken(4, TraciLexer.ID,    "fib", Token.DEFAULT_CHANNEL, 1, 4);
        assertToken(5, TraciLexer.LPAR,    "(", Token.DEFAULT_CHANNEL, 1, 7);
        assertToken(6, TraciLexer.ID,      "n", Token.DEFAULT_CHANNEL, 1, 8);
        assertToken(7, TraciLexer.RPAR,    ")", Token.DEFAULT_CHANNEL, 1, 9);
    }
}
