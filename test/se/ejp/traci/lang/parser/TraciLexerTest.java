package se.ejp.traci.lang.parser;

import static org.junit.Assert.assertEquals;

import java.io.IOException;

import org.antlr.runtime.NoViableAltException;
import org.antlr.runtime.Token;
import org.junit.Test;

public class TraciLexerTest extends TraciLexerBase
{
    @Test
    public void testLexer()
    {
        runLexer("17+\n23;");
        assertEquals(0, lexerErrors.size());
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
        assertEquals(1, lexerErrors.size());
        assertEquals(NoViableAltException.class, lexerErrors.get(0).e.getClass());
        assertEquals('%', lexerErrors.get(0).e.c);
    }

    @Test
    public void testLexerFile() throws IOException
    {
        runLexerFile("testcode/fibonacci.traci");
        assertEquals(0, lexerErrors.size());
        assertToken(0, TraciLexer.DEF, "def", Token.DEFAULT_CHANNEL, 1, 0);
        assertToken(1, TraciLexer.WS,    " ", Token.HIDDEN_CHANNEL,  1, 3);
        assertToken(2, TraciLexer.ID,  "fib", Token.DEFAULT_CHANNEL, 1, 4);
        assertToken(3, TraciLexer.LPAR,  "(", Token.DEFAULT_CHANNEL, 1, 7);
        assertToken(4, TraciLexer.ID,    "n", Token.DEFAULT_CHANNEL, 1, 8);
        assertToken(5, TraciLexer.RPAR,  ")", Token.DEFAULT_CHANNEL, 1, 9);
    }

    @Test
    public void testLexerWithPreprocessor()
    {
        runLexerPreprocessedFile("testcode/fibonacci.traci");
        assertEquals(0, lexerErrors.size());
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
