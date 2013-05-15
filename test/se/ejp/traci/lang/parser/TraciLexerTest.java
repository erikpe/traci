package se.ejp.traci.lang.parser;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

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
        assertToken(0, TraciLexer.INT,      "17", Token.DEFAULT_CHANNEL, 1, 0);
        assertToken(1, TraciLexer.PLUS_OP,   "+", Token.DEFAULT_CHANNEL, 1, 2);
        assertToken(2, TraciLexer.WS,       "\n", Token.HIDDEN_CHANNEL,  1, 3);
        assertToken(3, TraciLexer.INT,      "23", Token.DEFAULT_CHANNEL, 2, 0);
        assertToken(4, TraciLexer.SEMICOLON, ";", Token.DEFAULT_CHANNEL, 2, 2);
        assertToken(5, TraciLexer.EOF,   "<EOF>", Token.DEFAULT_CHANNEL, 2, 3);
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
    public void testFibonacciFromFile() throws IOException
    {
        runLexerFile("testcode/fibonacci.traci");
        assertNoError();
        assertToken(0, TraciLexer.DEF,    "def", Token.DEFAULT_CHANNEL, 1, 0);
        assertToken(1, TraciLexer.WS,     " ",   Token.HIDDEN_CHANNEL,  1, 3);
        assertToken(2, TraciLexer.ID,     "fib", Token.DEFAULT_CHANNEL, 1, 4);
        assertToken(3, TraciLexer.LPAR,   "(",   Token.DEFAULT_CHANNEL, 1, 7);
        assertToken(4, TraciLexer.ID,     "n",   Token.DEFAULT_CHANNEL, 1, 8);
        assertToken(5, TraciLexer.RPAR,   ")",   Token.DEFAULT_CHANNEL, 1, 9);
        assertToken(6, TraciLexer.WS,     null,  Token.HIDDEN_CHANNEL,  1, 10);
        assertToken(7, TraciLexer.LCURLY, "{",   Token.DEFAULT_CHANNEL, 2, 0);
    }

    @Test
    public void testFibonacciWithPreprocessor()
    {
        runLexerPreprocessedFile("testcode/fibonacci.traci");
        assertNoError();
        assertToken(0, TraciLexer.PPLINE, null,  Token.HIDDEN_CHANNEL, 1, 0);
        assertToken(1, TraciLexer.WS,     null,  Token.HIDDEN_CHANNEL);
        assertToken(2, TraciLexer.DEF,    "def", Token.DEFAULT_CHANNEL, 1, 0);
        assertToken(3, TraciLexer.WS,     " ",   Token.HIDDEN_CHANNEL,  1, 3);
        assertToken(4, TraciLexer.ID,     "fib", Token.DEFAULT_CHANNEL, 1, 4);
        assertToken(5, TraciLexer.LPAR,   "(",   Token.DEFAULT_CHANNEL, 1, 7);
        assertToken(6, TraciLexer.ID,     "n",   Token.DEFAULT_CHANNEL, 1, 8);
        assertToken(7, TraciLexer.RPAR,   ")",   Token.DEFAULT_CHANNEL, 1, 9);
        assertToken(8, TraciLexer.WS,     null,  Token.HIDDEN_CHANNEL,  1, 10);
        assertToken(9, TraciLexer.LCURLY, "{",   Token.DEFAULT_CHANNEL, 2, 0);
    }

    @Test
    public void testInclude()
    {
        runLexerPreprocessedFile("testcode/a.traci");
        assertNoError();
        assertToken( 0, TraciLexer.PPLINE,    null, Token.HIDDEN_CHANNEL, 1, 0);
        assertToken( 1, TraciLexer.WS,        null, Token.HIDDEN_CHANNEL);
        assertToken( 2, TraciLexer.INT,       "17", Token.DEFAULT_CHANNEL, 1, 0);
        assertToken( 3, TraciLexer.WS,        null, Token.HIDDEN_CHANNEL,  1, 2);
        assertToken( 4, TraciLexer.PPLINE,    null, Token.HIDDEN_CHANNEL,  2, 0);
        assertToken( 5, TraciLexer.WS,        null, Token.HIDDEN_CHANNEL);
        assertToken( 6, TraciLexer.PLUS_OP,   "+",  Token.DEFAULT_CHANNEL, 1, 0);
        assertToken( 7, TraciLexer.WS,        null, Token.HIDDEN_CHANNEL,  1, 1);
        assertToken( 8, TraciLexer.INT,       "23", Token.DEFAULT_CHANNEL, 2, 1);
        assertToken( 9, TraciLexer.WS,        null, Token.HIDDEN_CHANNEL,  2, 3);
        assertToken(10, TraciLexer.PPLINE,    null, Token.HIDDEN_CHANNEL,  3, 0);
        assertToken(11, TraciLexer.WS,        null, Token.HIDDEN_CHANNEL);
        assertToken(12, TraciLexer.SEMICOLON, ";",  Token.DEFAULT_CHANNEL, 3, 0);
        assertToken(13, TraciLexer.WS,        null, Token.HIDDEN_CHANNEL,  3, 1);
        assertToken(14, TraciLexer.EOF,       null, Token.DEFAULT_CHANNEL, 4, 0);

        TraciToken tok = (TraciToken) tokens.get(2);
        assertTrue(tok.location.fileLocation.filename.endsWith("a.traci"));
        assertTrue(tok.location.includePath.isEmpty());

        tok = (TraciToken) tokens.get(8);
        assertTrue(tok.location.fileLocation.filename.endsWith("b.traci"));
        assertEquals(1, tok.location.includePath.size());
        assertEquals(2, tok.location.includePath.get(0).row);
        assertEquals(0, tok.location.includePath.get(0).col);
        assertTrue(tok.location.includePath.get(0).filename.endsWith("a.traci"));

        tok = (TraciToken) tokens.get(12);
        assertTrue(tok.location.fileLocation.filename.endsWith("a.traci"));
        assertTrue(tok.location.includePath.isEmpty());
    }

    @Test
    public void testTransformation()
    {
        runLexer("translate scale scalex scaley scalez rotx roty rotz rotAround rotVecToVec identity");
        assertNoError();
        assertToken( 0, TraciLexer.TRANSFORMATION, "translate",   Token.DEFAULT_CHANNEL, 1,  0);
        assertToken( 1, TraciLexer.WS,             " ",           Token.HIDDEN_CHANNEL,  1,  9);
        assertToken( 2, TraciLexer.TRANSFORMATION, "scale",       Token.DEFAULT_CHANNEL, 1, 10);
        assertToken( 3, TraciLexer.WS,             " ",           Token.HIDDEN_CHANNEL,  1, 15);
        assertToken( 4, TraciLexer.TRANSFORMATION, "scalex",      Token.DEFAULT_CHANNEL, 1, 16);
        assertToken( 5, TraciLexer.WS,             " ",           Token.HIDDEN_CHANNEL,  1, 22);
        assertToken( 6, TraciLexer.TRANSFORMATION, "scaley",      Token.DEFAULT_CHANNEL, 1, 23);
        assertToken( 7, TraciLexer.WS,             " ",           Token.HIDDEN_CHANNEL,  1, 29);
        assertToken( 8, TraciLexer.TRANSFORMATION, "scalez",      Token.DEFAULT_CHANNEL, 1, 30);
        assertToken( 9, TraciLexer.WS,             " ",           Token.HIDDEN_CHANNEL,  1, 36);
        assertToken(10, TraciLexer.TRANSFORMATION, "rotx",        Token.DEFAULT_CHANNEL, 1, 37);
        assertToken(11, TraciLexer.WS,             " ",           Token.HIDDEN_CHANNEL,  1, 41);
        assertToken(12, TraciLexer.TRANSFORMATION, "roty",        Token.DEFAULT_CHANNEL, 1, 42);
        assertToken(13, TraciLexer.WS,             " ",           Token.HIDDEN_CHANNEL,  1, 46);
        assertToken(14, TraciLexer.TRANSFORMATION, "rotz",        Token.DEFAULT_CHANNEL, 1, 47);
        assertToken(15, TraciLexer.WS,             " ",           Token.HIDDEN_CHANNEL,  1, 51);
        assertToken(16, TraciLexer.TRANSFORMATION, "rotAround",   Token.DEFAULT_CHANNEL, 1, 52);
        assertToken(17, TraciLexer.WS,             " ",           Token.HIDDEN_CHANNEL,  1, 61);
        assertToken(18, TraciLexer.TRANSFORMATION, "rotVecToVec", Token.DEFAULT_CHANNEL, 1, 62);
        assertToken(19, TraciLexer.WS,             " ",           Token.HIDDEN_CHANNEL,  1, 73);
        assertToken(20, TraciLexer.TRANSFORMATION, "identity",    Token.DEFAULT_CHANNEL, 1, 74);
        assertToken(21, TraciLexer.EOF,            null,          Token.DEFAULT_CHANNEL, 1, 82);
    }

    @Test
    public void testPrimitiveShape()
    {
        runLexer("box cylinder plane sphere torus");
        assertNoError();
        assertToken(0, TraciLexer.PRIMITIVE_SHAPE, "box",      Token.DEFAULT_CHANNEL, 1,  0);
        assertToken(1, TraciLexer.WS,              " ",        Token.HIDDEN_CHANNEL,  1,  3);
        assertToken(2, TraciLexer.PRIMITIVE_SHAPE, "cylinder", Token.DEFAULT_CHANNEL, 1,  4);
        assertToken(3, TraciLexer.WS,              " ",        Token.HIDDEN_CHANNEL,  1, 12);
        assertToken(4, TraciLexer.PRIMITIVE_SHAPE, "plane",    Token.DEFAULT_CHANNEL, 1, 13);
        assertToken(5, TraciLexer.WS,              " ",        Token.HIDDEN_CHANNEL,  1, 18);
        assertToken(6, TraciLexer.PRIMITIVE_SHAPE, "sphere",   Token.DEFAULT_CHANNEL, 1, 19);
        assertToken(7, TraciLexer.WS,              " ",        Token.HIDDEN_CHANNEL,  1, 25);
        assertToken(8, TraciLexer.PRIMITIVE_SHAPE, "torus",    Token.DEFAULT_CHANNEL, 1, 26);
        assertToken(9, TraciLexer.EOF,             null,       Token.DEFAULT_CHANNEL, 1, 31);
    }

    @Test
    public void testCsgShape()
    {
        runLexer("union difference intersection");
        assertNoError();
        assertToken(0, TraciLexer.CSG_SHAPE, "union",        Token.DEFAULT_CHANNEL, 1,  0);
        assertToken(1, TraciLexer.WS,        " ",            Token.HIDDEN_CHANNEL,  1,  5);
        assertToken(2, TraciLexer.CSG_SHAPE, "difference",   Token.DEFAULT_CHANNEL, 1,  6);
        assertToken(3, TraciLexer.WS,        " ",            Token.HIDDEN_CHANNEL,  1, 16);
        assertToken(4, TraciLexer.CSG_SHAPE, "intersection", Token.DEFAULT_CHANNEL, 1, 17);
        assertToken(5, TraciLexer.EOF,       null,           Token.DEFAULT_CHANNEL, 1, 29);
    }
}
