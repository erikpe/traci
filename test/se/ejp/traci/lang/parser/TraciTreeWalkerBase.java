package se.ejp.traci.lang.parser;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;

import java.io.IOException;

import org.antlr.runtime.ANTLRFileStream;
import org.antlr.runtime.ANTLRStringStream;
import org.antlr.runtime.CharStream;
import org.antlr.runtime.CommonTokenStream;
import org.antlr.runtime.RecognitionException;
import org.antlr.runtime.tree.CommonTree;
import org.antlr.runtime.tree.CommonTreeNodeStream;

import se.ejp.traci.lang.interpreter.node.BlockNode;
import se.ejp.traci.lang.preprocessor.PreprocessorRunner;
import se.ejp.traci.main.Result;
import se.ejp.traci.main.options.MockSettings;

public class TraciTreeWalkerBase
{
    protected BlockNode rootNode = null;

    protected void run(final CharStream input) throws RecognitionException
    {
        final TraciLexer lexer = new TraciLexer(input);
        final TraciParser parser = new TraciParser(new CommonTokenStream(lexer));
        final CommonTree parseTree = (CommonTree) parser.scene().getTree();

        assertFalse(lexer.getLexerErrors().iterator().hasNext());
        assertFalse(parser.getParseErrors().iterator().hasNext());

        final TraciTreeWalker walker = new TraciTreeWalker(new CommonTreeNodeStream(parseTree));
        rootNode = walker.block();
    }

    protected void runTreeWalker(final String code) throws RecognitionException
    {
        run(new ANTLRStringStream(code));
    }

    protected void runTreeWalkerFile(final String filename) throws RecognitionException, IOException
    {
        run(new ANTLRFileStream(filename));
    }

    protected void runTreeWalkerPreprocessedFile(final String filename) throws RecognitionException
    {
        final MockSettings settings = new MockSettings();
        settings.setInputFilename(filename);
        final PreprocessorRunner pp = new PreprocessorRunner(settings);
        final Result result = pp.run();
        assertEquals(Result.SUCCESS, result);
        runTreeWalker(pp.getProcessedCode());
    }
}
