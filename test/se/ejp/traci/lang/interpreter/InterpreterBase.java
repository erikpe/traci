package se.ejp.traci.lang.interpreter;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.io.IOException;

import org.antlr.runtime.ANTLRFileStream;
import org.antlr.runtime.ANTLRStringStream;
import org.antlr.runtime.CharStream;
import org.antlr.runtime.CommonTokenStream;
import org.antlr.runtime.RecognitionException;
import org.antlr.runtime.tree.CommonTree;
import org.antlr.runtime.tree.CommonTreeNodeStream;

import se.ejp.traci.lang.interpreter.Entities.Entity;
import se.ejp.traci.lang.interpreter.exceptions.FunctionReturnException;
import se.ejp.traci.lang.interpreter.exceptions.InterpreterRuntimeException;
import se.ejp.traci.lang.interpreter.node.BlockNode;
import se.ejp.traci.lang.parser.TraciLexer;
import se.ejp.traci.lang.parser.TraciParser;
import se.ejp.traci.lang.parser.TraciTreeWalker;
import se.ejp.traci.lang.preprocessor.PreprocessorRunner;
import se.ejp.traci.main.Result;
import se.ejp.traci.main.options.MockSettings;
import se.ejp.traci.model.Scene;
import se.ejp.traci.model.shape.csg.Union;

public class InterpreterBase
{
    protected Scene scene = null;
    protected TraciValue value = null;

    private void run(final CharStream input) throws RecognitionException, InterpreterRuntimeException
    {
        final TraciLexer lexer = new TraciLexer(input);
        final TraciParser parser = new TraciParser(new CommonTokenStream(lexer));
        final CommonTree parseTree = (CommonTree) parser.scene().getTree();

        assertTrue(lexer.getLexerErrors().isEmpty());
        assertTrue(parser.getParseErrors().isEmpty());

        final TraciTreeWalker walker = new TraciTreeWalker(new CommonTreeNodeStream(parseTree));
        final BlockNode rootNode = walker.block();

        final Union rootUnion = new Union();
        final Entity entity = Entities.makeEntity(rootUnion);
        scene = new Scene();
        value = null;

        try
        {
            rootNode.eval(Context.newRootContext(scene, entity));
        }
        catch (final FunctionReturnException e)
        {
            assertNotNull(e.value);
            value = e.value;
        }
    }

    protected void runInterpreter(final String code) throws RecognitionException, InterpreterRuntimeException
    {
        run(new ANTLRStringStream(code));
    }

    protected void runInterpreterFile(final String filename) throws RecognitionException, IOException,
            InterpreterRuntimeException
    {
        run(new ANTLRFileStream(filename));
    }

    protected void runParserPreprocessedFile(final String filename) throws RecognitionException,
            InterpreterRuntimeException
    {
        final MockSettings settings = new MockSettings();
        settings.setInputFilename(filename);
        final PreprocessorRunner pp = new PreprocessorRunner(settings);
        final Result result = pp.run();
        assertEquals(Result.SUCCESS, result);
        runInterpreter(pp.getProcessedCode());
    }
}
