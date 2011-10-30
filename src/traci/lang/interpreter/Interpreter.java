package traci.lang.interpreter;

import java.io.IOException;

import org.antlr.runtime.ANTLRFileStream;
import org.antlr.runtime.CommonTokenStream;
import org.antlr.runtime.RecognitionException;
import org.antlr.runtime.tree.CommonTree;
import org.antlr.runtime.tree.CommonTreeNodeStream;

import traci.lang.interpreter.Entities.Entity;
import traci.lang.interpreter.node.BlockNode;
import traci.lang.parser.TraciLexer;
import traci.lang.parser.TraciParser;
import traci.lang.parser.TraciTreeWalker;
import traci.math.Vector;
import traci.model.Camera;
import traci.model.Scene;
import traci.model.light.PointLight;
import traci.model.material.Color;
import traci.model.shape.csg.Union;
import traci.util.Utilities;
import traci.util.Log;

public class Interpreter
{
    private final String filename;

    public Interpreter(final String filename)
    {
        this.filename = filename;
    }

    public Scene run()
    {
        Log.INFO("Parsing input file: " + filename);
        long start = System.currentTimeMillis();
        ANTLRFileStream input = null;
        try
        {
            input = new ANTLRFileStream(filename);
        }
        catch (final IOException e)
        {
            Log.ERROR("Unable to open input file: " + filename + ":");
            Log.ERROR(e.getMessage());
            System.exit(-1);
        }
        final TraciLexer lexer = new TraciLexer(input);
        final CommonTokenStream tokens = new CommonTokenStream(lexer);
        final TraciParser parser = new TraciParser(tokens);
        CommonTree tree = null;
        try
        {
            tree = (CommonTree) parser.scene().getTree();
        }
        catch (final RecognitionException e)
        {
            Log.ERROR("Parse error: " + e.getMessage());
            System.exit(-1);
        }
        long stop = System.currentTimeMillis();

        Log.INFO("Parsing finished in " + Utilities.millisecondsToString(stop - start));
        Log.INFO("Building interpreter");

        start = System.currentTimeMillis();
        final CommonTreeNodeStream nodes = new CommonTreeNodeStream(tree);
        final TraciTreeWalker walker = new TraciTreeWalker(nodes);
        BlockNode bn = null;
        try
        {
            bn = walker.block();
        }
        catch (final RecognitionException e)
        {
            Log.ERROR("Parse error: " + e.getMessage());
            System.exit(-1);
        }
        stop = System.currentTimeMillis();

        Log.INFO("Interpreter built in " + Utilities.millisecondsToString(stop - start));
        Log.INFO("Constructing scene");

        start = System.currentTimeMillis();
        final Union rootUnion = new Union();
        final Entity entity = Entities.makeEntity(rootUnion);
        try
        {
            bn.eval(Context.newRootContext(entity));
        }
        catch (final FunctionReturnException e)
        {
            // Ignore
        }
        stop = System.currentTimeMillis();

        Log.INFO("Scene constructed in " + Utilities.millisecondsToString(stop - start));

        final PointLight light = new PointLight(Vector.make(2, 15, 30), Color.WHITE.mul(30*50));
        final PointLight light2 = new PointLight(Vector.make(-10, 10, 10), Color.WHITE.mul(150));

        final Vector camLocation = Vector.make(-10, 15, 15);
        final Vector camLookAt = Vector.make(8, 2, 0);
        final Camera cam = new Camera(camLocation, camLookAt, Vector.UNIT_Y);
        final Scene scene = new Scene(rootUnion, cam);
        scene.addLight(light);
        scene.addLight(light2);

        return scene;
    }
}
