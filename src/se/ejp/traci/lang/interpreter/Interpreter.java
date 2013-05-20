package se.ejp.traci.lang.interpreter;

import se.ejp.traci.lang.interpreter.Entities.Entity;
import se.ejp.traci.lang.interpreter.exceptions.FunctionReturnException;
import se.ejp.traci.lang.interpreter.exceptions.InterpreterRuntimeException;
import se.ejp.traci.lang.interpreter.node.BlockNode;
import se.ejp.traci.main.Result;
import se.ejp.traci.main.options.Settings;
import se.ejp.traci.math.Vector;
import se.ejp.traci.model.Camera;
import se.ejp.traci.model.Scene;
import se.ejp.traci.model.shape.Shape;
import se.ejp.traci.model.shape.csg.Union;
import se.ejp.traci.util.Log;
import se.ejp.traci.util.Utilities;

public class Interpreter
{
    private final Settings settings;
    private final BlockNode blockNode;
    private final Scene scene;

    public Interpreter(final Settings settings, final BlockNode blockNode)
    {
        this.settings = settings;
        this.blockNode = blockNode;
        this.scene = new Scene();
    }

    public Scene getScene()
    {
        return scene;
    }

    public Result run()
    {
        Log.INFO("Constructing scene");
        final long start = System.currentTimeMillis();

        final Union rootUnion = Union.make();
        final Entity entity = Entities.makeEntity(rootUnion);

        try
        {
            blockNode.eval(Context.newRootContext(scene, entity));
        }
        catch (final InterpreterRuntimeException e)
        {
            final StringBuilder sb = new StringBuilder();

            if (e.includeLocation != null)
            {
                e.includeLocation.toString(sb);
                sb.append('\n');
            }

            sb.append("Runtime error: ").append(e.msg).append('\n');

            if (e.callStack != null)
            {
                e.callStack.format(sb, e.includeLocation.fileLocation);
            }

            Log.ERROR(sb.toString());

            return Result.RUNTIME_ERROR;
        }
        catch (final FunctionReturnException e)
        {
            // Ignore
        }

        //final Plane plane = Plane.make();
        //plane.setPigment(Checker.make(Color.WHITE, Color.BLACK));
        //rootUnion.add(plane);

        Shape optimizedRoot = rootUnion.optimize();
        if (optimizedRoot == null)
        {
            optimizedRoot = Union.make();
        }
        scene.setRootShape(optimizedRoot);

        final long stop = System.currentTimeMillis();
        Log.INFO("Scene constructed in " + Utilities.millisecondsToString(stop - start));

//        final Vector camLocation = Vector.make(-5, 4, 10);
//        final Vector camLookAt = Vector.make(0, 0, 0);
//        final Camera cam = new Camera(camLocation, camLookAt, Vector.UNIT_Y, settings);
        //Airplane
        Vector loc = Vector.make(-2.4*2, 6.7*2, 8.5*2);
        final Vector lookAt = Vector.make(7*2, 1.3*3, -1*2);
        loc = lookAt.add(loc.sub(lookAt).mul(2));
        final Vector camLocation = Vector.make(-2.4*2, 6.7*2, 8.5*2);
        final Vector camLookAt = Vector.make(7*2, 1.3*2, -1*2);
        final Camera cam = new Camera(loc, lookAt, Vector.UNIT_Y, settings);
        scene.setCamera(cam);

        return Result.SUCCESS;
    }
}
