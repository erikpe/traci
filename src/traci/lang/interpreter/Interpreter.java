package traci.lang.interpreter;

import traci.lang.interpreter.Entities.Entity;
import traci.lang.interpreter.exceptions.FunctionReturnException;
import traci.lang.interpreter.exceptions.InterpreterRuntimeException;
import traci.lang.interpreter.node.BlockNode;
import traci.main.Result;
import traci.main.options.Settings;
import traci.math.Vector;
import traci.model.Camera;
import traci.model.Scene;
import traci.model.shape.Shape;
import traci.model.shape.csg.Union;
import traci.util.Log;
import traci.util.Utilities;

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

        final Union rootUnion = new Union();
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

        Shape optimizedRoot = rootUnion.optimize();
        if (optimizedRoot == null)
        {
            optimizedRoot = new Union();
        }
        scene.setRootShape(optimizedRoot);

        final long stop = System.currentTimeMillis();
        Log.INFO("Scene constructed in " + Utilities.millisecondsToString(stop - start));

//        final PointLight light = new PointLight(Vector.make(2, 15, 30), Color.WHITE.mul(30*50));
//        final PointLight light2 = new PointLight(Vector.make(-10, 10, 10), Color.WHITE.mul(150));

        final Vector camLocation = Vector.make(-15, 30, 40);
        final Vector camLookAt = Vector.make(8, 2, 0);
        final Camera cam = new Camera(camLocation, camLookAt, Vector.UNIT_Y, settings);
        scene.setCamera(cam);
//        scene = new Scene(optimizedRoot, cam);
//        scene.addPointLight(light);
//        scene.addPointLight(light2);

        return Result.SUCCESS;
    }
}
