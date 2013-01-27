package traci.lang.interpreter;

import traci.lang.interpreter.Entities.Entity;
import traci.lang.interpreter.exceptions.FunctionReturnException;
import traci.lang.interpreter.exceptions.InterpreterRuntimeException;
import traci.lang.interpreter.node.BlockNode;
import traci.main.Result;
import traci.main.options.Settings;
import traci.math.Transformations;
import traci.math.Vector;
import traci.model.Camera;
import traci.model.Scene;
import traci.model.material.Color;
import traci.model.material.pigment.Checker;
import traci.model.material.pigment.NonUniform;
import traci.model.shape.Shape;
import traci.model.shape.csg.Union;
import traci.model.shape.primitive.Plane;
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

        final Plane plane = new Plane();
        plane.setPigment(new NonUniform(Transformations.identity(), new Checker(Color.WHITE, Color.BLACK)));
        //rootUnion.add(plane);

        Shape optimizedRoot = rootUnion.optimize();
        if (optimizedRoot == null)
        {
            optimizedRoot = new Union();
        }
        scene.setRootShape(optimizedRoot);

        final long stop = System.currentTimeMillis();
        Log.INFO("Scene constructed in " + Utilities.millisecondsToString(stop - start));

//        final Vector camLocation = Vector.make(-5, 10, 10);
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
