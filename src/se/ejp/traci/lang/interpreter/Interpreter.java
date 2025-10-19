package se.ejp.traci.lang.interpreter;

import se.ejp.traci.lang.interpreter.exceptions.FunctionReturnException;
import se.ejp.traci.lang.interpreter.exceptions.InterpreterInternalException;
import se.ejp.traci.lang.interpreter.exceptions.InterpreterRuntimeException;
import se.ejp.traci.lang.interpreter.node.BlockNode;
import se.ejp.traci.main.Result;
import se.ejp.traci.main.options.Settings;
import se.ejp.traci.model.Scene;
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

        final Context rootContext = Context.newRootContext(scene);
        try
        {
            blockNode.eval(rootContext);
        }
        catch (final InterpreterRuntimeException e)
        {
            Log.ERROR(e.fullMsg());
            return Result.RUNTIME_ERROR;
        }
        catch (final InterpreterInternalException e)
        {
            Log.ERROR(e.fullMsg());
            return Result.INTERNAL_ERROR;
        }
        catch (final FunctionReturnException e)
        {
            // Ignore
        }

        // !!! TODO: before using Csg.optimize(), determine what effects it has on bounding boxes.
        // Shape optimizedRoot = rootUnion.optimize();
        // if (optimizedRoot == null)
        // {
        //     optimizedRoot = Union.make();
        // }
        // scene.setRootShape(optimizedRoot);

        if (scene.camera == null)
        {
            Log.INFO("No camera object specified");
        }
        else
        {
            scene.camera.initialize(settings);
        }

        final long stop = System.currentTimeMillis();
        Log.INFO("Scene constructed in " + Utilities.millisecondsToString(stop - start));

        return Result.SUCCESS;
    }
}
