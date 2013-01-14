package traci.main;

import traci.gui.DynamicJPanelDrawArea;
import traci.gui.MainWindow;
import traci.gui.MultiDrawArea;
import traci.gui.PngDrawArea;
import traci.lang.interpreter.Interpreter;
import traci.lang.parser.ParserRunner;
import traci.lang.preprocessor.PreprocessorRunner;
import traci.model.Scene;
import traci.render.Renderer;
import traci.util.Log;

public class Main
{
    private static Result run(final String[] argv)
    {
        final Options options = new Options();
        final Settings settings = options.parse(argv);
        Result result;

        final PreprocessorRunner pp = new PreprocessorRunner(settings);
        result = pp.run();
        if (result != Result.SUCCESS)
        {
            return result;
        }
        final String code = pp.getProcessedCode();

        final ParserRunner parser = new ParserRunner(settings, code);
        result = parser.run();
        if (result != Result.SUCCESS)
        {
            return result;
        }
        final Interpreter interpreter = parser.getInterpreter();

        result = interpreter.run();
        if (result != Result.SUCCESS)
        {
            return result;
        }
        final Scene scene = interpreter.getScene();

        final MultiDrawArea drawAreas = new MultiDrawArea(settings.width, settings.height);

        if (settings.display)
        {
            final DynamicJPanelDrawArea visibleArea = new DynamicJPanelDrawArea(settings.width, settings.height);
            drawAreas.add(visibleArea);
            new MainWindow(visibleArea).setVisible(true);
        }

        if (settings.outputFilename != null)
        {
            drawAreas.add(new PngDrawArea(settings.width, settings.height, settings.outputFilename));
        }

        result = Renderer.renderScene(scene, settings, drawAreas);
        if (result != Result.SUCCESS)
        {
            return result;
        }

        return Result.SUCCESS;
    }

    public static void main(final String[] argv)
    {
        final Result result = run(argv);

        if (result == Result.SUCCESS)
        {
            Log.INFO("Exiting");
        }
        else
        {
            Log.ERROR("Aborting due to errors");
        }
    }
}
