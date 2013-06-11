package se.ejp.traci.main;

import se.ejp.traci.gui.DynamicJPanelDrawArea;
import se.ejp.traci.gui.MainWindow;
import se.ejp.traci.gui.MultiDrawArea;
import se.ejp.traci.gui.PngDrawArea;
import se.ejp.traci.lang.interpreter.Interpreter;
import se.ejp.traci.lang.parser.ParserRunner;
import se.ejp.traci.lang.preprocessor.PreprocessorRunner;
import se.ejp.traci.main.options.Options;
import se.ejp.traci.main.options.Settings;
import se.ejp.traci.model.Scene;
import se.ejp.traci.render.Renderer;
import se.ejp.traci.util.Log;

public class Main
{
    private static Result run(final String[] argv)
    {
        final Options options = new Options();
        Result result = options.parse(argv);
        if (result != Result.SUCCESS)
        {
            return result;
        }
        final Settings settings = options.getSettings();

        PreprocessorRunner pp = new PreprocessorRunner(settings);
        result = pp.run();
        if (result != Result.SUCCESS)
        {
            return result;
        }
        final String code = pp.getProcessedCode();
        pp = null;

        ParserRunner parser = new ParserRunner(settings, code);
        result = parser.run();
        if (result != Result.SUCCESS)
        {
            return result;
        }
        Interpreter interpreter = parser.getInterpreter();
        parser = null;

        result = interpreter.run();
        if (result != Result.SUCCESS)
        {
            return result;
        }
        final Scene scene = interpreter.getScene();
        interpreter = null;

        final MultiDrawArea drawAreas = new MultiDrawArea(settings.getWidth(), settings.getHeight());

        if (settings.getDisplay())
        {
            final DynamicJPanelDrawArea visibleArea = new DynamicJPanelDrawArea(settings.getWidth(), settings.getHeight());
            drawAreas.add(visibleArea);
            new MainWindow(visibleArea, "Traci: " + settings.getInputFilename()).setVisible(true);
        }

        if (settings.getOutputFilename() != null)
        {
            drawAreas.add(new PngDrawArea(settings.getWidth(), settings.getHeight(), settings.getOutputFilename()));
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

        switch (result)
        {
        case SUCCESS:
            Log.INFO("Exiting");
            break;

        case ABORT:
            break;

        default:
            Log.ERROR("Aborting due to errors");
            System.exit(result.code);
            break;
        }
    }
}
