package traci.main;

import traci.gui.DynamicJPanelDrawArea;
import traci.gui.MainWindow;
import traci.gui.MultiDrawArea;
import traci.gui.PngDrawArea;
import traci.lang.interpreter.Interpreter;
import traci.lang.preprocessor.TraciPreprocessor;
import traci.model.Scene;
import traci.render.Renderer;

public class Main
{
    public static void main(final String[] argv)
    {
        final Options options = new Options();
        final Settings settings = options.parse(argv);

        final TraciPreprocessor pp = new TraciPreprocessor(settings);
        final String code = pp.run();

        final Interpreter interpreter = new Interpreter(settings, code);
        final Scene scene = interpreter.run();

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

        Renderer.renderScene(scene, settings, drawAreas);
    }
}
