package traci.lang;

import traci.gui.DynamicJPanelDrawArea;
import traci.gui.MainWindow;
import traci.gui.MultiDrawArea;
import traci.gui.PngDrawArea;
import traci.lang.interpreter.Interpreter;
import traci.model.Scene;
import traci.render.Renderer;
import traci.render.Settings;

public class TestParser
{
    public static void main(final String[] args) throws Exception
    {
        final int width = 2560;
        final int height = 1440;
        final String filename = "out.png";

        final DynamicJPanelDrawArea visibleDrawArea = new DynamicJPanelDrawArea(width, height);
        final MainWindow window = new MainWindow(visibleDrawArea);
        window.setVisible(true);

        final PngDrawArea pngDrawArea = new PngDrawArea(width, height, filename);
        final MultiDrawArea multiDrawArea = new MultiDrawArea(pngDrawArea);
        multiDrawArea.add(visibleDrawArea);

        final Scene scene = new Interpreter("src/traci/lang/input.txt").run();
        Renderer.renderScene(scene, new Settings(), multiDrawArea, 8);
    }
}
