package traci.main;

import java.io.FileReader;
import java.io.IOException;
import java.io.PushbackReader;

import sisc.data.Value;
import sisc.interpreter.AppContext;
import sisc.interpreter.Context;
import sisc.interpreter.Interpreter;
import sisc.interpreter.SchemeException;
import sisc.modules.s2j.JavaObject;
import traci.gui.DynamicJPanelDrawArea;
import traci.gui.MainWindow;
import traci.gui.MultiDrawArea;
import traci.gui.PngDrawArea;
import traci.math.Vector;
import traci.model.Camera;
import traci.model.Scene;
import traci.model.light.PointLight;
import traci.model.material.Color;
import traci.model.shape.Shape;
import traci.render.Renderer;
import traci.render.Settings;

public class TestSISC
{
    private static void loadSchemeFile(final String filename,
            final Interpreter interpreter) throws IOException, SchemeException
    {
        final double start = System.currentTimeMillis();
        final FileReader fileReader = new FileReader(filename);
        final PushbackReader pbReader = new PushbackReader(fileReader);
        interpreter.evalInput(pbReader);
        final double stop = System.currentTimeMillis();
        
        System.out.println("Load time of `" + filename + "': " + (stop - start)
                + " ms.");
    }
    
    public static void main(final String[] args) throws IOException, SchemeException
    {
        final int width = 1024;
        final int height = 768;
        final String filename = "out.png";
        
        final DynamicJPanelDrawArea visibleDrawArea = new DynamicJPanelDrawArea(width, height);
        final MainWindow window = new MainWindow(visibleDrawArea);
        window.setVisible(true);
        
        final PngDrawArea pngDrawArea = new PngDrawArea(width, height, filename);
        final MultiDrawArea multiDrawArea = new MultiDrawArea(pngDrawArea);
        multiDrawArea.add(visibleDrawArea);

        double start = System.currentTimeMillis();
        final AppContext appContext = new AppContext();
        appContext.addDefaultHeap();
        final Interpreter interpreter = Context.enter(appContext);
        double stop = System.currentTimeMillis();
        System.out.println("Startup time: " + (stop - start) + " ms.");
        
        loadSchemeFile("scheme/main.scm", interpreter);
        loadSchemeFile("scheme/java.scm", interpreter);
        loadSchemeFile("scenes/test.traci", interpreter);
        //loadSchemeFile("scenes/lego.traci", interpreter);
        
        start = System.currentTimeMillis();
        Value val = interpreter.eval("(->java (hej))");
        stop = System.currentTimeMillis();
        System.out.println("Creation of scene: " + (stop - start) + " ms.");
        
        final PointLight light = new PointLight(Vector.make(2, 5, -30), Color.WHITE.mul(30*30));
        final PointLight light2 = new PointLight(Vector.make(-10, 10, 10), Color.RED.mul(50));
        
        final Vector camLocation = Vector.make(-5, 0, -15);
        final Vector camLookAt = Vector.make(0, 0, 0);
        final Camera cam = new Camera(camLocation, camLookAt, null);
        final Scene scene = new Scene((Shape) ((JavaObject) val).get(), cam);
        scene.addLight(light);
        scene.addLight(light2);
        
        Renderer.renderScene(scene, new Settings(), multiDrawArea, 3);
        
        start = 0;
     }
}
