package traci.main;

import java.io.FileReader;
import java.io.IOException;
import java.io.PushbackReader;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.Map.Entry;

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
import traci.model.material.pigment.Checker;
import traci.model.shape.Shape;
import traci.model.shape.csg.Union;
import traci.model.shape.primitive.Plane;
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
        
        System.out.println("> Load time of `" + filename + "': " + (stop - start)
                + " ms.");
    }
    
    public static void main(final String[] args) throws IOException, SchemeException
    {
        final int width = 1600;
        final int height = 1200;
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
        System.out.println("> Startup time: " + (stop - start) + " ms.");
        
        loadSchemeFile("scheme/primitive.scm", interpreter);
        loadSchemeFile("scheme/helper.scm", interpreter);
        loadSchemeFile("scheme/user.scm", interpreter);
        loadSchemeFile("scheme/java.scm", interpreter);
        
        loadSchemeFile("scenes/lego/constants.scm", interpreter);
        loadSchemeFile("scenes/lego/basic-shapes.scm", interpreter);
        loadSchemeFile("scenes/lego/lego-bricks.scm", interpreter);
        loadSchemeFile("scenes/lego/lego-technic-div.scm", interpreter);
        //loadSchemeFile("scenes/airplane.scm", interpreter);
        
        start = System.currentTimeMillis();
        //Value val = interpreter.eval("(->java (lego-plate 2 4))");
        Value val = interpreter.eval("(->java (legopiece-technic-pin))");
        //Value val = interpreter.eval("(->java (lego-peg))");
        stop = System.currentTimeMillis();
        System.out.println("> Creation of scene: " + (stop - start) + " ms.");
        
        final Shape shape = (Shape) ((JavaObject) val).get();
        
        Union union = new Union();
        union.add(shape);
        
        final Plane plane = new Plane();
        plane.material.setPigment(new Checker(Color.BLACK, Color.WHITE));
        plane.material.getPigment().translate(-.5, 0, -.5);
        plane.roty(15.23);
        //union.add(plane);
        
        final PointLight light = new PointLight(Vector.make(2, 15, 30), Color.WHITE.mul(30*55));
        final PointLight light2 = new PointLight(Vector.make(-10, 10, 10), Color.WHITE.mul(150));
        
        final Vector camLocation = Vector.make(-1, 10, 15);
        final Vector camLookAt = Vector.make(0, 0, 0);
        //final Vector camLookAt = Vector.make(.25, .65, .25);
        final Camera cam = new Camera(camLocation, camLookAt, Vector.make(.1, 1, .1));
        final Scene scene = new Scene(union, cam);
        scene.addLight(light);
        scene.addLight(light2);
        
        Renderer.renderScene(scene, new Settings(), multiDrawArea, 8);
        
//        ArrayList<Entry<String, Long>> list = new ArrayList<Entry<String, Long>>();
//        list.addAll(Vector.locMap.entrySet());
//        Collections.sort(list, new Comparator<Entry<String, Long>>()
//        {
//            @Override
//            public int compare(Entry<String, Long> o1, Entry<String, Long> o2)
//            {
//                return o1.getValue().compareTo(o2.getValue());
//            }
//        });
//        for (Entry<String, Long> entry : list)
//        {
//            System.out.println(entry.getValue() + ":\n" + entry.getKey() + "\n\n");
//        }
        
        start = 0;
     }
}
