package traci.main;

import traci.gui.DynamicJPanelDrawArea;
import traci.gui.MainWindow;
import traci.gui.MultiDrawArea;
import traci.gui.PngDrawArea;
import traci.math.PolynomSolver;
import traci.math.Vector;
import traci.model.Camera;
import traci.model.Scene;
import traci.model.light.PointLight;
import traci.model.material.Color;
import traci.model.material.pigment.Checker;
import traci.model.shape.csg.Union;
import traci.model.shape.primitive.Cylinder;
import traci.model.shape.primitive.Plane;
import traci.model.shape.primitive.Sphere;
import traci.model.shape.primitive.Torus;
import traci.render.Renderer;
import traci.render.Settings;

public class Main
{
    public static void main(final String[] args)
    {
        //Transformation.camera(Vector.ORIGO, Vector.make(1, 2, 3), Vector.UNIT_Y);
        
        final int width = 1024;
        final int height = 768;
        final String filename = "out.png";
        
        final DynamicJPanelDrawArea visibleDrawArea = new DynamicJPanelDrawArea(width, height);
        final MainWindow window = new MainWindow(visibleDrawArea);
        window.setVisible(true);
        
        final PngDrawArea pngDrawArea = new PngDrawArea(width, height, filename);
        final MultiDrawArea multiDrawArea = new MultiDrawArea(pngDrawArea);
        multiDrawArea.add(visibleDrawArea);
        
        final Plane plane = new Plane();
        plane.translatey(-1);
        plane.material.setPigment(new Checker(Color.BLACK, Color.WHITE));
        plane.material.getPigment().translate(-.5, 0, -.5);
        plane.material.getPigment().scale(5);
        
        final Sphere origoS = new Sphere();
        origoS.material.setColor(Color.WHITE);
        
        final Sphere left = new Sphere();
        left.translatex(-5);
        left.material.setColor(Color.CYAN);
        
        final Sphere right = new Sphere();
        right.translatex(5);
        right.material.setColor(Color.YELLOW);
        
        final Sphere near = new Sphere();
        near.translatez(5);
        near.material.setColor(Color.GREEN);
        
        final Sphere far = new Sphere();
        far.translatez(-5);
        far.material.setColor(Color.RED);
        
        final Cylinder cyl = new Cylinder();
        cyl.translatex(-2.5);
        
        final Cylinder cyl2 = new Cylinder(.5, Vector.make(-5, 1, 0), Vector.make(5, 1, 0));
        
        final Union spheres = new Union();
        spheres.add(origoS);
        spheres.add(left);
        spheres.add(right);
        spheres.add(near);
        spheres.add(far);
        spheres.rotz(Math.PI / 16);
        
        double[] res = PolynomSolver.solveQuartic(new double[] { 1, 2, -3, 4, 5 });
        
        final Sphere sphere = new Sphere();
        final Torus torus = new Torus(.1); torus.rotx(Math.PI / 2);
        final Cylinder xcyl = new Cylinder(); xcyl.translatey(-.5); xcyl.scale(.05, 7, .05); xcyl.rotz(Math.PI / 2);
        final Cylinder ycyl = new Cylinder(); ycyl.translatey(-.5); ycyl.scale(.05, 7, .05);
        final Cylinder zcyl = new Cylinder(); zcyl.translatey(-.5); zcyl.scale(.05, 7, .05); zcyl.rotx(Math.PI / 2);
        
        final Union union = new Union();
        union.add(plane);
        union.add(torus);
        union.add(xcyl);
        union.add(ycyl);
        union.add(zcyl);
        
        //final Vector camLocation = Vector.make(100, 200, 300);
        //final Vector camLookAt = Vector.make(101, 203, 309);
        final Vector camLocation = Vector.make(-2, 2, 10);
        final Vector camLookAt = Vector.make(0, 0, 0);
        final Camera cam = new Camera(camLocation, camLookAt, Vector.make(0, 1, 0));
        
        final Scene scene = new Scene(union, cam);
        final PointLight light = new PointLight(Vector.make(2, 5, 30), Color.WHITE.mul(30*30));
        final PointLight light2 = new PointLight(Vector.make(-10, 10, 10), Color.RED.mul(50));
        
        scene.addLight(light);
        scene.addLight(light2);
        
        Renderer.renderScene(scene, new Settings(), multiDrawArea, 8);
    }
}
