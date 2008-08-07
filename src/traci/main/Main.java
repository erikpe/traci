package traci.main;

import traci.gui.MultiDrawArea;
import traci.gui.NullDrawArea;
import traci.gui.PngDrawArea;
import traci.math.Projection2D;
import traci.math.Vector;
import traci.model.Camera;
import traci.model.Scene;
import traci.model.light.PointLight;
import traci.model.material.Color;
import traci.model.material.pigment.Checker;
import traci.model.material.pigment.PngImage;
import traci.model.material.pigment.PngImage.RepeatPolicy;
import traci.model.shape.csg.Csg;
import traci.model.shape.csg.Difference;
import traci.model.shape.csg.Union;
import traci.model.shape.primitive.Box;
import traci.model.shape.primitive.Cylinder;
import traci.model.shape.primitive.Plane;
import traci.model.shape.primitive.Sphere;
import traci.render.Renderer;
import traci.render.Settings;

public class Main
{
    public static void main(final String[] args)
    {
        final int width = 1024;
        final int height = 768;
        final String filename = "out.png";
        
        //final DynamicJPanelDrawArea visibleDrawArea = new DynamicJPanelDrawArea(width, height);
        //final MainWindow window = new MainWindow(visibleDrawArea);
        //window.setVisible(true);
        
        final PngDrawArea pngDrawArea = new PngDrawArea(width, height, filename);
        final MultiDrawArea multiDrawArea = new MultiDrawArea(pngDrawArea);
        //multiDrawArea.add(visibleDrawArea);
        
        final Cylinder cyl0 = new Cylinder();
        cyl0.translateY(-0.5);
        cyl0.scale(0.1, 2, 0.5);
        
        final Cylinder cyl1 = new Cylinder();
        cyl1.translateY(-0.5);
        cyl1.scale(0.1, 2, 0.5);
        cyl1.rotateZ(Math.PI/2);
        
        final Cylinder cyl2 = new Cylinder();
        cyl2.translateY(-0.5);
        cyl2.scale(0.1, 2, 0.5);
        cyl2.rotateX(Math.PI/2);
        
        final Sphere s0 = new Sphere(); s0.scale(0.1, 0.1, 0.5); s0.translate(-1, 0, 0);
        final Sphere s1 = new Sphere(); s1.scale(0.1, 0.1, 0.5); s1.translate(1, 0, 0);
        final Sphere s2 = new Sphere(); s2.scale(0.1, 0.1, 0.5); s2.translate(0, -1, 0);
        final Sphere s3 = new Sphere(); s3.scale(0.1, 0.1, 0.5); s3.translate(0, 1, 0); s3.material.setColor(Color.CYAN);
        final Sphere s4 = new Sphere(); s4.scale(0.1, 0.5, 0.1); s4.translate(0, 0, -1);
        final Sphere s5 = new Sphere(); s5.scale(0.1, 0.5, 0.1); s5.translate(0, 0, 1);
        s5.material.setColor(Color.GREEN);
        
        final Box box = new Box();
        box.material.setColor(Color.BLUE);
        
        final Sphere sphere0 = new Sphere(); sphere0.scale(0.2); sphere0.translate(Vector.make(0, 0, 0));
        final Sphere sphere1 = new Sphere(); sphere1.scale(0.2); sphere1.translate(Vector.make(0, 0, 1));
        final Sphere sphere2 = new Sphere(); sphere2.scale(0.2); sphere2.translate(Vector.make(0, 1, 0));
        final Sphere sphere3 = new Sphere(); sphere3.scale(0.2); sphere3.translate(Vector.make(0, 1, 1));
        final Sphere sphere4 = new Sphere(); sphere4.scale(0.2); sphere4.translate(Vector.make(1, 0, 0));
        final Sphere sphere5 = new Sphere(); sphere5.scale(0.2); sphere5.translate(Vector.make(1, 0, 1));
        final Sphere sphere6 = new Sphere(); sphere6.scale(0.2); sphere6.translate(Vector.make(1, 1, 0));
        final Sphere sphere7 = new Sphere(); sphere7.scale(0.2); sphere7.translate(Vector.make(1, 1, 1));
        sphere7.material.setColor(Color.RED);
        
        final Csg csg = new Difference();
        csg.add(box);
        csg.add(sphere0); csg.add(sphere1);
        csg.add(sphere2); csg.add(sphere3);
        csg.add(sphere4); csg.add(sphere5);
        csg.add(sphere6); csg.add(sphere7);
        csg.translate(-0.5, -0.5, -0.5);
        
        final Plane plane = new Plane();
        plane.material.setPigment(new Checker(Color.BLACK, Color.WHITE));
        plane.material.getPigment().translate(-.5, 0, -.5);
        plane.material.getPigment().scale(1);
        plane.material.getPigment().rotateY(0.2);
        plane.translateY(-1);
        
        final Sphere cylinder = new Sphere();
        cylinder.translateY(0.5);
        cylinder.material.setPigment(new PngImage("resources"
                + System.getProperty("file.separator") + "lenna.png",
                RepeatPolicy.STRETCH, Projection2D.CYLINDER));
        cylinder.rotateY(1.7);
        cylinder.translateY(-0.3);
        cylinder.rotateX(0.05);
        cylinder.scale(0.3, 1.5, 0.3);
        cylinder.translateX(-1.5);
        cylinder.rotateZ(0.3);
        
        final Csg csg2 = new Union();
        csg2.add(csg);
        csg2.add(cyl0);
        csg2.add(cyl1);
        csg2.add(cyl2);
        csg2.add(s0);
        csg2.add(s1);
        csg2.add(s2);
        csg2.add(s3);
        csg2.add(s4);
        csg2.add(s5);
        csg2.rotateX(0.03);
        final Sphere origo = new Sphere();
        origo.scale(0.1);
        csg2.add(origo);
        csg2.add(plane);
        csg2.add(cylinder);
        
        final Vector camLocation = Vector.make(-1, 1.5, 5);
        final Vector camLookAt = Vector.make(0, -.5, 0);
        final Camera cam = new Camera(camLocation, camLookAt);
        cam.rotateZ(0.3);
        cam.rotateX(0.1);
        final Scene scene = new Scene(csg2, cam);
        final PointLight light = new PointLight(Vector.make(2, 5, 30), Color.WHITE.mul(30*30));
        final PointLight light2 = new PointLight(Vector.make(-10, 10, 10), Color.RED.mul(50));
        
        scene.addLight(light);
        scene.addLight(light2);
        
        Renderer.renderScene(scene, new Settings(), new NullDrawArea(width, height), 2);
    }
}
