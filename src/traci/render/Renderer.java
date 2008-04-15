package traci.render;

import traci.gui.DrawArea;
import traci.math.Vector;
import traci.model.Camera;
import traci.model.Scene;
import traci.model.csg.Primitive;
import traci.model.light.PointLight;
import traci.model.texture.Color;

public class Renderer
{
    public static void drawScene(final Scene scene, final DrawArea area)
    {
        final Camera cam = scene.camera;
        
        final double fovy = cam.fov * (((double) area.height) / area.width);
        final double xx = 2.0 * Math.tan(cam.fov / 2.0);
        final double yy = 2.0 * Math.tan(fovy / 2.0);
        
        for (int y = 0; y < area.height; ++y)
        {
            for (int x = 0; x < area.width; ++x)
            {
                double lookX = ((double) x) / (area.width-1); // [0.0 .. 1.0]
                double lookY = ((double) y) / (area.height-1); // [0.0 .. 1.0]
                
                lookX = (lookX - 0.5); // [-0.5 .. 0.5]
                lookY = (0.5 - lookY); // [0.5 .. -0.5] 
                
                lookX = lookX * xx;
                lookY = lookY * yy;
                
                final Vector point = Vector.ORIGO;
                final Vector dir = Vector.make(lookX, lookY, -1.0);
                final Vector lookAt = point.add(dir);
                
                final Vector camPoint = cam.location;
                final Vector camDir = cam.mat.mul(dir);
                final Vector camLookAt = camPoint.add(camDir);
                
                final Color color = raytrace(scene, 4, camPoint, camLookAt);
                area.draw(x, y, color);
            }
        }
        
        area.finish();
    }
    
    private static Color raytrace(final Scene scene, final int depth,
                                  final Vector p, final Vector lookAt)
    {
        if (depth <= 0)
        {
            return Color.BLACK;
        }
        
        final Intervals ivals = scene.shape.shootRay(p, lookAt);
        
        if (ivals == null || ivals.isEmpty())
        {
            return Color.BLACK;
        }
        
        final Point hit = ivals.get(0).p0;
        final Primitive obj = hit.obj;
        
        final Vector normal = hit.normal.normalize();
        final Vector hitP = p.add(lookAt.sub(p).normalize().mul(hit.dist));
        
        /**
         * Ambient light
         */
        final Color cAmb = Color.make(0.1, 0.1, 0.1);
        Color totalLight = cAmb;
        
        for (final PointLight light : scene.lights)
        {
            final Vector toLight = light.location.sub(hitP).normalize();
            
            /**
             * Diffuse light
             */
            double c = toLight.dot(normal);
            c = (c < 0.0 ? 0.0 : c);
            final Color cDiff = light.color.mul(c * obj.diffCoeff());
            
            totalLight = totalLight.add(cDiff);
            
            /**
             * Specular light
             */
            final Vector toCamera = p.sub(hitP).normalize();
            final Vector r = normal.mul(normal.mul(2).dot(toLight)).sub(toLight);
            final double cosTheta = r.dot(toCamera);
            
            if (cosTheta > 0)
            {
                final double shininess = obj.shininess();
                final double specCoeff = obj.specCoeff();
                
                final Color cSpec = light.color.mul(Math.pow(cosTheta, shininess) * specCoeff);
                totalLight = totalLight.add(cSpec);
            }
        }
        
        return obj.getColor().mul(totalLight);
    }
}
