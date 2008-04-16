package traci.render;

import traci.gui.DrawArea;
import traci.math.Vector;
import traci.model.Camera;
import traci.model.Scene;
import traci.model.light.PointLight;
import traci.model.texture.Color;
import traci.model.texture.Finish;
import traci.model.texture.Pigment;

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
                
                final Vector dir = Vector.make(lookX, lookY, -1.0);
                final Vector camDir = cam.mat.mul(dir).normalize();
                
                final Color color = raytrace(scene, 4, cam.location, camDir);
                area.draw(x, y, color);
            }
        }
        
        area.finish();
    }
    
    private static Color raytrace(final Scene scene, final int depth,
                                  final Vector p, final Vector dir)
    {
        if (depth <= 0)
        {
            return Color.BLACK;
        }
        
        final Ray ray = scene.shape.shootRay(p, dir);
        
        if (ray == null)
        {
            return Color.BLACK;
        }
        
        final Point hit = ray.get(0).p0;
        final Vector normal = hit.obj.transform.normal(hit.normal).normalize();
        final Vector hitPoint = p.add(dir.mul(hit.dist));
        
        final Pigment pigment = hit.obj.texture.getPigment();
        final Finish finish = hit.obj.texture.getFinish();
        
        /**
         * Ambient light
         */
        final Color cAmb = finish.getCAmb();
        Color totalLight = cAmb;
        
        for (final PointLight light : scene.lights)
        {
            final Vector toLight = light.location.sub(hitPoint);
            final Vector dirToLight = toLight.normalize();
            
            final Ray lightRay = scene.shape.shootRay(hitPoint, dirToLight);
            
            if (lightRay != null)
            {
                continue;
            }
            
            final double distToLight = toLight.length();
            final double distCoeff = 1.0 / (distToLight * distToLight);
            
            /**
             * Diffuse light
             */
            double c = dirToLight.dot(normal);
            c = (c < 0.0 ? 0.0 : c);
            final Color cDiff = light.color.mul(c * finish.getDiffCoeff() * distCoeff);
            
            totalLight = totalLight.add(cDiff);
            
            /**
             * Specular light
             */
            final Vector r = normal.mul(normal.mul(2).dot(dirToLight)).sub(dirToLight);
            final double cosTheta = -r.dot(dir);
            
            if (cosTheta > 0)
            {
                final double shininess = finish.getShininess();
                final double specCoeff = finish.getSpecCoeff();
                
                final Color cSpec = light.color.mul(Math.pow(cosTheta, shininess) * specCoeff * distCoeff);
                totalLight = totalLight.add(cSpec);
            }
        }
        
        return pigment.getColor().mul(totalLight);
    }
}
