package traci.render;

import traci.gui.DrawArea;
import traci.math.Vector;
import traci.model.Camera;
import traci.model.Scene;
import traci.model.light.PointLight;
import traci.model.material.Color;
import traci.model.material.Finish;
import traci.model.material.Pigment;

public class Renderer
{
    public static void drawScene(final Scene scene, final DrawArea area)
    {
        final Camera cam = scene.camera;
        
        final double fovy = cam.fov * (((double) area.height) / area.width);
        final double xx = 2.0 * Math.tan(cam.fov / 2.0);
        final double yy = 2.0 * Math.tan(fovy / 2.0);
        
        final int aa = 1;
        
        for (int y = 0; y < area.height; ++y)
        {
            for (int x = 0; x < area.width; ++x)
            {
                Color color = Color.BLACK;
                
                for (int aay = 0; aay < aa; ++aay)
                {
                    for (int aax = 0; aax < aa; ++aax)
                    {
                        double lookX = ((double) x + aax*(1.0/aa)) / (area.width-1); // [0.0 .. 1.0]
                        double lookY = ((double) y + aay*(1.0/aa)) / (area.height-1); // [0.0 .. 1.0]
                        
                        lookX = (lookX - 0.5); // [-0.5 .. 0.5]
                        lookY = (0.5 - lookY); // [0.5 .. -0.5] 
                        
                        lookX = lookX * xx;
                        lookY = lookY * yy;
                        
                        final Vector dir = Vector.make(lookX, lookY, -1.0);
                        final Vector camDir = cam.mat.mul(dir).normalize();
                        
                        color = color.add(raytrace(scene, 5, cam.location, camDir));
                    }
                }
                
                color = color.div(aa*aa);
                
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
        
        final Pigment pigment = hit.obj.material.getPigment();
        final Finish finish = hit.obj.material.getFinish();
        
        /**
         * Ambient light
         */
        final Color colorAmb = pigment.getColor().mul(PointLight.ambient);
        Color colorTotal = colorAmb;
        
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
            final Color lightAtPoint = light.color.mul(distCoeff);
            
            /**
             * Diffuse light
             */
            double c = dirToLight.dot(normal);
            c = Math.max(c, 0.0);
            final Color colorDiff = pigment.getColor().mul(lightAtPoint.mul(c * finish.getDiffCoeff()));
            
            colorTotal = colorTotal.add(colorDiff);
            
            /**
             * Specular light
             */
            final Vector lightRef = normal.mul(normal.mul(2).dot(dirToLight)).sub(dirToLight);
            final double cosTheta = -lightRef.dot(dir);
            
            if (cosTheta > 0)
            {
                final double shininess = finish.getShininess();
                final double specCoeff = finish.getSpecCoeff();
                
                final Color colorSpec = light.color.mul(Math.pow(cosTheta, shininess) * specCoeff * distCoeff);
                colorTotal = colorTotal.add(colorSpec);
            }
        }
        
        /**
         * Reflection
         */
        final Vector rr = dir.sub(normal.mul(dir.mul(2).dot(normal)));
        final Color colorReflect = raytrace(scene, depth-1, hitPoint, rr.normalize());
        colorTotal = colorTotal.add(colorReflect.mul(finish.getReflectivness()));
        
        return colorTotal;
    }
}
