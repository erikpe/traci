package traci.render;

import traci.math.Vector;
import traci.model.Scene;
import traci.model.light.PointLight;
import traci.model.material.Color;
import traci.model.material.Finish;
import traci.model.material.pigment.Pigment;

public class Raytrace
{
    protected static Color raytrace(final Scene scene, final int depth,
            final Vector p, final Vector dir)
    {
        final Ray ray = scene.shape.shootRay(p, dir);
        
        if (ray == null)
        {
            return Color.BLACK;
        }
        
        final Point hit = ray.get(0).p0;
        
        final Vector normal = hit.obj.transformation.normal(hit.normal).normalize();
        final Vector hitPoint = p.add(dir.mul(hit.dist));
        
        final Pigment pigment = hit.obj.material.getPigment();
        final Finish finish = hit.obj.material.getFinish();
        
        /**
         * Ambient light
         */
        final Color colorAmb = pigment.getColor(hitPoint).mul(PointLight.ambient);
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
            final double c = Math.max(dirToLight.dot(normal), 0.0);
            final Color colorDiff = pigment.getColor(hitPoint).mul(
                    lightAtPoint.mul(c * finish.getDiffCoeff()));
            
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
                
                final Color colorSpec = light.color.mul(Math.pow(cosTheta,
                        shininess) * specCoeff * distCoeff);
                colorTotal = colorTotal.add(colorSpec);
            }
        }
        
        /**
         * Reflection
         */
        if (depth > 0)
        {
            final Vector rr = dir.sub(normal.mul(dir.mul(2).dot(normal)));
            final Color colorReflect = raytrace(scene, depth - 1, hitPoint, rr.normalize());
            colorTotal = colorTotal.add(colorReflect.mul(finish.getReflectivness()));
        }
        
        return colorTotal;
    }
}
