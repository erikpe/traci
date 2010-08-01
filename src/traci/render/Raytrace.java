package traci.render;

import traci.math.Vector;
import traci.model.Scene;
import traci.model.light.PointLight;
import traci.model.material.Color;
import traci.model.material.Finish;
import traci.model.material.pigment.Pigment;
import traci.model.shape.primitive.Primitive;

public class Raytrace
{
    private static final boolean USE_NEW_MODEL = false;
    
    protected static Color raytrace(final Scene scene, final int depth,
            final Vector p, final Vector dir)
    {
        double dist;
        Primitive obj;
        
        if (USE_NEW_MODEL)
        {
            final IntersectionStack iStack = IntersectionStack.make();
            scene.shape.allIntersections(iStack, p, dir);
            
            if (iStack.isEmpty())
            {
                return Color.RED;
            }
            
            dist = iStack.dists[0];
            obj = iStack.objs[0];
            
            for (int i = 1; i < iStack.size(); ++i)
            {
                if (iStack.dists[i] < dist)
                {
                    dist = iStack.dists[i];
                    obj = iStack.objs[i];
                }
            }
        }
        else
        {
            final Ray ray = scene.shape.shootRay(p, dir);
            
            if (ray == null)
            {
                return Color.RED;
            }
            
            final Point hit = ray.get(0).p0();
            
            dist = hit.dist();
            obj = hit.obj();
        }
        
        final Vector hitPoint = p.add(dir.mul(dist));
        final Vector normal = obj.getNormalAt(hitPoint, dir);
        
        final Pigment pigment = obj.material.getPigment();
        final Finish finish = obj.material.getFinish();
        
        /**
         * Ambient light
         */
        final Color colorAmb = pigment.getColor(hitPoint).mul(PointLight.ambient);
        Color colorTotal = colorAmb;
        
        for (final PointLight light : scene.lights)
        {
            final Vector toLight = light.location.sub(hitPoint);
            final Vector dirToLight = toLight.normalize();
            
            if (USE_NEW_MODEL)
            {
                final IntersectionStack iStack = IntersectionStack.make();
                scene.shape.allIntersections(iStack, hitPoint, dirToLight);
                
                if (!iStack.isEmpty())
                {
                    continue;
                }
            }
            else
            {
                final Ray lightRay = scene.shape.shootRay(hitPoint, dirToLight);
                
                if (lightRay != null)
                {
                    continue;
                }
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
