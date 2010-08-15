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
    private static enum Method { OLD_RAY, ISTACK, RAY2 }
    
    private static final Method method = Method.RAY2;
    
    protected static Color raytrace(final Scene scene, final int depth,
            final Vector p, final Vector dir)
    {
        double dist = 0;
        Primitive obj = null;
        
        switch (method)
        {
        case OLD_RAY:
            final Ray ray = scene.shape.shootRay(p, dir);
            
            if (ray == null)
            {
                return Color.RED;
            }
            
            final Point hit = ray.get(0).p0();
            
            dist = hit.dist();
            obj = hit.obj();
            break;
            
        case ISTACK:
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
            break;
            
        case RAY2:
            final Ray2 ray2 = scene.shape.shootRay2(p, dir);
            
            if (ray2 == null)
            {
                return Color.WHITE.mul(.5);
            }
            
            final Point2 hit2 = ray2.first();
            
            if (hit2 == null)
            {
                return Color.WHITE.mul(.5);
            }
            
            dist = hit2.dist();
            obj = hit2.obj();
            break;
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
            
            switch (method)
            {
            case OLD_RAY:
                final Ray lightRay = scene.shape.shootRay(hitPoint, dirToLight);
                if (lightRay != null)
                {
                    continue;
                }
                break;
                
            case ISTACK:
                if (scene.shape.anyIntersection(hitPoint, dirToLight))
                {
                    continue;
                }
                break;
                
            case RAY2:
                final Ray2 lightRay2 = scene.shape.shootRay2(hitPoint, dirToLight);
                if (lightRay2 != null && lightRay2.first() != null)
                {
                    continue;
                }
                break;
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
