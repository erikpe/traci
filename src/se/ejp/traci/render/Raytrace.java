package se.ejp.traci.render;

import se.ejp.traci.math.Vector;
import se.ejp.traci.model.Color;
import se.ejp.traci.model.Scene;
import se.ejp.traci.model.light.PointLight;
import se.ejp.traci.model.material.Finish;
import se.ejp.traci.model.material.Interior;
import se.ejp.traci.model.material.pigment.Pigment;
import se.ejp.traci.model.shape.primitive.Primitive;
import se.ejp.traci.render.Point.Type;

public class Raytrace
{
    protected static Color raytrace(final Scene scene, final int depth, final Vector p, final Vector dir,
            final Interior inside)
    {
        if (depth < 0)
        {
            return Color.BLACK;
        }

        final Ray ray = scene.rootShape.shootRay(p, dir);

        if (ray == null)
        {
            return scene.backgroundColor;
        }

        final Point hit = ray.first();

        if (hit == null)
        {
            return scene.backgroundColor;
        }

        final double dist = hit.dist;
        final Primitive obj = hit.obj;

        final Vector hitPoint = p.add(dir.mul(dist));
        final Vector normal = obj.getNormalAt(hitPoint, dir);

        final Pigment pigment = obj.getMaterial().texture.pigment;
        final Finish finish = obj.getMaterial().texture.finish;

        final Color hitPointColor = pigment.getColor(hitPoint);

        /**
         * Ambient light
         */
        Color colorTotal;
        if (scene.ambientLight != null)
        {
            colorTotal = hitPointColor.mul(scene.ambientLight.getColor());
        }
        else
        {
            colorTotal = Color.BLACK;
        }

        for (final PointLight light : scene.pointLights)
        {
            final Vector toLight = light.getLocation().sub(hitPoint);
            final Vector dirToLight = toLight.normalize();

            /**
             * Check if path to light is obstructed
             */
            final Ray lightRay2 = scene.rootShape.shootRay(hitPoint, dirToLight);
            final double distToLight = toLight.length();
            if (lightRay2 != null)
            {
                final Point pp = lightRay2.first();
                if (pp != null && pp.dist < distToLight)
                {
                    /* Light is obstructed */
                    continue;
                }
            }

            final double distCoeff = 1.0 / (distToLight * distToLight);
            final Color lightAtPoint = light.getColor().mul(distCoeff);

            /**
             * Diffuse light
             */
            final double c = Math.max(dirToLight.dot(normal), 0.0);
            final Color colorDiff = hitPointColor.mul(lightAtPoint.mul(c * finish.diffCoeff));

            colorTotal = colorTotal.add(colorDiff);

            /**
             * Specular light
             */
            final Vector lightRef = normal.mul(normal.mul(2).dot(dirToLight)).sub(dirToLight);
            final double cosTheta = -lightRef.dot(dir);

            if (cosTheta > 0)
            {
                final double shininess = finish.shininess;
                final double specCoeff = finish.specCoeff;

                final Color colorSpec = light.getColor().mul(Math.pow(cosTheta, shininess) * specCoeff * distCoeff);
                colorTotal = colorTotal.add(colorSpec);
            }
        }

        /**
         * Refraction
         */
        if (obj.getMaterial().interior != Interior.OPAQUE && hitPointColor.transmit > 0.0)
        {
            final Interior newInside;
            if (hit.type == Type.ENTER)
            {
                newInside = obj.getMaterial().interior;
            }
            else if (hit.type == Type.INTERSECT)
            {
                newInside = inside;
            }
            else // Type.LEAVE
            {
                newInside = null;
            }

            final Vector refractDir = refractDir(normal, dir, inside, newInside);

            if (refractDir != null)
            {
                final Color colorRefract = raytrace(scene, depth - 1, hitPoint, refractDir, newInside);
                colorTotal = colorTotal.add(colorRefract);
            }
        }

        /**
         * Reflection
         */
        final Vector rr = dir.sub(normal.mul(dir.mul(2).dot(normal)));
        final Color colorReflect = raytrace(scene, depth - 1, hitPoint, rr, inside);
        colorTotal = colorTotal.add(colorReflect.mul(finish.reflectiveness));

        if (inside != null)
        {
            colorTotal = inside.filterThrough(colorTotal, dist);
        }

        return colorTotal;
    }

    private static Vector refractDir(final Vector normal, final Vector incident, final Interior i1, final Interior i2)
    {
        final double n1 = (i1 == null ? 1.0 : i1.ior);
        final double n2 = (i2 == null ? 1.0 : i2.ior);

        if (n1 == n2)
        {
            return incident;
        }

        final double n = n1 / n2;
        final double cosI = -normal.dot(incident);
        final double sinT2 = n * n * (1.0 - cosI * cosI);

        if (sinT2 > 1.0)
        {
            return null; // Total internal reflection
        }

        final double cosT = Math.sqrt(1.0 - sinT2);
        return incident.mul(n).add(normal.mul(n * cosI - cosT)).normalize();
    }
}
