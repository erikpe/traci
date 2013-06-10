package se.ejp.traci.render;

import se.ejp.traci.math.Vector;
import se.ejp.traci.model.Color;
import se.ejp.traci.model.Scene;
import se.ejp.traci.model.light.PointLight;
import se.ejp.traci.model.material.Finish;
import se.ejp.traci.model.material.Interior;
import se.ejp.traci.model.material.Material;
import se.ejp.traci.model.shape.primitive.Primitive;

public class Raytrace
{
    protected static Color raytrace(final Scene scene, final int depth, final Vector p, final Vector dir, final Interior inside)
    {
        if (depth <= 0)
        {
            return Color.BLACK;
        }

        final Ray ray = scene.rootShape.shootRay(p, dir);

        final Point hit;
        if (ray == null || (hit = ray.first()) == null)
        {
            if (scene.skybox != null)
            {
                return scene.skybox.sample(dir);
            }

            return scene.backgroundColor;
        }

        final double dist = hit.dist;
        final Primitive obj = hit.obj;

        final Vector hitPoint = p.add(dir.mul(dist));
        final Vector normal = obj.getNormalAt(hitPoint, dir, hit.normal);

        final Material material = obj.getMaterial();
        final Finish finish = material.texture.finish;

        final Color hitPointColor = material.texture.pigment.getColor(hitPoint);

        final boolean hasPhong = hitPointColor.transmit < 1.0;
        final boolean hasRefraction = material.interior != Interior.OPAQUE && hitPointColor.transmit > 0.0;
        final boolean hasReflection = hasRefraction || finish.reflectiveness > 0.0;

        /**
         * Ambient light
         */
        Color phongColor = Color.BLACK;
        if (hasPhong)
        {
            if (scene.ambientLight != null)
            {
                phongColor = hitPointColor.mul(scene.ambientLight.getColor());
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
                        // Light is obstructed
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

                phongColor = phongColor.add(colorDiff);

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
                    phongColor = phongColor.add(colorSpec);
                }
            }
        }

        /**
         * Reflection
         */
        Color reflectColor = null;
        if (hasReflection)
        {
            final Vector reflectDir = dir.sub(normal.mul(dir.mul(2).dot(normal)));
            reflectColor = raytrace(scene, depth - 1, hitPoint, reflectDir, inside);
        }

        /**
         * Refraction
         */
        Color fresnelColor = null;
        if (hasRefraction)
        {
            final Vector refractDir;
            final Interior newInside;

            switch (hit.type)
            {
            case ENTER:
                newInside = obj.getMaterial().interior;
                refractDir = refractDir(normal, dir, inside, newInside);
                break;

            case LEAVE:
                newInside = null;
                refractDir = refractDir(normal, dir, inside, newInside);
                break;

            default: // INTERSECT
                newInside = inside;
                refractDir = dir;
                break;
            }

            final double reflectWeight = reflectance(normal, dir, inside, obj.getMaterial().interior);
            final double refractWeight = (1.0 - reflectWeight);

            Color refractColor = Color.BLACK;
            if (refractDir != null)
            {
                //return Color.YELLOW.mul(reflectWeight);
                refractColor = raytrace(scene, depth - 1, hitPoint, refractDir, newInside);
            }
//            else
//            {
//                return Color.RED;
//            }

            fresnelColor = reflectColor.mul(reflectWeight).add(refractColor.mul(refractWeight));
        }

        Color totalColor = phongColor;
        if (hasReflection)
        {
            totalColor = totalColor.add(reflectColor.mul(finish.reflectiveness));
            totalColor = totalColor.mul(1.0 - hitPointColor.transmit);
        }

        if (hasRefraction)
        {
            totalColor = totalColor.add(fresnelColor.mul(hitPointColor.transmit));
        }

        if (inside != null)
        {
            totalColor = inside.filterThrough(totalColor, dist);
        }

        return totalColor;
    }

    private static double reflectance(final Vector normal, final Vector incident, final Interior i1, final Interior i2)
    {
        final double n1 = (i1 == null ? 1.0 : i1.ior);
        final double n2 = (i2 == null ? 1.0 : i2.ior);

        final double n = n1 / n2;
        final double cosI = -normal.dot(incident);
        final double sinT2 = n * n * (1.0 - cosI * cosI);

        if (sinT2 > 1.0)
        {
            return 1.0; // Total internal reflection
        }

        final double cosT = Math.sqrt(1.0 - sinT2);
        final double rOrth = (n1 * cosI - n2 * cosT) / (n1 * cosI + n2 * cosT);
        final double rPar = (n2 * cosI - n1 * cosT) / (n2 * cosI + n1 * cosT);
        return (rOrth * rOrth + rPar * rPar) / 2.0;
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
