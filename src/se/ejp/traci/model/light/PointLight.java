package se.ejp.traci.model.light;

import se.ejp.traci.math.Transformation;
import se.ejp.traci.math.Vector;
import se.ejp.traci.model.material.Color;

public class PointLight extends Light
{
    private Vector location;
    private final Color color;

    private PointLight(final Vector location, final Color color)
    {
        this.location = location;
        this.color = color;
    }

    public static PointLight make(final Vector location, final Color color)
    {
        return new PointLight(location, color);
    }

    @Override
    public void transform(final Transformation transformation)
    {
        location = transformation.point(location);
    }

    public Vector getLocation()
    {
        return location;
    }

    public Color getColor()
    {
        return color;
    }
}
