package se.ejp.traci.model.light;

import se.ejp.traci.math.Transformation;
import se.ejp.traci.model.Color;

public class AmbientLight extends Light
{
    private final Color color;

    private AmbientLight(final Color color)
    {
        this.color = color;
    }

    public static AmbientLight make(final Color color)
    {
        return new AmbientLight(color);
    }

    @Override
    public void transform(final Transformation transformation)
    {
        // Intentionally left blank
    }

    public Color getColor()
    {
        return color;
    }
}
