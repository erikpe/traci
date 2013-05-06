package se.ejp.traci.model.light;

import se.ejp.traci.math.Vector;
import se.ejp.traci.model.material.Color;

public class PointLight extends Light
{
    public final Vector location;
    public final Color color;

    public PointLight(final Vector location, final Color color)
    {
        this.location = location;
        this.color = color;
    }
}
