package se.ejp.traci.model.light;

import se.ejp.traci.model.material.Color;

public class AmbientLight extends Light
{
    public final Color color;

    private AmbientLight(final Color color)
    {
        this.color = color;
    }

    public static AmbientLight make(final Color color)
    {
        return new AmbientLight(color);
    }
}
