package se.ejp.traci.model.light;

import se.ejp.traci.model.material.Color;

public class AmbientLight extends Light
{
    public final Color color;

    public AmbientLight(final Color color)
    {
        this.color = color;
    }
}
