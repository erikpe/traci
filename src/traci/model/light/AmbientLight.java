package traci.model.light;

import traci.model.material.Color;

public class AmbientLight extends Light
{
    public final Color color;

    public AmbientLight(final Color color)
    {
        this.color = color;
    }
}
