package traci.model.light;

import traci.math.Vector;
import traci.model.material.Color;

public class PointLight extends Light
{
    public final Vector location;
    public final Color color;

    public static final Color ambient = Color.make(0.4, 0.4, 0.4);

    public PointLight(final Vector location, final Color color)
    {
        this.location = location;
        this.color = color;
    }
}
