package traci.model.light;

import traci.math.Vector;
import traci.model.texture.Color;

public class PointLight
{
    public final Vector location;
    public final Color color;
    
    public PointLight(final Vector location, final Color color)
    {
        this.location = location;
        this.color = color;
    }
}
