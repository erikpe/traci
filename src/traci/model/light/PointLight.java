package traci.model.light;

import traci.math.Vector;
import traci.model.material.Color;

public class PointLight
{
    public final Vector location;
    public final Color color;
    
    public static final Color ambient = Color.make(0.1, 0.1, 0.1);
    
    public static boolean isLight(final String str)
    {
        return str.equals("light");
    }
    
    public PointLight(final Vector location, final Color color)
    {
        this.location = location;
        this.color = color;
    }
}
