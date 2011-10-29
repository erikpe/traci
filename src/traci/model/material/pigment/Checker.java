package traci.model.material.pigment;

import traci.math.Vector;
import traci.model.material.Color;

public class Checker extends Pattern
{
    private final Color color1;
    private final Color color2;

    public Checker(final Color color1, final Color color2)
    {
        this.color1 = color1;
        this.color2 = color2;
    }

    @Override
    public Color getColorTransformed(final Vector p)
    {
        if ((Math.round(p.x()) + Math.round(p.y()) + Math.round(p.z())) % 2 == 0)
        {
            return color1;
        }

        return color2;
    }
}
