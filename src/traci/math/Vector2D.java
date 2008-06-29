package traci.math;

public class Vector2D
{
    public final double x;
    public final double y;
    
    private Vector2D(final double x, final double y)
    {
        this.x = x;
        this.y = y;
    }
    
    public static Vector2D make(final double x, final double y)
    {
        return new Vector2D(x, y);
    }
}
