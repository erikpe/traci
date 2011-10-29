package traci.math;


public class Vector2D
{
    private double x;
    private double y;
    
    private Vector2D(final double x, final double y)
    {
        this.x = x;
        this.y = y;
    }
    
    public static Vector2D make(final double x, final double y)
    {
        return new Vector2D(x, y);
    }
    
    public double x()
    {
        return x;
    }
    
    public double y()
    {
        return y;
    }
}
