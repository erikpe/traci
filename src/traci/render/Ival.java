package traci.render;


public class Ival
{
    public final Point p0;
    public final Point p1;
    
    public Ival(final Point p0, final Point p1)
    {
        assert p0.dist < p1.dist;
        
        this.p0 = p0;
        this.p1 = p1;
    }
}
