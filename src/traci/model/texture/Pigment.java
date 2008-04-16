package traci.model.texture;

public class Pigment implements Cloneable
{
    protected Color color;
    
    protected Pigment() { }
    
    public Color getColor()
    {
        return color;
    }
    
    public void setColor(final Color color)
    {
        this.color = color;
    }
    
    @Override
    protected Object clone() throws CloneNotSupportedException
    {
        final Pigment res = (Pigment) super.clone();
        
        res.color = color;
        
        return res;
    }
}
