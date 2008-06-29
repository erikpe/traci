package traci.model.material;

public class Color
{
    public final double r, g, b;
    
    public static final Color BLACK = Color.make(0, 0, 0);
    public static final Color WHITE = Color.make(1, 1, 1);
    public static final Color RED = Color.make(1, 0, 0);
    public static final Color GREEN = Color.make(0, 1, 0);
    public static final Color BLUE = Color.make(0, 0, 1);
    public static final Color YELLOW = Color.make(1, 1, 0);
    public static final Color CYAN = Color.make(0, 1, 1);
    public static final Color MAGENTA = Color.make(1, 0, 1);
    
    private Color(final double r, final double g, final double b)
    {
        this.r = r;
        this.g = g;
        this.b = b;
    }
    
    public static Color make(final double r, final double g, final double b)
    {
        return new Color(r, g, b);
    }
    
    public static Color makeRGB(final int rgb)
    {
        final double r = ((rgb >> 16) & 0xff) / 255.0;
        final double g = ((rgb >> 8) & 0xff) / 255.0;
        final double b = (rgb & 0xff) / 255.0;
        
        return make(r, g, b);
    }
    
    public Color mul(final double val)
    {
        return make(r * val, g * val, b * val);
    }
    
    public Color mul(final Color color)
    {
        return make(r * color.r, g * color.g, b * color.b);
    }
    
    public Color div(final double val)
    {
        return make(r / val, g / val, b / val);
    }
    
    public Color add(final Color color)
    {
        return make(r + color.r, g + color.g, b + color.b);
    }
    
    @Override
    public String toString()
    {
        return "<" + r + ", " + g + ", " + b + ">";
    }
}
