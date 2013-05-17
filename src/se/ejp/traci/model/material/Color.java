package se.ejp.traci.model.material;

import se.ejp.traci.math.Vector;

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

    public static Color make(final Vector v)
    {
        return make(v.x(), v.y(), v.z());
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

    public Color sub(final Color color)
    {
        return make(r - color.r, g - color.g, b - color.b);
    }

    public Color neg()
    {
        return make(-r, -g, -b);
    }

    @Override
    public int hashCode()
    {
        int hash = getClass().hashCode();
        hash = 31 * hash + Double.valueOf(r).hashCode();
        hash = 31 * hash + Double.valueOf(g).hashCode();
        hash = 31 * hash + Double.valueOf(b).hashCode();
        return hash;
    }

    @Override
    public boolean equals(final Object other)
    {
        if (other == null)
        {
            return false;
        }
        else if (other == this)
        {
            return true;
        }
        else if (other.getClass() != getClass())
        {
            return false;
        }

        final Color otherColor = (Color) other;

        return Double.valueOf(r).equals(Double.valueOf(otherColor.r)) &&
               Double.valueOf(g).equals(Double.valueOf(otherColor.g)) &&
               Double.valueOf(b).equals(Double.valueOf(otherColor.b));
    }

    @Override
    public String toString()
    {
        return "[" + r + "," + g + "," + b + "]";
    }
}
