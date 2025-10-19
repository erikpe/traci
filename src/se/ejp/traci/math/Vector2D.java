package se.ejp.traci.math;


public class Vector2D
{
    private final double x;
    private final double y;

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

    @Override
    public int hashCode()
    {
        int hash = getClass().hashCode() | 0x0000001;
        hash = 31 * hash + Double.valueOf(x).hashCode();
        hash = 31 * hash + Double.valueOf(y).hashCode();
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

        final Vector2D otherVector = (Vector2D) other;

        return Double.valueOf(x).equals(Double.valueOf(otherVector.x)) &&
               Double.valueOf(y).equals(Double.valueOf(otherVector.y));
    }
}
