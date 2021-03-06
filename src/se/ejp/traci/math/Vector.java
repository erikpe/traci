package se.ejp.traci.math;

public final class Vector
{
    private final double x, y, z;

    public static final Vector ORIGO = new Vector(0, 0, 0);

    public static final Vector UNIT_X = new Vector(1, 0, 0);
    public static final Vector UNIT_Y = new Vector(0, 1, 0);
    public static final Vector UNIT_Z = new Vector(0, 0, 1);

    public static final Vector UNIT_NEG_X = new Vector(-1, 0, 0);
    public static final Vector UNIT_NEG_Y = new Vector(0, -1, 0);
    public static final Vector UNIT_NEG_Z = new Vector(0, 0, -1);

    private Vector(final double x, final double y, final double z)
    {
        this.x = x;
        this.y = y;
        this.z = z;
    }

    public static Vector make(final double x, final double y, final double z)
    {
        return new Vector(x, y, z);
    }

    public static Vector make(final Double x, final Double y, final Double z)
    {
        return make(x.doubleValue(), y.doubleValue(), z.doubleValue());
    }

    public double x()
    {
        return x;
    }

    public double y()
    {
        return y;
    }

    public double z()
    {
        return z;
    }

    public double length()
    {
        return Math.sqrt(x * x + y * y + z * z);
    }

    public Vector normalize()
    {
        return div(length());
    }

    public double dot(final Vector vec)
    {
        return x * vec.x + y * vec.y + z * vec.z;
    }

    public Vector add(final Vector vec)
    {
        return make(x + vec.x, y + vec.y, z + vec.z);
    }

    public Vector sub(final Vector vec)
    {
        return make(x - vec.x, y - vec.y, z - vec.z);
    }

    public Vector neg()
    {
        return make(-x, -y, -z);
    }

    public Vector mul(final double val)
    {
        return make(val * x, val * y, val * z);
    }

    public Vector div(final double val)
    {
        return make(x / val, y / val, z / val);
    }

    public Vector cross(final Vector vec)
    {
        return make(y * vec.z - z * vec.y, z * vec.x - x * vec.z, x * vec.y - y * vec.x);
    }

    @Override
    public String toString()
    {
        return "[" + x + "," + y + "," + z + "]";
    }

    @Override
    public int hashCode()
    {
        int hash = getClass().hashCode() | 0x0000001;
        hash = 31 * hash + Double.valueOf(x).hashCode();
        hash = 31 * hash + Double.valueOf(y).hashCode();
        hash = 31 * hash + Double.valueOf(z).hashCode();
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

        final Vector otherVector = (Vector) other;

        return Double.valueOf(x).equals(Double.valueOf(otherVector.x)) &&
               Double.valueOf(y).equals(Double.valueOf(otherVector.y)) &&
               Double.valueOf(z).equals(Double.valueOf(otherVector.z));
    }
}
