package traci.math;

public abstract class Projection2D
{
    static double PI2 = 2.0 * Math.PI;

    public static Projection2D XZ_PLANE = new Projection2D()
    {
        @Override
        public Vector2D project(final Vector p)
        {
            return Vector2D.make(p.x(), p.z());
        }
    };

    public static Projection2D XY_PLANE = new Projection2D()
    {
        @Override
        public Vector2D project(final Vector p)
        {
            return Vector2D.make(p.x(), p.y());
        }
    };

    public static Projection2D YZ_PLANE = new Projection2D()
    {
        @Override
        public Vector2D project(final Vector p)
        {
            return Vector2D.make(p.y(), p.z());
        }
    };

    public static Projection2D CYLINDER = new Projection2D()
    {
        @Override
        public Vector2D project(final Vector p)
        {
            final double d = Math.sqrt(p.x() * p.x() + p.z() * p.z());
            final double cosAlpha = p.x() / d;

            final double alpha = (p.z() > 0.0 ? Math.acos(cosAlpha) : PI2 - Math.acos(cosAlpha));

            return Vector2D.make(alpha / PI2, p.y());
        }
    };

    public abstract Vector2D project(final Vector p);
}
