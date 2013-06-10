package se.ejp.traci.math;

import java.util.HashMap;
import java.util.Map;
import java.util.Set;

public enum Projection2D
{
    XY_PLANE("xy")
    {
        @Override
        public Vector2D project(final Vector p)
        {
            return Vector2D.make(p.x(), p.y());
        }
    },

    XZ_PLANE("xz")
    {
        @Override
        public Vector2D project(final Vector p)
        {
            return Vector2D.make(p.x(), p.z());
        }
    },

    YX_PLANE("yx")
    {
        @Override
        public Vector2D project(final Vector p)
        {
            return Vector2D.make(p.y(), p.x());
        }
    },

    YZ_PLANE("yz")
    {
        @Override
        public Vector2D project(final Vector p)
        {
            return Vector2D.make(p.y(), p.z());
        }
    },

    ZX_PLANE("zx")
    {
        @Override
        public Vector2D project(final Vector p)
        {
            return Vector2D.make(p.z(), p.x());
        }
    },

    ZY_PLANE("zy")
    {
        @Override
        public Vector2D project(final Vector p)
        {
            return Vector2D.make(p.z(), p.y());
        }
    },

    CYLINDER("cylinder")
    {
        @Override
        public Vector2D project(final Vector p)
        {
            final double d = Math.sqrt(p.x() * p.x() + p.z() * p.z());
            final double cosAlpha = p.x() / d;

            final double alpha = (p.z() > 0.0 ? Math.acos(cosAlpha) : (Math.PI * 2) - Math.acos(cosAlpha));

            return Vector2D.make(alpha / (Math.PI * 2), p.y());
        }
    };

    public final String id;

    private static final Map<String, Projection2D> idMap = new HashMap<String, Projection2D>();
    static
    {
        for (final Projection2D proj2D : Projection2D.values())
        {
            idMap.put(proj2D.id, proj2D);
        }
    }

    private Projection2D(final String id)
    {
        this.id = id;
    }

    public static Projection2D get(final String id)
    {
        return idMap.get(id);
    }

    public static Set<String> getAllProjections()
    {
        return idMap.keySet();
    }

    public abstract Vector2D project(final Vector p);
}
