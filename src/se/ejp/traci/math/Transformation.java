package se.ejp.traci.math;

public class Transformation
{
    public final Matrix mat;
    public final Matrix invMat;

    private Transformation(final Matrix mat, final Matrix invMat)
    {
        this.mat = mat;
        this.invMat = invMat;
    }

    static Transformation make(final Matrix mat, final Matrix invMat)
    {
        return new Transformation(mat, invMat);
    }

    public Transformation compose(final Transformation transformation)
    {
        final Matrix newMat = transformation.mat.mul(mat);
        final Matrix newInvMat = invMat.mul(transformation.invMat);

        return make(newMat, newInvMat);
    }

    public Transformation invert()
    {
        return make(invMat, mat);
    }

    public Vector point(final Vector vec)
    {
        return mat.mul(vec);
    }

    public Vector pointInv(final Vector vec)
    {
        return invMat.mul(vec);
    }

    public Vector dir(final Vector vec)
    {
        return mat.mulDir(vec);
    }

    public Vector dirInv(final Vector vec)
    {
        return invMat.mulDir(vec);
    }

    public Vector normal(final Vector vec)
    {
        return invMat.mulNormal(vec);
    }

    public Vector normalInv(final Vector vec)
    {
        return mat.mulNormal(vec);
    }

    @Override
    public int hashCode()
    {
        return 31 * mat.hashCode() + invMat.hashCode();
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

        final Transformation otherTransformation = (Transformation) other;

        return mat.equals(otherTransformation.mat) && invMat.equals(otherTransformation.invMat);
    }

    @Override
    public String toString()
    {
        return "Transformation";
    }
}
