package se.ejp.traci.math;


public class Matrix
{
    private static final Matrix EYE = newEye();

    private final double[] data;

    private Matrix()
    {
        data = new double[16];
    }

    public double at(final int row, final int col)
    {
        return data[row * 4 + col];
    }

    private static Matrix newEye()
    {
        final Matrix res = newZero();

        res.data[0] = 1.0;
        res.data[5] = 1.0;
        res.data[10] = 1.0;
        res.data[15] = 1.0;

        return res;
    }

    private static Matrix newZero()
    {
        return new Matrix();
    }

    static Matrix eye()
    {
        return EYE;
    }

    static Matrix rotx(final double theta)
    {
        return rotx(Math.sin(theta), Math.cos(theta));
    }

    static Matrix rotx(final double sinTheta, final double cosTheta)
    {
        final Matrix res = newZero();

        res.data[0] = 1.0;
        res.data[5] = cosTheta;
        res.data[6] = -sinTheta;
        res.data[9] = sinTheta;
        res.data[10] = cosTheta;
        res.data[15] = 1.0;

        return res;
    }

    static Matrix roty(final double theta)
    {
        return roty(Math.sin(theta), Math.cos(theta));
    }

    static Matrix roty(final double sinTheta, final double cosTheta)
    {
        final Matrix res = newZero();

        res.data[0] = cosTheta;
        res.data[2] = sinTheta;
        res.data[5] = 1.0;
        res.data[8] = -sinTheta;
        res.data[10] = cosTheta;
        res.data[15] = 1.0;

        return res;
    }

    static Matrix rotz(final double theta)
    {
        return rotz(Math.sin(theta), Math.cos(theta));
    }

    static Matrix rotz(final double sinTheta, final double cosTheta)
    {
        final Matrix res = newZero();

        res.data[0] = cosTheta;
        res.data[1] = -sinTheta;
        res.data[4] = sinTheta;
        res.data[5] = cosTheta;
        res.data[10] = 1.0;
        res.data[15] = 1.0;

        return res;
    }

    static Matrix scale(final Vector scale)
    {
        final Matrix res = newZero();

        res.data[0] = scale.x();
        res.data[5] = scale.y();
        res.data[10] = scale.z();
        res.data[15] = 1.0;

        return res;
    }

    static Matrix translate(final Vector translate)
    {
        final Matrix res = newEye();

        res.data[3] = translate.x();
        res.data[7] = translate.y();
        res.data[11] = translate.z();

        return res;
    }

    static Matrix make(final Vector v0, final Vector v1, final Vector v2)
    {
        final Matrix res = newZero();

        res.data[0] = v0.x();
        res.data[4] = v0.y();
        res.data[8] = v0.z();

        res.data[1] = v1.x();
        res.data[5] = v1.y();
        res.data[9] = v1.z();

        res.data[2] = v2.x();
        res.data[6] = v2.y();
        res.data[10] = v2.z();

        res.data[15] = 1.0;

        return res;
    }

    Matrix mul(final Matrix mat)
    {
        final Matrix res = newZero();

        for (int row = 0; row < 4; ++row)
        {
            for (int col = 0; col < 4; ++col)
            {
                res.data[row * 4 + col] =
                        data[row * 4 + 0] * mat.data[0 * 4 + col] +
                        data[row * 4 + 1] * mat.data[1 * 4 + col] +
                        data[row * 4 + 2] * mat.data[2 * 4 + col] +
                        data[row * 4 + 3] * mat.data[3 * 4 + col];
            }
        }

        return res;
    }

    Vector mul(final Vector vec)
    {
        final double vx = vec.x();
        final double vy = vec.y();
        final double vz = vec.z();

        final double[] tmpData = data;

        return Vector.make(vx * tmpData[0] +
                           vy * tmpData[1] +
                           vz * tmpData[2] +
                           tmpData[3],

                           vx * tmpData[4] +
                           vy * tmpData[5] +
                           vz * tmpData[6] +
                           tmpData[7],

                           vx * tmpData[8] +
                           vy * tmpData[9] +
                           vz * tmpData[10] +
                           tmpData[11]);
    }

    Vector mulDir(final Vector vec)
    {
        final double vx = vec.x();
        final double vy = vec.y();
        final double vz = vec.z();

        final double[] tmpData = data;

        return Vector.make(vx * tmpData[0] +
                           vy * tmpData[1] +
                           vz * tmpData[2],

                           vx * tmpData[4] +
                           vy * tmpData[5] +
                           vz * tmpData[6],

                           vx * tmpData[8] +
                           vy * tmpData[9] +
                           vz * tmpData[10]);
    }

    Vector mulNormal(final Vector vec)
    {
        final double vx = vec.x();
        final double vy = vec.y();
        final double vz = vec.z();

        final double[] tmpData = data;

        return Vector.make(vx * tmpData[0] +
                           vy * tmpData[4] +
                           vz * tmpData[8],

                           vx * tmpData[1] +
                           vy * tmpData[5] +
                           vz * tmpData[9],

                           vx * tmpData[2] +
                           vy * tmpData[6] +
                           vz * tmpData[10]);
    }

    @Override
    public int hashCode()
    {
        int hash = getClass().hashCode();
        for (int i = 0; i < data.length; ++i)
        {
            hash = 31 * hash + Double.valueOf(data[i]).hashCode();
        }
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

        final Matrix otherMatrix = (Matrix) other;

        for (int i = 0; i < data.length; ++i)
        {
            if (!Double.valueOf(data[i]).equals(Double.valueOf(otherMatrix.data[i])))
            {
                return false;
            }
        }

        return true;
    }

    @Override
    public String toString()
    {
        final StringBuilder buf = new StringBuilder();

        buf.append("[ [");

        for (int row = 0; row < 4; ++row)
        {
            if (row > 0)
            {
                buf.append("\n  [");
            }

            for (int col = 0; col < 4; ++col)
            {
                buf.append(' ');
                buf.append(data[row * 4 + col]);
            }

            buf.append(" ]");
        }

        buf.append(" ]");

        return buf.toString();
    }
}
