package traci.math;

public class Matrix
{
    private static final Matrix EYE = newEye();
    
    private final double[][] data;
    
    private Matrix()
    {
        data = new double[4][4];
    }
    
    private static Matrix newEye()
    {
        Matrix res = newZero();
        
        res.data[0][0] = 1.0;
        res.data[1][1] = 1.0;
        res.data[2][2] = 1.0;
        res.data[3][3] = 1.0;
        
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
        final Matrix res = newEye();
        
        res.data[1][1] = cosTheta;
        res.data[1][2] = -sinTheta;
        res.data[2][1] = sinTheta;
        res.data[2][2] = cosTheta;
        
        return res;
    }
    
    static Matrix roty(final double theta)
    {
        return roty(Math.sin(theta), Math.cos(theta));
    }
    
    static Matrix roty(final double sinTheta, final double cosTheta)
    {
        final Matrix res = newEye();
        
        res.data[0][0] = cosTheta;
        res.data[0][2] = sinTheta;
        res.data[2][0] = -sinTheta;
        res.data[2][2] = cosTheta;
        
        return res;
    }
    
    static Matrix rotz(final double theta)
    {
        return rotz(Math.sin(theta), Math.cos(theta));
    }
    
    static Matrix rotz(final double sinTheta, final double cosTheta)
    {
        final Matrix res = newEye();
        
        res.data[0][0] = cosTheta;
        res.data[0][1] = -sinTheta;
        res.data[1][0] = sinTheta;
        res.data[1][1] = cosTheta;
        
        return res;
    }
    
    static Matrix scale(final Vector scale)
    {
        final Matrix res = newEye();
        
        res.data[0][0] = scale.x();
        res.data[1][1] = scale.y();
        res.data[2][2] = scale.z();
        
        return res;
    }
    
    static Matrix translate(final Vector translate)
    {
        final Matrix res = newEye();
        
        res.data[0][3] = translate.x();
        res.data[1][3] = translate.y();
        res.data[2][3] = translate.z();
        
        return res;
    }
    
    static Matrix make(final Vector v0, final Vector v1, final Vector v2)
    {
        final Matrix res = newEye();
        
        res.data[0][0] = v0.x();
        res.data[1][0] = v0.y();
        res.data[2][0] = v0.z();
        
        res.data[0][1] = v1.x();
        res.data[1][1] = v1.y();
        res.data[2][1] = v1.z();
        
        res.data[0][2] = v2.x();
        res.data[1][2] = v2.y();
        res.data[2][2] = v2.z();
        
        return res;
    }
    
    Matrix mul(final Matrix mat)
    {
        final Matrix res = newZero();
        
        for (int row = 0; row < 4; ++row)
        {
            for (int col = 0; col < 4; ++col)
            {
                res.data[row][col] = data[row][0] * mat.data[0][col] +
                                     data[row][1] * mat.data[1][col] +
                                     data[row][2] * mat.data[2][col] +
                                     data[row][3] * mat.data[3][col];
            }
        }
        
        return res;
    }
    
    Vector mul(final Vector vec)
    {
        final double x = vec.x() * data[0][0] +
                         vec.y() * data[0][1] +
                         vec.z() * data[0][2] +
                         data[0][3];
        
        final double y = vec.x() * data[1][0] +
                         vec.y() * data[1][1] +
                         vec.z() * data[1][2] +
                         data[1][3];
        
        final double z = vec.x() * data[2][0] +
                         vec.y() * data[2][1] +
                         vec.z() * data[2][2] +
                         data[2][3];
        
        return Vector.make(x, y, z);
    }
    
    Vector mulDir(final Vector vec)
    {
        final double vx = vec.x();
        final double vy = vec.y();
        final double vz = vec.z();
        
        final double x = vx * data[0][0] +
                         vy * data[0][1] +
                         vz * data[0][2];
        
        final double y = vx * data[1][0] +
                         vy * data[1][1] +
                         vz * data[1][2];
        
        final double z = vx * data[2][0] +
                         vy * data[2][1] +
                         vz * data[2][2];
        
        return Vector.make(x, y, z);
    }
    
    Vector mulNormal(final Vector vec)
    {
        final double vx = vec.x();
        final double vy = vec.y();
        final double vz = vec.z();
        
        final double x = vx * data[0][0] +
                         vy * data[1][0] +
                         vz * data[2][0];
        
        final double y = vx * data[0][1] +
                         vy * data[1][1] +
                         vz * data[2][1];
        
        final double z = vx * data[0][2] +
                         vy * data[1][2] +
                         vz * data[2][2];
        
        return Vector.make(x, y, z);
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
                buf.append(data[row][col]);
            }
            
            buf.append(" ]");
        }
        
        buf.append(" ]");
        
        return buf.toString();
    }
}
