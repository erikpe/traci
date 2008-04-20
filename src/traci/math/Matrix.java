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
    
    public static Matrix eye()
    {
        return EYE;
    }
    
    public static Matrix rotX(final double theta)
    {
        return rotX(Math.sin(theta), Math.cos(theta));
    }
    
    public static Matrix rotX(final double sinTheta, final double cosTheta)
    {
        final Matrix res = newEye();
        
        res.data[1][1] = cosTheta;
        res.data[1][2] = -sinTheta;
        res.data[2][1] = sinTheta;
        res.data[2][2] = cosTheta;
        
        return res;
    }
    
    public static Matrix rotY(final double theta)
    {
        return rotY(Math.sin(theta), Math.cos(theta));
    }
    
    public static Matrix rotY(final double sinTheta, final double cosTheta)
    {
        final Matrix res = newEye();
        
        res.data[0][0] = cosTheta;
        res.data[0][2] = sinTheta;
        res.data[2][0] = -sinTheta;
        res.data[2][2] = cosTheta;
        
        return res;
    }
    
    public static Matrix rotZ(final double theta)
    {
        return rotZ(Math.sin(theta), Math.cos(theta));
    }
    
    public static Matrix rotZ(final double sinTheta, final double cosTheta)
    {
        final Matrix res = newEye();
        
        res.data[0][0] = cosTheta;
        res.data[0][1] = -sinTheta;
        res.data[1][0] = sinTheta;
        res.data[1][1] = cosTheta;
        
        return res;
    }
    
    public static Matrix scale(final Vector scale)
    {
        final Matrix res = newEye();
        
        res.data[0][0] = scale.x;
        res.data[1][1] = scale.y;
        res.data[2][2] = scale.z;
        
        return res;
    }
    
    public static Matrix translate(final Vector translate)
    {
        final Matrix res = newEye();
        
        res.data[0][3] = translate.x;
        res.data[1][3] = translate.y;
        res.data[2][3] = translate.z;
        
        return res;
    }
    
    public Matrix mul(final Matrix mat)
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
    
    public Vector mul(final Vector vec)
    {
        final double x = vec.x * data[0][0] +
                         vec.y * data[0][1] +
                         vec.z * data[0][2] +
                         data[0][3];
        
        final double y = vec.x * data[1][0] +
                         vec.y * data[1][1] +
                         vec.z * data[1][2] +
                         data[1][3];
        
        final double z = vec.x * data[2][0] +
                         vec.y * data[2][1] +
                         vec.z * data[2][2] +
                         data[2][3];
        
        return Vector.make(x, y, z);
    }
    
    protected Vector mulDir(final Vector vec)
    {
        final double x = vec.x * data[0][0] +
                         vec.y * data[0][1] +
                         vec.z * data[0][2];
        
        final double y = vec.x * data[1][0] +
                         vec.y * data[1][1] +
                         vec.z * data[1][2];
        
        final double z = vec.x * data[2][0] +
                         vec.y * data[2][1] +
                         vec.z * data[2][2];
        
        return Vector.make(x, y, z);
    }
    
    protected Vector mulNormal(final Vector vec)
    {
        final double x = vec.x * data[0][0] +
                         vec.y * data[1][0] +
                         vec.z * data[2][0];
        
        final double y = vec.x * data[0][1] +
                         vec.y * data[1][1] +
                         vec.z * data[2][1];
        
        final double z = vec.x * data[0][2] +
                         vec.y * data[1][2] +
                         vec.z * data[2][2];
        
        return Vector.make(x, y, z);
    }
    
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
