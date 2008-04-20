package traci.model;

import traci.math.Matrix;
import traci.math.Vector;

public class Camera
{
    public final Vector location;
    public final Vector lookAt;
    
    public final double fov = (50.0 / 360.0) * Math.PI * 2.0;
    
    public final Matrix mat;
    
    public Camera(final Vector location, final Vector lookAt)
    {
        this.location = location;
        this.lookAt = lookAt;
        
        mat = calcMat();
    }
    
    public static boolean isCamera(final String str)
    {
        return str.equals("camera");
    }
    
    /**
     * The initial camera position is in origo, and it is directed
     * towards {@link Vector.UNIT_NEG_Z}.
     */
    private Matrix calcMat()
    {
        final Vector u = lookAt.sub(location);
        
        final Vector u_YZ = Vector.make(0, u.y, u.z);
        double cosAlpha = u_YZ.cosTheta(Vector.UNIT_NEG_Z);
        double sinAlpha = u_YZ.sinTheta(Vector.UNIT_NEG_Z);
        sinAlpha = (u_YZ.cross(Vector.UNIT_NEG_Z).x > 0 ? -sinAlpha : sinAlpha);
        
        final Vector u_XZ = Vector.make(u.x, 0, u.z);
        double cosBeta = u_XZ.cosTheta(Vector.UNIT_NEG_Z);
        double sinBeta = u_XZ.sinTheta(Vector.UNIT_NEG_Z);
        sinBeta = (u_XZ.cross(Vector.UNIT_NEG_Z).y > 0 ? -sinBeta : sinBeta);
        
        final Matrix rotX = Matrix.rotX(sinAlpha, cosAlpha);
        final Matrix rotY = Matrix.rotY(sinBeta, cosBeta);
        final Matrix mat = rotY.mul(rotX);
        
        return mat;
    }
}
