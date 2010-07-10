package traci.model;

import traci.math.Transformable;
import traci.math.TransformableHelper;
import traci.math.Transformation;
import traci.math.Vector;

public class Camera extends TransformableHelper implements Transformable
{
    public final double fov = (50.0 / 360.0) * Math.PI * 2.0;
    
    public Transformation transformation;
    
    public final double focalDist = 4.8;
    
    public final double aperture = 0.15;
    
    public Camera(final Vector location, final Vector lookAt, final Vector up)
    {
        transformation = Transformation.identity();
        calcTransformation(location, lookAt, up);
    }
    
    public static boolean isCamera(final String str)
    {
        return str.equals("camera");
    }
    
    /**
     * The initial camera position is in origo, and it is directed towards
     * {@link Vector.UNIT_NEG_Z} with {@link Vector.UNIT_Y} as up.
     */
    private void calcTransformation(final Vector location, final Vector lookAt,
            final Vector up)
    {
        Vector cam = Vector.UNIT_NEG_Z;
        
        Vector u = lookAt.sub(location);
        
        final Vector u_XZ = Vector.make(u.x(), 0, u.z()).normalize();
        final double beta = Math.atan2(u_XZ.x(), u_XZ.z())
                - Math.atan2(cam.x(), cam.z());
        
        rotateY(beta);
        
        cam = transformation.dir(cam);
        
        final Vector u_YZ = Vector.make(0, u.y(), u.z()).normalize();
        final double alpha = Math.atan2(u_YZ.y(), u_YZ.z())
                - Math.atan2(cam.y(), cam.z());
        
        rotateX(alpha);
        translate(location);
    }
    
    @Override
    public void transform(final Transformation tr)
    {
        transformation = transformation.transform(tr);
    }
}
