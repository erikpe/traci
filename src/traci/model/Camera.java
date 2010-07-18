package traci.model;

import traci.math.Transformable;
import traci.math.TransformableHelper;
import traci.math.Transformation;
import traci.math.Vector;

public class Camera extends TransformableHelper implements Transformable
{
    public final double fov = (15.0 / 360.0) * Math.PI * 2.0;
    
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
        final Vector dir = lookAt.sub(location).normalize();
        
        final Vector origLocation = Vector.ORIGO;
        final Vector origLookAt = Vector.UNIT_NEG_Z;
        final Vector origDir = origLookAt.sub(origLocation).normalize();
        
        final Vector dir_XZ = Vector.make(dir.x(), 0, dir.z()).normalize();
        final double beta = Math.atan2(dir_XZ.x(), dir_XZ.z())
                - Math.atan2(origDir.x(), origDir.z());
        
        Transformation tmpTr1 = Transformation.rotateY(beta);
        //Vector foo = tmpTr1.dir(origDir);
        Vector invRotDir = tmpTr1.dirInv(dir);
        
        final Vector dir_YZ = Vector.make(0, invRotDir.y(), invRotDir.z())
                .normalize();
        final double alpha = Math.atan2(dir_YZ.y(), dir_YZ.z())
                - Math.atan2(origDir.y(), origDir.z());
        
        rotateX(-alpha);
        rotateY(beta);
        translate(location);
        
        //foo = transformation.point(Vector.ORIGO);
        //Vector bar = transformation.dir(Vector.UNIT_NEG_Z);
    }
    
    @Override
    public void transform(final Transformation tr)
    {
        transformation = transformation.transform(tr);
    }
}
