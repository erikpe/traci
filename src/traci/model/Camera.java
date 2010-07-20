package traci.model;

import traci.math.Transformable;
import traci.math.TransformableHelper;
import traci.math.Transformation;
import traci.math.Transformations;
import traci.math.Vector;

public class Camera extends TransformableHelper implements Transformable
{
    private final double fovx = (30.0 / 360.0) * Math.PI * 2.0;
    private final double aspectRatio = 4.0 / 3.0;
    
    private Transformation transformation;
    
    //private final double focalDist = 4.8;
    
    //private final double aperture = 0.15;
    
    private final double xx;
    private final double yy;
    
    public Camera(final Vector location, final Vector lookAt, final Vector up)
    {
        transformation = Transformations.camera(location, lookAt, up);
        
        final double fovy = fovx / aspectRatio;
        this.xx = 2.0 * Math.tan(fovx / 2.0);
        this.yy = 2.0 * Math.tan(fovy / 2.0);
    }
    
    @Override
    public void transform(final Transformation tr)
    {
        transformation = transformation.compose(tr);
    }
    
    public Vector getLoc()
    {
        return transformation.point(Vector.ORIGO);
    }
    
    public Vector getDir(final double lookX, final double lookY)
    {
        final double x = (lookX - 0.5) * xx;
        final double y = (0.5 - lookY) * yy;
        
        final Vector lookAt = Vector.make(x, y, 1);
        final Vector location = Vector.ORIGO;
        final Vector dir = lookAt.sub(location).normalize();
        
        return transformation.dir(dir);
    }
}
