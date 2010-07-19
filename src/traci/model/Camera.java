package traci.model;

import traci.math.Transformable;
import traci.math.TransformableHelper;
import traci.math.Transformation;
import traci.math.Transformations;
import traci.math.Vector;

public class Camera extends TransformableHelper implements Transformable
{
    public final double fov = (50.0 / 360.0) * Math.PI * 2.0;
    
    public Transformation transformation;
    
    public final double focalDist = 4.8;
    
    public final double aperture = 0.15;
    
    public Camera(final Vector location, final Vector lookAt, final Vector up)
    {
        transformation = Transformations.camera(location, lookAt, up);
    }
    
    @Override
    public void transform(final Transformation tr)
    {
        transformation = transformation.compose(tr);
    }
}
