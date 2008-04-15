package traci.model;

import java.util.ArrayList;
import java.util.List;

import traci.model.csg.Shape;
import traci.model.light.PointLight;

public class Scene
{
    public final Camera camera;
    
    public final Shape shape;
    
    public final List<PointLight> lights;
    
    public Scene(final Shape shape, final Camera camera)
    {
        this.camera = camera;
        this.shape = shape;
        this.lights = new ArrayList<PointLight>();
    }
    
    public void addLight(final PointLight light)
    {
        lights.add(light);
    }
}
