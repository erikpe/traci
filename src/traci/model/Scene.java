package traci.model;

import java.util.ArrayList;
import java.util.List;

import traci.model.light.PointLight;
import traci.model.shape.Shape;

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
