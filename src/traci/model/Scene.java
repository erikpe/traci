package traci.model;

import java.util.ArrayList;
import java.util.List;

import traci.model.light.PointLight;
import traci.model.shape.Shape;

public class Scene
{
    public Camera camera = null;
    
    public Shape shape = null;
    
    public final List<PointLight> lights;
    
    public Scene()
    {
        this(null, null);
    }
    
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
    
    public void setCamera(final Camera camera)
    {
        this.camera = camera;
    }
    
    public void setShape(final Shape shape)
    {
        this.shape = shape;
    }
}
