package traci.model;

import java.util.ArrayList;
import java.util.List;

import traci.model.light.AmbientLight;
import traci.model.light.Light;
import traci.model.light.PointLight;
import traci.model.material.Color;
import traci.model.shape.Shape;

public class Scene
{
    public Shape rootShape;
    public Camera camera;

    public AmbientLight ambientLight;
    public final List<PointLight> pointLights;

    public Color backgroundColor;

    public Scene()
    {
        this.rootShape = null;
        this.camera = null;

        this.ambientLight = null;
        this.pointLights = new ArrayList<PointLight>();

        this.backgroundColor = Color.WHITE.mul(.25);
    }

    public void setCamera(final Camera camera)
    {
        this.camera = camera;
    }

    public void setRootShape(final Shape rootShape)
    {
        this.rootShape = rootShape;
    }

    public void addLight(final Light light)
    {
        if (light instanceof PointLight)
        {
            addPointLight((PointLight) light);
        }
        else if (light instanceof AmbientLight)
        {
            setAmbientLight((AmbientLight) light);
        }
    }

    public void addPointLight(final PointLight pointLight)
    {
        pointLights.add(pointLight);
    }

    public void setAmbientLight(final AmbientLight ambientLight)
    {
        this.ambientLight = ambientLight;
    }

    public void setBackgroundColor(final Color backgroundColor)
    {
        this.backgroundColor = backgroundColor;
    }
}
