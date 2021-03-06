package se.ejp.traci.model;

import java.util.ArrayList;
import java.util.List;

import se.ejp.traci.model.light.AmbientLight;
import se.ejp.traci.model.light.Light;
import se.ejp.traci.model.light.PointLight;
import se.ejp.traci.model.shape.csg.Union;

public class Scene
{
    public final Union rootUnion;
    public Camera camera;

    public AmbientLight ambientLight;
    public final List<PointLight> pointLights;

    public Skybox skybox;
    public Color backgroundColor;

    public Scene()
    {
        this.rootUnion = Union.make();
        this.camera = null;

        this.ambientLight = null;
        this.pointLights = new ArrayList<PointLight>();

        this.backgroundColor = Color.WHITE.mul(.25);
    }

    public void setCamera(final Camera camera)
    {
        this.camera = camera;
    }

    public void setSkybox(final Skybox skybox)
    {
        this.skybox = skybox;
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
        else
        {
            throw new IllegalArgumentException("Unknown light type: " + light.toString());
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
