package se.ejp.traci.model.light;

import se.ejp.traci.math.Transformable;

public abstract class Light implements Cloneable, Transformable
{
    @Override
    public Light clone() throws CloneNotSupportedException
    {
        return (Light) super.clone();
    }
}
