package se.ejp.traci.model.material.pigment;

import java.util.HashMap;
import java.util.Map;
import java.util.Set;

public enum RepeatPolicy
{
    REPEAT("repeat"),
    BORDER("border"),
    STRETCH("stretch");

    private final String id;

    private static final Map<String, RepeatPolicy> idMap = new HashMap<String, RepeatPolicy>();
    static
    {
        for (final RepeatPolicy repeatPolicy : RepeatPolicy.values())
        {
            idMap.put(repeatPolicy.id, repeatPolicy);
        }
    }

    private RepeatPolicy(final String id)
    {
        this.id = id;
    }

    public static RepeatPolicy get(final String id)
    {
        return idMap.get(id);
    }

    public static Set<String> getAllPolicies()
    {
        return idMap.keySet();
    }
}
