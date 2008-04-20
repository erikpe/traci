package traci.parser;

import java.util.AbstractList;
import java.util.ArrayList;
import java.util.List;

public class Node extends AbstractList<Node> implements List<Node>
{
    public final String id;
    
    public final boolean isLeaf;
    
    private final List<Node> children;
    
    public Node(final String id, final boolean isLeaf)
    {
        this.id = id;
        this.isLeaf = isLeaf;
        
        if (isLeaf)
        {
            this.children = new ArrayList<Node>();
        }
        else
        {
            this.children = null;
        }
    }
    
    @Override
    public boolean add(final Node child)
    {
        assert !isLeaf;
        return children.add(child);
    }
    
    @Override
    public Node get(int index)
    {
        assert !isLeaf;
        return children.get(index);
    }
    
    @Override
    public int size()
    {
        assert !isLeaf;
        return children.size();
    }
}
