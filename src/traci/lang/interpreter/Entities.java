package traci.lang.interpreter;

import traci.math.Transformable;
import traci.math.Transformation;
import traci.model.material.Color;
import traci.model.shape.BoundingBox;
import traci.model.shape.Shape;
import traci.model.shape.csg.Csg;
import traci.model.shape.primitive.Primitive;

public class Entities
{
    public static final Entity NULL_ENTITY = new Entity()
    {
        public void applyValue(final TraciValue value) { }
    };
    
    public static Entity makeEntity(final Object object)
    {
        final EntityHelper entity;
        
        if (object instanceof Csg)
        {
            entity = new CsgEntity();
        }
        else if (object instanceof Primitive)
        {
            entity = new ShapeEntity();
        }
        else if (object instanceof BoundingBox)
        {
            entity = new TransformableEntity();
        }
        else
        {
            return null;
        }
        
        entity.object = object;
        return entity;
    }
    
    private static class EntityHelper implements Entity
    {
        protected Object object = null;
        
        public void applyValue(final TraciValue value)
        {
            switch (value.getType())
            {
            case TRANSFORMATION:
                apply(value.getTransformation());
                break;
                
            case CSG_SHAPE:
                apply((Shape) value.getCsg());
                break;
                
            case PRIMITIVE_SHAPE:
                apply((Shape) value.getPrimitive());
                break;
                
            case BOUNDING_BOX:
                apply(value.getBoundingBox());
                break;
                
            case COLOR:
                apply(value.getColor());
                break;
            }
        }
        
        protected void apply(final Transformation transformation) { }
        protected void apply(final Shape shape) { }
        protected void apply(final BoundingBox bBox) { }
        protected void apply(final Color color) { }
    }
    
    private static class TransformableEntity extends EntityHelper
    {
        @Override
        protected void apply(final Transformation transformation)
        {
            ((Transformable) object).transform(transformation);
        }
    }
    
    private static class ShapeEntity extends TransformableEntity
    {
        @Override
        protected void apply(final Color color)
        {
            ((Shape) object).setColor(color);
        }
    }
    
    private static class CsgEntity extends ShapeEntity
    {
        @Override
        protected void apply(final Shape shape)
        {
            ((Csg) object).add(shape);
        }
        
        @Override
        protected void apply(final BoundingBox bBox)
        {
            ((Csg) object).setBoundingBox(bBox);
        }
    }
}
