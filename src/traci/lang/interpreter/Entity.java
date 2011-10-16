package traci.lang.interpreter;

import traci.math.Transformations;
import traci.math.Vector;
import traci.model.material.Color;
import traci.model.shape.BoundingBox;
import traci.model.shape.Shape;
import traci.model.shape.csg.Csg;
import traci.model.shape.csg.Union;
import traci.model.shape.primitive.Primitive;

public interface Entity
{
    public static final Entity DUMMY_ENTITY = new Entity()
    {
        public void applyValue(final TraciValue value) { }
    };
    
    public class SceneEntity implements Entity
    {
        public final Union rootUnion;
        
        public SceneEntity()
        {
            rootUnion = new Union();
        }
        
        public void applyValue(final TraciValue value)
        {
            switch (value.getType())
            {
            case PRIMITIVE_SHAPE:
            case CSG_SHAPE:
                rootUnion.add(value.getShape());
                break;
                
            default:
                break;
            }
        }
    }
    
    public class CsgEntity implements Entity
    {
        private final Csg csg;
        
        public CsgEntity(final Csg csg)
        {
            this.csg = csg;
        }
        
        public void applyValue(final TraciValue value)
        {
            switch (value.getType())
            {
            case MODIFIER:
                applyModifier(csg, value.getModifierValue());
                break;
                
            case PRIMITIVE_SHAPE:
            case CSG_SHAPE:
                csg.add(value.getShape());
                break;
                
            default:
                break;
            }
        }
        
        private void applyModifier(final Shape shape, final ModifierValue modifier)
        {
            switch (modifier.type)
            {
            case ROTX:
                shape.transform(Transformations.rotx(modifier.getNumber()));
                break;
                
            case ROTY:
                shape.transform(Transformations.roty(modifier.getNumber()));
                break;
                
            case ROTZ:
                shape.transform(Transformations.roty(modifier.getNumber()));
                break;
                
            case TRANSLATE:
                shape.transform(Transformations.translate(modifier.getVector()));
                break;
                
            case SCALE:
            {
                final Object value = modifier.getValue();
                if (value instanceof Vector)
                {
                    shape.transform(Transformations.scale((Vector) value));
                }
                else
                {
                    shape.transform(Transformations.scale((Double) value));
                }
                break;
            }
            
            case COLOR:
                shape.setColor(Color.make(modifier.getVector()));
                break;
                
            }
        }
    }
    
    public class PrimitiveEntity implements Entity
    {
        private final Primitive primitive;
        
        public PrimitiveEntity(final Primitive primitive)
        {
            this.primitive = primitive;
        }
        
        public void applyValue(final TraciValue value)
        {
            switch (value.getType())
            {
            case MODIFIER:
                applyModifier(primitive, value.getModifierValue());
                break;
                
            default: break;
            }
        }
        
        private void applyModifier(final Shape shape, final ModifierValue modifier)
        {
            switch (modifier.type)
            {
            case ROTX:
                shape.transform(Transformations.rotx(modifier.getNumber()));
                break;
                
            case ROTY:
                shape.transform(Transformations.roty(modifier.getNumber()));
                break;
                
            case ROTZ:
                shape.transform(Transformations.roty(modifier.getNumber()));
                break;
                
            case TRANSLATE:
                shape.transform(Transformations.translate(modifier.getVector()));
                break;
                
            case SCALE:
            {
                final Object value = modifier.getValue();
                if (value instanceof Vector)
                {
                    shape.transform(Transformations.scale((Vector) value));
                }
                else
                {
                    shape.transform(Transformations.scale((Double) value));
                }
                break;
            }
            
            case COLOR:
                shape.setColor(Color.make(modifier.getVector()));
                break;
                
            }
        }
    }
    
    public class BBoxEntity
    {
        private final BoundingBox bBox;
        
        public BBoxEntity(final BoundingBox bBox)
        {
            this.bBox = bBox;
        }
        
        public void applyValue(final TraciValue value)
        {
            switch (value.getType())
            {
            case MODIFIER:
                applyModifier(bBox, value.getModifierValue());
                break;
                
            default: break;
            }
        }
        
        private void applyModifier(final BoundingBox bBox, final ModifierValue modifier)
        {
            switch (modifier.type)
            {
            case ROTX:
                bBox.transform(Transformations.rotx(modifier.getNumber()));
                break;
                
            case ROTY:
                bBox.transform(Transformations.roty(modifier.getNumber()));
                break;
                
            case ROTZ:
                bBox.transform(Transformations.rotz(modifier.getNumber()));
                break;
                
            case TRANSLATE:
                bBox.transform(Transformations.translate(modifier.getVector()));
                break;
                
            case SCALE:
            {
                final Object value = modifier.getValue();
                if (value instanceof Vector)
                {
                    bBox.transform(Transformations.scale(modifier.getVector()));
                }
                else
                {
                    bBox.transform(Transformations.scale(modifier.getNumber()));
                }
                break;
            }
            
            default: break;
            }
        }
    }
    
    public void applyValue(final TraciValue value);
}
