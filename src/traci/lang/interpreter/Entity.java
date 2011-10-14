package traci.lang.interpreter;

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
            case PRIMITIVE_SHAPE:
            case CSG_SHAPE:
                csg.add(value.getShape());
                break;
                
            default:
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
            default: break;
            }
        }
    }
    
    public void applyValue(final TraciValue value);
}
