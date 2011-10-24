package traci.lang.interpreter;

import traci.math.Transformation;
import traci.model.material.Material;
import traci.model.material.Texture;
import traci.model.shape.BoundingBox;
import traci.model.shape.csg.Csg;
import traci.model.shape.primitive.Primitive;

public class Entities
{
    public interface Entity
    {
        public void applyValue(final TraciValue value);
    }
    
    public static final Entity NULL_ENTITY = new Entity()
    {
        public void applyValue(final TraciValue value) { }
    };
    
    public static Entity makeEntity(final Object object)
    {
        if (object instanceof Csg)
        {
            return new CsgEntity((Csg) object);
        }
        else if (object instanceof Primitive)
        {
            return new PrimitiveEntity((Primitive) object);
        }
        else if (object instanceof BoundingBox)
        {
            return new BoundingBoxEntity((BoundingBox) object);
        }
        else if (object instanceof Transformation)
        {
            return new TransformationEntity((Transformation) object);
        }
        else if (object instanceof Material)
        {
            return new MaterialEntity((Material) object);
        }
        else if (object instanceof Texture)
        {
            return new TextureEntity((Texture) object);
        }
        
        throw new RuntimeException();
    }
    
    private static class PrimitiveEntity implements Entity
    {
        private final Primitive primitive;
        
        private PrimitiveEntity(final Primitive primitive)
        {
            this.primitive = primitive;
        }
        
        public void applyValue(final TraciValue value)
        {
            switch (value.getType())
            {
            case MATERIAL:
                primitive.setMaterial(value.getMaterial());
                break;
                
            case TEXTURE:
                primitive.setTexture(value.getTexture());
                break;
                
            case FINISH:
                primitive.setFinish(value.getFinish());
                break;
                
            case PIGMENT:
                primitive.setPigment(value.getPigment());
                break;
                
            case TRANSFORMATION:
                primitive.transform(value.getTransformation());
                break;
                
            default:
                throw new RuntimeException();
            }
        }
    }
    
    private static class CsgEntity implements Entity
    {
        private final Csg csg;
        
        private CsgEntity(final Csg csg)
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
                
            case BOUNDING_BOX:
                csg.setBoundingBox(value.getBoundingBox());
                break;
                
            case MATERIAL:
                csg.setMaterial(value.getMaterial());
                break;
                
            case TEXTURE:
                csg.setTexture(value.getTexture());
                break;
                
            case FINISH:
                csg.setFinish(value.getFinish());
                break;
                
            case PIGMENT:
                csg.setPigment(value.getPigment());
                break;
                
            case TRANSFORMATION:
                csg.transform(value.getTransformation());
                break;
                
            default:
                throw new RuntimeException();
            }
        }
    }
    
    private static class TransformationEntity implements Entity
    {
        private Transformation transformation;
        
        private TransformationEntity(final Transformation transformation)
        {
            this.transformation = transformation;
        }
        
        public void applyValue(final TraciValue value)
        {
            switch (value.getType())
            {
            case TRANSFORMATION:
                transformation = transformation.compose(value.getTransformation());
                break;
                
            default:
                throw new RuntimeException();
            }
        }
    }
    
    private static class TextureEntity implements Entity
    {
        private Texture texture;
        
        private TextureEntity(final Texture texture)
        {
            this.texture = texture;
        }
        
        public void applyValue(final TraciValue value)
        {
            switch (value.getType())
            {
            case TRANSFORMATION:
                texture = texture.transform(value.getTransformation());
                break;
                
            case FINISH:
                texture = texture.setFinish(value.getFinish());
                break;
                
            case PIGMENT:
                texture = texture.setPigment(value.getPigment());
                break;
                
            default:
                throw new RuntimeException();
            }
        }
    }
    
    private static class MaterialEntity implements Entity
    {
        private Material material;
        
        private MaterialEntity(final Material material)
        {
            this.material = material;
        }
        
        public void applyValue(final TraciValue value)
        {
            switch (value.getType())
            {
            case TRANSFORMATION:
                material = material.transform(value.getTransformation());
                break;
                
            case FINISH:
                material = material.setFinish(value.getFinish());
                break;
                
            case PIGMENT:
                material = material.setPigment(value.getPigment());
                break;
                
            case TEXTURE:
                material = material.setTexture(value.getTexture());
                break;
                
            default:
                throw new RuntimeException();
            }
        }
    }
    
    private static class BoundingBoxEntity implements Entity
    {
        private final BoundingBox bbox;
        
        private BoundingBoxEntity(final BoundingBox bbox)
        {
            this.bbox = bbox;
        }
        
        public void applyValue(final TraciValue value)
        {
            switch (value.getType())
            {
            case TRANSFORMATION:
                bbox.transform(value.getTransformation());
                break;
                
            default:
                throw new RuntimeException();
            }
        }
    }
}
