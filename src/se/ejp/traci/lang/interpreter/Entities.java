package se.ejp.traci.lang.interpreter;

import se.ejp.traci.lang.interpreter.exceptions.InterpreterInternalException;
import se.ejp.traci.math.Transformation;
import se.ejp.traci.model.light.Light;
import se.ejp.traci.model.material.Material;
import se.ejp.traci.model.material.Texture;
import se.ejp.traci.model.material.pigment.Pigment;
import se.ejp.traci.model.material.pigment.Solid;
import se.ejp.traci.model.shape.BoundingBox;
import se.ejp.traci.model.shape.csg.Csg;
import se.ejp.traci.model.shape.primitive.Primitive;

public class Entities
{
    public interface Entity
    {
        public void applyValue(final TraciValue value);

        public TraciValue getValue();
    }

    public static final Entity NULL_ENTITY = new Entity()
    {
        @Override
        public void applyValue(final TraciValue value) { }

        @Override
        public TraciValue getValue()
        {
            throw new InterpreterInternalException("getValue() called on null entity");
        }

        @Override
        public String toString()
        {
            return "<ENTITY:null>";
        }
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
        else if (object instanceof Pigment)
        {
            return new PigmentEntity((Pigment) object);
        }
        else if (object instanceof Light)
        {
            return new LightEntity((Light) object);
        }

        throw new RuntimeException();
    }

    private static abstract class EntityHelper<T> implements Entity
    {
        protected T obj;

        private EntityHelper(final T obj)
        {
            this.obj = obj;
        }

        @Override
        public TraciValue getValue()
        {
            return new TraciValue(obj);
        }

        @Override
        public String toString()
        {
            return "<ENTITY:" + obj.toString() + ">";
        }
    }

    private static class PrimitiveEntity extends EntityHelper<Primitive>
    {
        private PrimitiveEntity(final Primitive primitive)
        {
            super(primitive);
        }

        @Override
        public void applyValue(final TraciValue value)
        {
            switch (value.getType())
            {
            case MATERIAL:
                obj.setMaterial(value.getMaterial());
                break;

            case TEXTURE:
                obj.setTexture(value.getTexture());
                break;

            case FINISH:
                obj.setFinish(value.getFinish());
                break;

            case PIGMENT:
                obj.setPigment(value.getPigment());
                break;

            case TRANSFORMATION:
                obj.transform(value.getTransformation());
                break;

            case COLOR:
                obj.setPigment(Solid.make(value.getColor()));
                break;

            default:
                throw new RuntimeException();
            }
        }
    }

    private static class CsgEntity extends EntityHelper<Csg>
    {
        private CsgEntity(final Csg csg)
        {
            super(csg);
        }

        @Override
        public void applyValue(final TraciValue value)
        {
            switch (value.getType())
            {
            case PRIMITIVE_SHAPE:
            case CSG_SHAPE:
                obj.add(value.getShape());
                break;

            case BOUNDING_BOX:
                obj.setBoundingBox(value.getBoundingBox());
                break;

            case MATERIAL:
                obj.setMaterial(value.getMaterial());
                break;

            case TEXTURE:
                obj.setTexture(value.getTexture());
                break;

            case FINISH:
                obj.setFinish(value.getFinish());
                break;

            case PIGMENT:
                obj.setPigment(value.getPigment());
                break;

            case TRANSFORMATION:
                obj.transform(value.getTransformation());
                break;

            case COLOR:
                obj.setPigment(Solid.make(value.getColor()));
                break;

            default:
                throw new RuntimeException();
            }
        }
    }

    private static class TransformationEntity extends EntityHelper<Transformation>
    {
        private TransformationEntity(final Transformation transformation)
        {
            super(transformation);
        }

        @Override
        public void applyValue(final TraciValue value)
        {
            switch (value.getType())
            {
            case TRANSFORMATION:
                obj = obj.compose(value.getTransformation());
                break;

            default:
                throw new RuntimeException();
            }
        }
    }

    private static class TextureEntity extends EntityHelper<Texture>
    {
        private TextureEntity(final Texture texture)
        {
            super(texture);
        }

        @Override
        public void applyValue(final TraciValue value)
        {
            switch (value.getType())
            {
            case TRANSFORMATION:
                obj = obj.transform(value.getTransformation());
                break;

            case FINISH:
                obj = obj.setFinish(value.getFinish());
                break;

            case PIGMENT:
                obj = obj.setPigment(value.getPigment());
                break;

            case COLOR:
                obj = obj.setPigment(Solid.make(value.getColor()));
                break;

            default:
                throw new RuntimeException();
            }
        }
    }

    private static class MaterialEntity extends EntityHelper<Material>
    {
        private MaterialEntity(final Material material)
        {
            super(material);
        }

        @Override
        public void applyValue(final TraciValue value)
        {
            switch (value.getType())
            {
            case TRANSFORMATION:
                obj = obj.transform(value.getTransformation());
                break;

            case FINISH:
                obj = obj.setFinish(value.getFinish());
                break;

            case PIGMENT:
                obj = obj.setPigment(value.getPigment());
                break;

            case TEXTURE:
                obj = obj.setTexture(value.getTexture());
                break;

            case COLOR:
                obj = obj.setPigment(Solid.make(value.getColor()));

            default:
                throw new RuntimeException();
            }
        }
    }

    private static class PigmentEntity extends EntityHelper<Pigment>
    {
        private PigmentEntity(final Pigment pigment)
        {
            super(pigment);
        }

        @Override
        public void applyValue(final TraciValue value)
        {
            switch (value.getType())
            {
            case TRANSFORMATION:
                obj = obj.transform(value.getTransformation());
                break;

            default:
                throw new RuntimeException();
            }
        }
    }

    private static class BoundingBoxEntity extends EntityHelper<BoundingBox>
    {
        private BoundingBoxEntity(final BoundingBox bBox)
        {
            super(bBox);
        }

        @Override
        public void applyValue(final TraciValue value)
        {
            switch (value.getType())
            {
            case TRANSFORMATION:
                obj.transform(value.getTransformation());
                break;

            default:
                throw new RuntimeException();
            }
        }
    }

    private static class LightEntity extends EntityHelper<Light>
    {
        private LightEntity(final Light light)
        {
            super(light);
        }

        @Override
        public void applyValue(final TraciValue value)
        {
            switch (value.getType())
            {
            case TRANSFORMATION:
                obj.transform(value.getTransformation());
                break;

            default:
                throw new RuntimeException();
            }
        }
    }
}
