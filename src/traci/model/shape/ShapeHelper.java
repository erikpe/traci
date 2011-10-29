package traci.model.shape;

import traci.model.shape.csg.Csg;
import traci.model.shape.primitive.Primitive;

public class ShapeHelper
{
    public static int numPrimitives(final Shape shape)
    {
        if (shape instanceof Primitive)
        {
            return 1;
        }
        else if (shape instanceof Csg)
        {
            int numPrimitives = 0;

            for (final Shape subShape : ((Csg) shape))
            {
                numPrimitives += numPrimitives(subShape);
            }

            return numPrimitives;
        }

        return 0;
    }

    public static int numCsgs(final Shape shape)
    {
        if (shape instanceof Primitive)
        {
            return 0;
        }
        else if (shape instanceof Csg)
        {
            int numCsgs = 1;

            for (final Shape subShape : ((Csg) shape))
            {
                numCsgs += numCsgs(subShape);
            }

            return numCsgs;
        }

        return 0;
    }

    public static int numBBoxes(final Shape shape)
    {
        if (shape instanceof Primitive)
        {
            return 0;
        }
        else if (shape instanceof Csg)
        {
            final Csg csg = (Csg) shape;
            int numBBoxes = csg.getBoundingBox() == null ? 0 : 1;

            for (final Shape subShape : csg)
            {
                numBBoxes += numBBoxes(subShape);
            }

            return numBBoxes;
        }

        return 0;
    }
}
