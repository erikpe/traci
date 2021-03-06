package se.ejp.traci.model.material.pigment;

import se.ejp.traci.model.Color;

public enum Interpolator
{
    NEAREST_NEIGHBOUR
    {
        @Override
        public Color interpolate(final Interpolatable image, final double x, final double y)
        {
            return image.getAt(Math.round(x), Math.round(y));
        }
    },

    BI_LINEAR
    {
        @Override
        public Color interpolate(final Interpolatable image, final double x, final double y)
        {
            final long xFloor = (long) Math.floor(x);
            final long yFloor = (long) Math.floor(y);

            final long xFloorPlus1 = Math.min(xFloor + 1, image.getWidth() - 1);
            final long yFloorPlus1 = Math.min(yFloor + 1, image.getHeight() - 1);

            Color SW = image.getAt(xFloor, yFloor);
            Color NW = image.getAt(xFloor, yFloorPlus1);
            Color SE = image.getAt(xFloorPlus1, yFloor);
            Color NE = image.getAt(xFloorPlus1, yFloorPlus1);

            final double SWCoeff = (1.0 - (x - xFloor)) * (1.0 - (y - yFloor));
            final double NWCoeff = (1.0 - (x - xFloor)) * (y - yFloor);
            final double SECoeff = (x - xFloor) * (1.0 - (y - yFloor));
            final double NECoeff = (x - xFloor) * (y - yFloor);

            SW = SW.mul(SWCoeff);
            NW = NW.mul(NWCoeff);
            SE = SE.mul(SECoeff);
            NE = NE.mul(NECoeff);

            return SW.add(NW).add(SE).add(NE);
        }
    },

    BI_CUBIC_SPLINE
    {
        @Override
        public Color interpolate(final Interpolatable image, final double x, final double y)
        {
            throw new UnsupportedOperationException("Bicubic spline interpolator not yet implemented");
        }
    };

    public abstract Color interpolate(final Interpolatable image, final double x, final double y);
}
