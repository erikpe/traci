package se.ejp.traci.util;

public class Pair<T1, T2> implements Cloneable
{
    public final T1 first;
    public final T2 second;

    protected Pair(final T1 first, final T2 second)
    {
        this.first = first;
        this.second = second;
    }

    public static <T1, T2> Pair<T1, T2> make(final T1 first, final T2 second)
    {
        return new Pair<T1, T2>(first, second);
    }

    @Override
    public Pair<T1, T2> clone()
    {
        return this;
    }

    @Override
    public int hashCode()
    {
        int hash = getClass().hashCode() | 0x00000001;
        hash = 31 * hash + (first == null ? 17 : first.hashCode());
        hash = 31 * hash + (second == null ? 23 : second.hashCode());
        return hash;
    }

    private boolean elemEquals(final Object o1, final Object o2)
    {
        return (o1 == null ? o2 == null : o1.equals(o2));
    }

    @Override
    public boolean equals(final Object other)
    {
        if (other == null)
        {
            return false;
        }
        else if (other == this)
        {
            return true;
        }
        else if (getClass() != other.getClass())
        {
            return false;
        }

        final Pair<?, ?> otherPair = (Pair<?, ?>) other;
        return elemEquals(first, otherPair.first) && elemEquals(second, otherPair.second);
    }

    @Override
    public String toString()
    {
        return "Pair<" + first + ", " + second + ">";
    }
}

