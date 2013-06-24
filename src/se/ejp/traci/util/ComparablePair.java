package se.ejp.traci.util;

public class ComparablePair<T1 extends Comparable<T1>, T2> extends Pair<T1, T2> implements
        Comparable<ComparablePair<? extends T1, ?>>
{
    private ComparablePair(final T1 first, final T2 second)
    {
        super(first, second);
    }

    public static <T1 extends Comparable<T1>, T2> ComparablePair<T1, T2> make(final T1 first, final T2 second)
    {
        return new ComparablePair<T1, T2>(first, second);
    }

    @Override
    public int compareTo(final ComparablePair<? extends T1, ?> other)
    {
        return first.compareTo(other.first);
    }
}
