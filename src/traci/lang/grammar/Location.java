package traci.lang.grammar;

public class Location
{
    public final String filename;
    public final int row;
    public final int col;

    public Location(final String filename, final int row, final int col)
    {
        this.filename = TraciToken.unquote(filename);
        this.row = row;
        this.col = col;
    }

    @Override
    public String toString()
    {
        return filename + ":" + row + ":" + col;
    }
}
