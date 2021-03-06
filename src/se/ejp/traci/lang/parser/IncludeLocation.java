package se.ejp.traci.lang.parser;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

public class IncludeLocation
{
    public static class FileLocation
    {
        public final String filename;
        public final int row;
        public final int col;

        public FileLocation(final String filename, final int row, final int col)
        {
            this.filename = filename;
            this.row = row;
            this.col = col;
        }

        @Override
        public String toString()
        {
            return filename + ":" + row + ":" + col;
        }
    }

    public final FileLocation fileLocation;
    public final List<FileLocation> includePath;

    public IncludeLocation(final FileLocation fileLocation, final List<FileLocation> includePath)
    {
        this.fileLocation = fileLocation;
        this.includePath = Collections.unmodifiableList(new ArrayList<FileLocation>(includePath));
    }

    public void toString(final StringBuilder sb)
    {
        for (int i = includePath.size() - 1; i >= 0; --i)
        {
            if (i == (includePath.size() - 1))
            {
                sb.append("In file included from ");
            }
            else
            {
                sb.append("                 from ");
            }

            sb.append(includePath.get(i).filename);
            sb.append(':');
            sb.append(includePath.get(i).row);

            if (i == 0)
            {
                sb.append(":\n");
            }
            else
            {
                sb.append(",\n");
            }
        }

        sb.append("In file: ");
        sb.append(fileLocation.filename).append(':');
        sb.append(fileLocation.row).append(':');
        sb.append(fileLocation.col).append(':');
    }

    @Override
    public String toString()
    {
        final StringBuilder sb = new StringBuilder();
        toString(sb);
        return sb.toString();
    }
}
