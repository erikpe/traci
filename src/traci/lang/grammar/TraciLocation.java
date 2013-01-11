package traci.lang.grammar;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

public class TraciLocation
{
    public static class FileLocation
    {
        public final String filename;
        public final int row;
        public final int col;

        public FileLocation(final String filename, final int row, final int col)
        {
            this.filename = unquote(filename);
            this.row = row;
            this.col = col;
        }

        private static String unquote(String str)
        {
            if (str == null)
            {
                return null;
            }
            str = str.substring(1, str.length() - 1);
            str = str.replace("\\\\", "\\");
            return str;
        }
    }

    public final FileLocation fileLocation;
    public final List<FileLocation> includePath;

    public TraciLocation(final FileLocation fileLocation, final List<FileLocation> includePath)
    {
        this.fileLocation = fileLocation;
        this.includePath = Collections.unmodifiableList(new ArrayList<FileLocation>(includePath));
    }

    @Override
    public String toString()
    {
        final StringBuilder sb = new StringBuilder();

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

            if (i < includePath.size() - 1)
            {
                sb.append(",\n");
            }
            else
            {
                sb.append(":\n");
            }
        }

        sb.append(fileLocation.filename).append(':');
        sb.append(fileLocation.row).append(':');
        sb.append(fileLocation.col).append(':');

        return sb.toString();
    }
}
