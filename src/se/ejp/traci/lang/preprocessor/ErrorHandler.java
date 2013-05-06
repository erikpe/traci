package se.ejp.traci.lang.preprocessor;

import org.anarres.cpp.FileLexerSource;
import org.anarres.cpp.PreprocessorListener;
import org.anarres.cpp.Source;

import se.ejp.traci.util.Log;

public class ErrorHandler extends PreprocessorListener
{
    private int errors;
    private int warnings;

    public ErrorHandler()
    {
        this.errors = 0;
        this.warnings = 0;
    }

    @Override
    public int getErrors()
    {
        return errors;
    }

    @Override
    public int getWarnings()
    {
        return warnings;
    }

    private static void makeIncludePath(final StringBuilder sb, final FileLexerSource source, final boolean last)
    {
        final FileLexerSource parent = (FileLexerSource) source.getParent();

        if (parent == null)
        {
            sb.append("In file included from ");
            sb.append(source.getPath()).append(':');
            sb.append(source.getLine() - 1);
        }
        else
        {
            makeIncludePath(sb, parent, false);
            sb.append("                 from ");
            sb.append(source.getPath()).append(':');
            sb.append(source.getLine() - 1);
        }

        if (last)
        {
            sb.append(":\n");
        }
        else
        {
            sb.append(",\n");
        }
    }

    private static String makeIncludePath(final StringBuilder sb, final FileLexerSource source, final int line,
            final int column)
    {
        final FileLexerSource parent = (FileLexerSource) source.getParent();

        if (parent != null)
        {
            makeIncludePath(sb, parent, true);
        }

        sb.append("In file: ");
        sb.append(source.getPath()).append(':');
        sb.append(line).append(':');
        sb.append(column).append(':');

        return sb.toString();
    }

    @Override
    public void handleWarning(final Source source, final int line, final int column, final String msg)
    {
        warnings++;

        final StringBuilder sb = new StringBuilder();

        makeIncludePath(sb, (FileLexerSource) source, line, column);
        sb.append("\nPreprocessor warning: ");
        sb.append(msg);

        Log.WARNING(sb.toString());
    }

    @Override
    public void handleError(final Source source, final int line, final int column, final String msg)
    {
        errors++;

        final StringBuilder sb = new StringBuilder();

        makeIncludePath(sb, (FileLexerSource) source, line, column);
        sb.append("\nPreprocessor error: ");
        sb.append(msg);

        Log.ERROR(sb.toString());
    }
}
