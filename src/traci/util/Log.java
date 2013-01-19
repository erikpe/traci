package traci.util;

import java.io.PrintStream;

public class Log
{
    public enum LogLevel
    {
        ERROR  ("[ERROR]   ", "          ", System.err),
        WARNING("[WARNING] ", "          ", System.err),
        INFO   ("[INFO]    ", "          ", System.out),
        DEBUG  ("[DEBUG]   ", "          ", System.out);

        private final String prefixOne;
        private final String prefixTwo;
        private final PrintStream printStream;

        private LogLevel(final String prefixOne, final String prefixTwo, final PrintStream printStream)
        {
            this.prefixOne = prefixOne;
            this.prefixTwo = prefixTwo;
            this.printStream = printStream;

        }
    }

    private static LogLevel logLevel = LogLevel.INFO;

    private static void handleMessage(final LogLevel level, final String msg)
    {
        if (logLevel.compareTo(level) >= 0)
        {
            final String[] lines = msg.split("\n");

            for (int i = 0; i < lines.length; ++i)
            {
                final String line;
                if (i == 0)
                {
                    line = level.prefixOne + lines[i];
                }
                else
                {
                    line = level.prefixTwo + lines[i];
                }

                level.printStream.println(line);
            }
        }
    }

    public static void ERROR(final String msg)
    {
        handleMessage(LogLevel.ERROR, msg);
    }

    public static void WARNING(final String msg)
    {
        handleMessage(LogLevel.WARNING, msg);
    }

    public static void INFO(final String msg)
    {
        handleMessage(LogLevel.INFO, msg);
    }

    public static void DEBUG(final String msg)
    {
        handleMessage(LogLevel.DEBUG, msg);
    }
}
