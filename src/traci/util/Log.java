package traci.util;

public class Log
{
    public enum LogLevel
    {
        ERROR,
        WARNING,
        INFO,
        DEBUG
    }
    private static LogLevel logLevel = LogLevel.INFO;

    public static void ERROR(final String msg)
    {
        if (logLevel.compareTo(LogLevel.ERROR) >= 0)
        {
            final String[] lines = msg.split("\n");
            for (int i = 0; i < lines.length; ++i)
            {
                if (i == 0)
                {
                    System.err.println("[ERROR] " + lines[i]);
                }
                else
                {
                    System.err.println("        " + lines[i]);
                }
            }
        }
    }

    public static void WARNING(final String msg)
    {
        if (logLevel.compareTo(LogLevel.WARNING) >= 0)
        {
            System.out.println("[WARNING] " + msg);
        }
    }

    public static void INFO(final String msg)
    {
        if (logLevel.compareTo(LogLevel.INFO) >= 0)
        {
            System.out.println("[INFO] " + msg);
        }
    }

    public static void DEBUG(final String msg)
    {
        if (logLevel.compareTo(LogLevel.DEBUG) >= 0)
        {
            System.out.println("[DEBUG] " + msg);
        }
    }
}
