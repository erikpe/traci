package traci.main;

import gnu.getopt.Getopt;
import gnu.getopt.LongOpt;

import java.util.ArrayList;
import java.util.List;

public class Options
{
    private static final int VAL_FOV = 1000;
    private static final int VAL_FOCAL_BLUR_SAMPLES = 1001;
    private static final int VAL_WORKBLOCK_WIDTH = 1002;
    private static final int VAL_WORKBLOCK_HEIGHT = 1003;
    private static final int VAL_DEBUG = 1004;
    private static final int VAL_THREADS = 1005;

    final String opts;
    final LongOpt[] longOpts;
    final StringBuffer sb = new StringBuffer();

    public Options()
    {
        final List<LongOpt> longOptsList = new ArrayList<LongOpt>();
        longOptsList.add(new LongOpt("width", LongOpt.REQUIRED_ARGUMENT, null, 'w'));
        longOptsList.add(new LongOpt("height", LongOpt.REQUIRED_ARGUMENT, null, 'h'));
        longOptsList.add(new LongOpt("aa-level", LongOpt.REQUIRED_ARGUMENT, null, 'a'));
        longOptsList.add(new LongOpt("output", LongOpt.REQUIRED_ARGUMENT, null, 'o'));
        longOptsList.add(new LongOpt("display", LongOpt.NO_ARGUMENT, null, 'd'));
        longOptsList.add(new LongOpt("fov", LongOpt.REQUIRED_ARGUMENT, sb, VAL_FOV));
        longOptsList.add(new LongOpt("focal-blur-samples", LongOpt.REQUIRED_ARGUMENT, sb, VAL_FOCAL_BLUR_SAMPLES));
        longOptsList.add(new LongOpt("workblock-width", LongOpt.REQUIRED_ARGUMENT, sb, VAL_WORKBLOCK_WIDTH));
        longOptsList.add(new LongOpt("workblock-height", LongOpt.REQUIRED_ARGUMENT, sb, VAL_WORKBLOCK_HEIGHT));
        longOptsList.add(new LongOpt("debug", LongOpt.NO_ARGUMENT, sb, VAL_DEBUG));
        longOptsList.add(new LongOpt("threads", LongOpt.REQUIRED_ARGUMENT, sb, VAL_THREADS));
        longOpts = longOptsList.toArray(new LongOpt[longOptsList.size()]);
        opts = ":w:h:a:o:d";
    }

    private static void parseLongOpt(final int opt, final String arg, final Settings settings)
    {
        switch (opt)
        {
        case VAL_FOV:
            settings.fov = Integer.parseInt(arg);
            break;

        case VAL_FOCAL_BLUR_SAMPLES:
            settings.focalBlurSamples = Integer.parseInt(arg);
            settings.focalBlurEnabled = true;
            break;

        case VAL_WORKBLOCK_WIDTH:
            settings.workBlockWidth = Integer.parseInt(arg);
            break;

        case VAL_WORKBLOCK_HEIGHT:
            settings.workBlockHeight = Integer.parseInt(arg);
            break;

        case VAL_DEBUG:
            settings.debug = true;
            break;

        case VAL_THREADS:
            settings.numThreads = Integer.parseInt(arg);
            break;
        }
    }

    public Settings parse(final String[] argv)
    {
        final Getopt g = new Getopt("traci", argv, opts, longOpts);
        final Settings settings = Settings.getDefault();

        int c;
        while ((c = g.getopt()) != -1)
        {
            switch (c)
            {
            case 0:
                parseLongOpt(Integer.parseInt(sb.toString()), g.getOptarg(), settings);
                break;

            case 'w':
                settings.width = Integer.parseInt(g.getOptarg());
                break;

            case 'h':
                settings.height = Integer.parseInt(g.getOptarg());
                break;

            case 'a':
                settings.aaLevel = Integer.parseInt(g.getOptarg());
                settings.antialiasEnabled = true;
                break;

            case 'o':
                settings.outputFilename = g.getOptarg();
                break;

            case 'd':
                settings.display = true;
                break;
            }
        }

        if (g.getOptind() < argv.length)
        {
            settings.inputFilename = argv[g.getOptind()];
        }

        return settings;
    }
}
