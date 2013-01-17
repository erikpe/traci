package traci.main.options;

import org.apache.commons.cli.CommandLine;
import org.apache.commons.cli.CommandLineParser;
import org.apache.commons.cli.HelpFormatter;
import org.apache.commons.cli.MissingArgumentException;
import org.apache.commons.cli.ParseException;
import org.apache.commons.cli.PosixParser;
import org.apache.commons.cli.UnrecognizedOptionException;

import traci.main.Result;
import traci.main.Settings;
import traci.util.Log;

public class Options
{
    private static final String PROGNAME = "traci";
    private final org.apache.commons.cli.Options allOptions;
    private Settings settings;

    public Options()
    {
        this.allOptions = new org.apache.commons.cli.Options();
        initialize();
    }

    public Settings getSettings()
    {
        return settings;
    }

    public Result parse(final String[] argv)
    {
        settings = Settings.getDefault();

        final CommandLineParser parser = new PosixParser();
        final CommandLine cmd;

        try
        {
            cmd = parser.parse(allOptions, argv);
        }
        catch (final MissingArgumentException e)
        {
            final TraciOption option = (TraciOption) e.getOption();
            Log.ERROR("Missing argument for option '" + option.getBothNames() + "'\n" + getHelp());
            return Result.INVALID_ARGUMENT_ERROR;
        }
        catch (final UnrecognizedOptionException e)
        {
            Log.ERROR("Unrecognized option '" + e.getOption() + "'\n" + getHelp());
            return Result.INVALID_ARGUMENT_ERROR;
        }
        catch (final ParseException e)
        {
            Log.ERROR("Option error: " + e.getMessage() + "\n" + getHelp());
            return Result.INVALID_ARGUMENT_ERROR;
        }

        return readCmdLine(cmd, settings);
    }

    private Result readCmdLine(final CommandLine cmd, final Settings settings)
    {
        for (final Object o : allOptions.getOptions())
        {
            final TraciOption option = (TraciOption) o;
            final Result result = option.checkOption(cmd);

            if (result != Result.SUCCESS)
            {
                return result;
            }
        }

        final String[] args = cmd.getArgs();

        if (args.length == 0)
        {
            Log.ERROR("No input file given\n" + getHelp());
            return Result.INVALID_ARGUMENT_ERROR;
        }
        else if (args.length > 1)
        {
            Log.ERROR("More than one input file specified\n" + getHelp());
            return Result.INVALID_ARGUMENT_ERROR;
        }

        settings.inputFilename = args[0];

        return Result.SUCCESS;
    }

    private void addOption(final TraciOption option)
    {
        allOptions.addOption(option);
    }

    @SuppressWarnings("serial")
    private void initialize()
    {
        addOption(new IntOption('w', "width", "image width in pixels", "SIZE")
        {
            @Override
            public Result handleIntOption(final long value)
            {
                settings.width = (int) value;
                return Result.SUCCESS;
            }
        });

        addOption(new IntOption('h', "height", "image height in pixels", "SIZE")
        {
            @Override
            public Result handleIntOption(final long value)
            {
                settings.height = (int) value;
                return Result.SUCCESS;
            }
        });

        addOption(new IntOption('a', "aa-level", "level of antialiasing", "LEVEL")
        {
            @Override
            public Result handleIntOption(final long value)
            {
                settings.aaLevel = (int) value;
                return Result.SUCCESS;
            }
        });

        addOption(new StringOption('o', "output", "output file", "FILE")
        {
            @Override
            public Result handleStringOption(final String value)
            {
                settings.outputFilename = value;
                return Result.SUCCESS;
            }
        });

        addOption(new FlagOption('d', "display", "display image")
        {
            @Override
            public Result handleFlagOption()
            {
                settings.display = true;
                return Result.SUCCESS;
            }
        });

        addOption(new FloatOption(null, "fov", "field of view in degrees", "DEGREES")
        {
            @Override
            public Result handleFloatOption(final double value)
            {
                settings.fov = (int) value;
                return Result.SUCCESS;
            }
        });

        addOption(new IntOption(null, "focal-blur-samples", "number of samples per pixel for focal blur", "NUM")
        {
            @Override
            public Result handleIntOption(final long value)
            {
                settings.focalBlurSamples = (int) value;
                return Result.SUCCESS;
            }
        });

        addOption(new IntOption(null, "workblock-width", "workblock width in pixels", "SIZE")
        {
            @Override
            public Result handleIntOption(final long value)
            {
                settings.workBlockWidth = (int) value;
                return Result.SUCCESS;
            }
        });

        addOption(new IntOption(null, "workblock-height", "workblock height in pixels", "SIZE")
        {
            @Override
            public Result handleIntOption(final long value)
            {
                settings.workBlockHeight = (int) value;
                return Result.SUCCESS;
            }
        });

        addOption(new FlagOption(null, "debug", "enable debug messages")
        {
            @Override
            public Result handleFlagOption()
            {
                settings.debug = true;
                return Result.SUCCESS;
            }
        });

        addOption(new IntOption(null, "threads", "number of worker threads", "NUM")
        {
            @Override
            public Result handleIntOption(final long value)
            {
                settings.numThreads = (int) value;
                return Result.SUCCESS;
            }
        });

        addOption(new FlagOption(null, "help", "show help")
        {
            @Override
            public Result handleFlagOption()
            {
                final HelpFormatter formatter = new HelpFormatter();
                formatter.printHelp("traci [options] <input file>", allOptions);
                return Result.ABORT;
            }
        });

        final TraciOption macroOption = new TraciOption('D', null, "define macro for preprocessor", "NAME=VALUE")
        {
            @Override
            public Result handleOption(final CommandLine cmd)
            {
                // final String[] options = cmd.getOptionValues(optName);
                return Result.SUCCESS;
            }
        };
        macroOption.setArgs(2);
        macroOption.setValueSeparator('=');
        addOption(macroOption);
    }

    static String getHelp()
    {
        return "run '" + PROGNAME + " --help' for help";
    }
}
