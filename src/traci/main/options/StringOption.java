package traci.main.options;

import org.apache.commons.cli.CommandLine;

import traci.main.Result;
import traci.util.Log;

@SuppressWarnings("serial")
public abstract class StringOption extends TraciOption
{
    private final String defaultValue;

    public StringOption(final Character shortOpt, final String longOpt, final String desc, final String argName,
            final String defaultValue)
    {
        super(shortOpt, longOpt, desc, argName);
        this.defaultValue = defaultValue;
    }

    @Override
    public Result checkOption(final CommandLine cmd)
    {
        if (cmd.hasOption(optName))
        {
            final String[] values = cmd.getOptionValues(optName);

            if (values.length > 1)
            {
                Log.ERROR("Option '" + getBothNames() + "' used more than once\n" + Options.getHelp());
                return Result.INVALID_ARGUMENT_ERROR;
            }

            return handleOption(values[0], true);
        }
        else if (defaultValue != null)
        {
            return handleOption(defaultValue, false);
        }

        return Result.SUCCESS;
    }

    public abstract Result handleOption(final String value, final boolean userSupplied);
}
