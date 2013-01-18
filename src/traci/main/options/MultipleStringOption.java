package traci.main.options;

import org.apache.commons.cli.CommandLine;

import traci.main.Result;

@SuppressWarnings("serial")
public abstract class MultipleStringOption extends TraciOption
{
    private final String[] defaultValues;

    public MultipleStringOption(final Character shortOpt, final String longOpt, final String desc,
            final String argName, final String[] defaultValues)
    {
        super(shortOpt, longOpt, desc, argName);

        if (defaultValues == null)
        {
            this.defaultValues = new String[0];
        }
        else
        {
            this.defaultValues = defaultValues;
        }
    }

    @Override
    public Result checkOption(final CommandLine cmd)
    {
        if (cmd.hasOption(optName))
        {
            final String[] values = cmd.getOptionValues(optName);
            return handleOption(values, true);
        }

        return handleOption(defaultValues, false);
    }

    public abstract Result handleOption(final String[] values, final boolean userSupplied);
}
