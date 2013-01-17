package traci.main.options;

import org.apache.commons.cli.CommandLine;

import traci.main.Result;

@SuppressWarnings("serial")
public abstract class StringOption extends TraciOption
{
    public StringOption(final Character shortOpt, final String longOpt, final String desc, final String argName)
    {
        super(shortOpt, longOpt, desc, argName);
    }

    @Override
    public Result handleOption(final CommandLine cmd)
    {
        return handleStringOption(cmd.getOptionValue(optName));
    }

    public abstract Result handleStringOption(final String value);
}
