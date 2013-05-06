package se.ejp.traci.main.options;

import org.apache.commons.cli.CommandLine;

import se.ejp.traci.main.Result;

@SuppressWarnings("serial")
public abstract class FlagOption extends TraciOption
{
    public FlagOption(final Character shortOpt, final String longOpt, final String desc)
    {
        super(shortOpt, longOpt, desc, null);
    }

    @Override
    public Result checkOption(final CommandLine cmd)
    {
        return handleOption(cmd.hasOption(optName));
    }

    public abstract Result handleOption(final boolean flagSet);
}
