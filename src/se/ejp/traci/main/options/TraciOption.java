package se.ejp.traci.main.options;

import java.util.Comparator;

import org.apache.commons.cli.CommandLine;
import org.apache.commons.cli.Option;

import se.ejp.traci.main.Result;

@SuppressWarnings("serial")
public abstract class TraciOption extends Option
{
    final protected String optName;
    Integer sortingIdx = 0;

    public static final Comparator<Option> COMPARATOR = new Comparator<Option>()
    {
        @Override
        public int compare(final Option o1, final Option o2)
        {
            return ((TraciOption) o1).sortingIdx.compareTo(((TraciOption) o2).sortingIdx);
        }
    };

    public String getBothNames()
    {
        if (getOpt() == null)
        {
            return "--" + getLongOpt();
        }
        else if (getLongOpt() == null)
        {
            return "-" + getOpt();
        }
        return "-" + getOpt() + "/--" + getLongOpt();
    }

    public TraciOption(final Character shortOpt, final String longOpt, final String desc, final String argName)
    {
        super(shortOpt == null ? null : String.valueOf(shortOpt), longOpt, argName != null, desc);

        if (argName != null)
        {
            setArgName(argName);
        }

        if (getOpt() == null)
        {
            this.optName = getLongOpt();
        }
        else
        {
            this.optName = getOpt();
        }
    }

    public abstract Result checkOption(final CommandLine cmd);
}
