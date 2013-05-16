package se.ejp.traci.lang.parser;

class ParserUtilities
{
    static String unquoteQstring(String str)
    {
        if (str == null)
        {
            return null;
        }

        str = str.substring(1, str.length() - 1);
        str = str.replace("\\\\", "\\");

        return str;
    }
}
