using System;
using System.Collections.Generic;
using System.Text;

namespace Sofia.Cab.Shell
{
    class Entry
    {
        [STAThread]
        public static void Main()
        {
            //start up the CAB application
            new ShellApplication().Run();
        }
    }
}
