using System;
using System.Collections.Generic;
using System.Text;

namespace BankShell
{
	public static class Entry
	{
		[STAThread]
		public static void Main()
		{
			//start up the CAB application
			new BankShellApplication().Run();
		}
	}
}
