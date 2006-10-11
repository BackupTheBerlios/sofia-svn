
using System;
using System.Collections;

namespace Sofia.Core
{
	
	public class CommandManager
	{
	
		Hashtable cmds = new Hashtable ();
		
		public CommandManager()
		{
		}
		
		public void RegisterCommand (ICommand cmd)
		{
			cmds [cmd.Id] = cmd;			
		} 
		
		public ICommand GetCommand (string cmdId)
		{
			if (!cmds.ContainsKey(cmdId))
				throw new InvalidOperationException ("Identifiant de commande incorrect: " + cmdId);
				
			ICommand cmd = cmds [cmdId] as ICommand;
			
			return cmd;
		}
		
		public CommandProperties GetProperties(string cmdId) 
		{
			ICommand cmd = GetCommand(cmdId);
			return cmd.Properties;
		}
		
		internal ICommand FindCommand (object cmdId)
		{
			return cmds [cmdId] as ICommand;
		}
		
	}
	
}
