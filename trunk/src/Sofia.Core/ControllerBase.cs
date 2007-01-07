
using System;
using System.Collections;

using Sofia.Commands;

namespace Sofia.Mvc
{
	
	public class ControllerBase : IController
	{
		//Gestionnaire de commandes	
		CommandManager _CommandManager;
		
		public ControllerBase() {
            _CommandManager = new CommandManager();  	
		}

		#region implémentation de l'interface
		
		public virtual CommandManager CommandManager {
			get { return _CommandManager; } 
		}	
		
		
		public virtual void ExecuteCommand(string ident, string parameters)	
		{
			_CommandManager.CommandByName(ident).Execute(parameters);
		}
		
	
		#endregion
	}
	
}
