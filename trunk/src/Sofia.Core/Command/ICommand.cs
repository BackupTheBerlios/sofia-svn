
using System;

namespace Sofia.Core
{
	
	public interface ICommand
	{
		//Execution/Annulation
		void Execute(string parameters);
		void UnExecute();
		
		//Identifiant de la commande
		string Id  { 
			get; 
		}
		
		//Informations sur la commande
		CommandProperties Properties {
			get;
		}
	}
	
}
