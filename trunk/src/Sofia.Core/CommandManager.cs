
using System;
using System.Collections;

namespace Sofia.Core.Commands
{
	
	public class CommandManager
	{
	
		Hashtable cmds = new Hashtable ();
		
        /// <summary>
        /// Constructeur
        /// </summary>
		public CommandManager()
		{
		}
		
        /// <summary>
        /// Enregistre une nouvelle commande
        /// </summary>
        /// <param name="cmd">Un objet commande</param>
		public void RegisterCommand (ICommand cmd)
		{
			cmds [cmd.Id] = cmd;
			cmd.CommandManager = this;
		} 
		
        /// <summary>
        /// Retourne un objet commande
        /// </summary>
        /// <param name="cmdId">Identifiant de la commande</param>
        /// <returns>Un objet commande</returns>
		public ICommand CommandByName (string cmdId)
		{
			if (!cmds.ContainsKey(cmdId))
				throw new InvalidOperationException ("Identifiant de commande introuvable: " + cmdId);
				
			ICommand cmd = cmds [cmdId] as ICommand;
			
			return cmd;
		}
		
        /// <summary>
        /// Retourne les propriétés d'une commande
        /// </summary>
        /// <param name="cmdId">Identifiant de la commande</param>
        /// <returns>Un objet propriétés de commande</returns>
		public CommandProperties PropertiesByName(string cmdId) 
		{
			ICommand cmd = CommandByName(cmdId);
			return cmd.Properties;
		}
		
	}
	
}
