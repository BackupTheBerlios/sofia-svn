
using System;
using System.Collections;

using Sofia.Core.Commands;

namespace Sofia.Core.Plugins
{
	
	public interface IController
    {
      	/// <summary>
        /// Gestionnaire de commandes
      	/// </summary>
      	CommandManager CommandManager { get; }
      	
      	/// <summary>
      	/// Execute une commande
      	/// </summary>
      	/// <param name="ident">Identifiant de la commande</param>
      	/// <param name="parameters">Paramètres de la commande</param>
      	void ExecuteCommand(string ident, string parameters);
      	      	
    }
	
}
