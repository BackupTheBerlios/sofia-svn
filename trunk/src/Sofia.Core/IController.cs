
using System;
using System.Collections;

using Sofia.Commands;

namespace Sofia.Mvc
{
	
	public interface IController
    {
        IView View { get; set; }
        IModel Model { get; set; }

      	/// <summary>
      	/// Execute une commande
      	/// </summary>
      	/// <param name="ident">Identifiant de la commande</param>
      	/// <param name="parameters">Paramètres de la commande</param>
      	void ExecuteCommand(string ident, string parameters);

        /// <summary>
        /// Enregistre la vue dans le modèle
        /// </summary>
        void Save();
      	      	
    }
	
}
