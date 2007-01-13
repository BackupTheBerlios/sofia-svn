using System;
using System.Collections.Generic;
using System.Text;

using Sofia.Mvc;

namespace Sofia.Plugins
{
    public interface IViewHost
    {        
        /// <summary>
        /// Insère une vue dans un emplacement spécifique de l'hôte
        /// </summary>
        /// <param name="view">Une vue</param>
        /// <param name="destination">L'identifiant de la zone d'insertion</param>
        void Insert(IView view, string destination);

        /// <summary>
        /// Sauve dans le modèle la vue active
        /// </summary>
        void Save();

        /// <summary>
        /// Crée un nouveau document maître
        /// </summary>
        void New();

    }
}
