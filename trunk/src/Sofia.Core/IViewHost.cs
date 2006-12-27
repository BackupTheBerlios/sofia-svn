using System;
using System.Collections.Generic;
using System.Text;

namespace Sofia.Core.Plugins
{
    public interface IViewHost
    {        
        /// <summary>
        /// Insère un plugin dans un emplacement spécifique de l'hôte
        /// </summary>
        /// <param name="plugin">Un objet plugin</param>
        /// <param name="destination">L'identifiant de la zone d'insertion</param>
        void Insert(IPlugin plugin, string destination);
    }
}
