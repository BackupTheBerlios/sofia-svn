using System;
using System.Collections.Generic;
using System.Text;

namespace Sofia.Core.Plugins
{
    public interface IViewHost
    {        
        /// <summary>
        /// Ins�re un plugin dans un emplacement sp�cifique de l'h�te
        /// </summary>
        /// <param name="plugin">Un objet plugin</param>
        /// <param name="destination">L'identifiant de la zone d'insertion</param>
        void Insert(IPlugin plugin, string destination);
    }
}
