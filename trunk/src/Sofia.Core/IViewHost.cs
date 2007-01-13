using System;
using System.Collections.Generic;
using System.Text;

using Sofia.Mvc;

namespace Sofia.Plugins
{
    public interface IViewHost
    {        
        /// <summary>
        /// Ins�re une vue dans un emplacement sp�cifique de l'h�te
        /// </summary>
        /// <param name="view">Une vue</param>
        /// <param name="destination">L'identifiant de la zone d'insertion</param>
        void Insert(IView view, string destination);

        /// <summary>
        /// Sauve dans le mod�le la vue active
        /// </summary>
        void Save();

        /// <summary>
        /// Cr�e un nouveau document ma�tre
        /// </summary>
        void New();

    }
}
