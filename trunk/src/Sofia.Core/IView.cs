
using System;
using System.Xml;
using System.Collections;

namespace Sofia.Mvc
{

    public enum ViewFormat
    {
        Xml,
        Text,
        Html
    }

	public interface IView
    {
        /// <summary>
        /// Désérialisation de la vue
        /// </summary>
        /// <param name="viewFormat">Type de transformation</param>
        /// <param name="rawXml"></param>
        void LoadFrom(string raw, ViewFormat viewFormat);

        /// <summary>
        /// Sérialisation de la vue
        /// </summary>
        /// <param name="viewFormat">Type de transformation</param>
        /// <returns>Une chaine</returns>
        void SaveTo(ViewFormat viewFormat);        

        object Control { get; }

        Guid ContentId { get; set; }

        string ContentSummary { get; }

        string ContentXml { get; }

        bool IsMasterView { get; }

        string[] Tags { get; }

        /// <summary>
        /// Obtient l'ordre d'instanciation de la vue
        /// </summary>
        int Index { get; set;}

        IController Controller { get; set; }
        IModel Model { get; set;}

    }
      
}
