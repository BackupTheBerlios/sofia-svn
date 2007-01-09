
using System;
using System.Xml;
using System.Collections;

namespace Sofia.Mvc
{
	
	public interface IView
    {
        /// <summary>
        /// Désérialisation de la vue
        /// </summary>
        /// <param name="rawXml"></param>
        void LoadFromXml(string rawXml);

        /// <summary>
        /// Sérialisation de la vue
        /// </summary>
        /// <returns></returns>
        string SaveToXml();

        object Control { get; }

        Guid ContentId { get; set; }

        string ContentSummary { get; }

        bool IsMasterView { get; set; }

        string[] Tags { get; }

        IController Controller { get; set; }

    }
      
}
