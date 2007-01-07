
using System;
using System.Xml;

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

        bool IsMasterView { get; set; }

        IController Controller { get; set; }

    }
      
}
