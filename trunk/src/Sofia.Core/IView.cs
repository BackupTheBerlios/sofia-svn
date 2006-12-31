
using System;
using System.Xml;

namespace Sofia.Core.Plugins
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

    }
      
}
