
using System;
using System.Xml;

namespace Sofia.Core.Plugins
{
	
	public interface IView
    {

        IModel Model { get; }
           
        /// <summary>
        /// Désérialisation de la vue
        /// </summary>
        /// <param name="rawXml"></param>
        void Load(string rawXml);

        /// <summary>
        /// Sérialisation de la vue
        /// </summary>
        /// <returns></returns>
        string Save();        

    }
      
}
