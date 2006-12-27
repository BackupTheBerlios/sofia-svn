using System;
using System.Collections.Generic;
using System.Text;

namespace Sofia.Core.Plugins
{
    public interface IModel
    {
        /// <summary>
        /// Ajoute ou mets � jour un document
        /// </summary>
        /// <param name="id">Identifiant du document</param>
        /// <param name="rawXml">Vue s�rialis�e</param>
        void Update(string id, string rawXml);
    }
}
