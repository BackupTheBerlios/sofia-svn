using System;
using System.Collections.Generic;
using System.Text;

namespace Sofia.Mvc
{
    public interface IModel
    {
        /// <summary>
        /// Ajoute ou mets à jour un document
        /// </summary>
        /// <param name="id">Identifiant du document</param>
        /// <param name="rawXml">Vue sérialisée</param>
        void UpdateDocument(string id, string rawXml);
    }
}
