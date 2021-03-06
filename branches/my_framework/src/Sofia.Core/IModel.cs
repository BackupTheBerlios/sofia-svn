using System;
using System.Collections.Generic;
using System.Text;

namespace Sofia.Mvc
{
    public interface IModel
    {
        /// <summary>
        /// Ajoute ou mets � jour un document
        /// </summary>
        /// <param name="id">Identifiant du document</param>
        /// <param name="rawXml">Vue s�rialis�e</param>
        void UpdateDocument(string contentId, string contentSummary, string contentXml, bool isMasterDocument, string[] tags);
    }
}
