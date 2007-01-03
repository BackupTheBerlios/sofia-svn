using System;
using System.Data;
using System.Data.Common;

namespace Sofia.Data.Common
{
    internal interface ICommandFactory
    {

        /// <summary>
        /// Ajoute un paramètre pour la commande
        /// </summary>
        /// <param name="name">Nom du paramètre (sans aucun symbole du type "@")</param>
        /// <param name="type">Type du paramètre</param>
        /// <param name="value">Valeur du paramètre</param>
        void AddParameter(string name, DbType type, object value, ParameterDirection direction);

        /// <summary>
        /// Instancie un objet DbCommand
        /// </summary>
        /// <returns>un objet DbCommand</returns>
        DbCommand CreateCommand(CommandType commandType);

        /// <summary>
        /// Vide la liste des paramètres
        /// </summary>
        void FlushParameters();

        /// <summary>
        /// Requête SQL ou nom d'une procédure stockée
        /// </summary>
        string CommandText { get; set; }

        /// <summary>
        /// Obtient la valeur d'un paramètre
        /// </summary>
        /// <param name="parameterName">Nom du paramètre</param>
        /// <returns>La valeur du paramètre, null si le paramètre est introuvable</returns>
        object GetParameterValue(string parameterName);
    }
}
