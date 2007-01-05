using System;
using System.Collections.Generic;
using System.Text;

using System.Data;

namespace Sofia.Data.Common
{
    public interface ISQLTypes
    {
        /// <summary>
        /// Obtient la chaine de caractère représentant le type de donnée spécifique à un SGBD
        /// </summary>
        /// <param name="dbType">Le type de donnée abstrait</param>
        /// <returns></returns>
        string GetTypeString(DbType dbType);
    }
}
