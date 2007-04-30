using System;
using System.Collections.Generic;
using System.Text;

using System.Data;

namespace Sofia.Data.Common
{
    public interface ISgbdDDL
    {
        /// <summary> Obtient la chaine de caract�re repr�sentant le type de donn�e sp�cifique � un SGBD
        /// </summary>
        /// <param name="dbType">Le type de donn�e abstrait</param>
        /// <returns></returns>
        string GetDDLType(DbType dbType, int size, bool isNotNullable);

        /// <summary> M�thode de cr�ation de la base de donn�es
        /// </summary>
        /// <param name="databaseName"></param>
        void CreateDatabase(Server server);
    }
}
