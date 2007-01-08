using System;
using System.Collections;
using System.Text;
using System.Data;
using System.Data.Common;
using System.IO;

using Sofia.Data.Common;
using FirebirdSql.Data.FirebirdClient;

namespace Sofia.Data.Firebird
{
    public class FirebirdDDL : ISgbdDDL
    {
        private Hashtable _Types;

        public FirebirdDDL()
        {
            _Types = new Hashtable();

            _Types[DbType.String] = "VARCHAR";
            _Types[DbType.Int32] = "INTEGER";
            _Types[DbType.Binary] = "BLOB SUB_TYPE 0";
        }

        #region Implémentation de l'interface

        public string GetDDLType(DbType dbType, int size, bool isNullable)
        {
            string type = _Types[dbType].ToString();

            if (size != 0) type += String.Format("({0})", size.ToString());
            if (dbType == DbType.String && size == -1) type = "BLOB SUB_TYPE 1";
            if (!isNullable) type += " NOT NULL";

            return type;
        }

        public void CreateDatabase(Server server)
        {
            string fileName = server.DatabaseName;
            if (File.Exists(fileName))
                return;
            FbConnection.CreateDatabase(server.ConnectionString, 8192, true, false);

        }

        #endregion

    }
}
