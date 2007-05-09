using System;
using System.Collections.Generic;
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
        private Dictionary<DbType, string> _types;

        public FirebirdDDL()
        {
            _types = new Dictionary<DbType, string>();

            _types[DbType.String] = "VARCHAR";
            _types[DbType.Int32] = "INTEGER";
            _types[DbType.Binary] = "BLOB SUB_TYPE 0";

        }

        #region ISgdbDDL members

        public string GetDDLType(DbType dbType, int size, bool isNotNullable)
        {
            string type = _types[dbType].ToString();

            if (size > 0) type += String.Format("({0})", size.ToString());
            if (dbType == DbType.String && size == -1) type = "BLOB SUB_TYPE 1";
            if (isNotNullable) type += " NOT NULL";

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
