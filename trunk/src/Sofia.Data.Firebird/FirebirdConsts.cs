using System;
using System.Collections;
using System.Text;
using System.Data;

using Sofia.Data.Common;

namespace Sofia.Data.Firebird
{
    public class FirebirdConsts: ISgbdConsts
    {
        private Hashtable _Types;

        public FirebirdConsts()
        {
            _Types = new Hashtable();

            _Types[DbType.String] = "VARCHAR";
            _Types[DbType.Int32] = "INTEGER";
            _Types[DbType.Binary] = "BLOB SUB_TYPE 0";
        }

        public string GetDDLString(DbType dbType)
        {
            return _Types[dbType].ToString();
        }

        public string GetTextBlobString()
        {
            return "BLOB SUB_TYPE 1";
        }
    }
}
