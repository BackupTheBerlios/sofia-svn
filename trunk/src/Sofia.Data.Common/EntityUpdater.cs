using System;
using System.Collections.Generic;
using System.Text;
using System.Data;
using System.Reflection;

namespace Sofia.Data.Common
{
    public class EntityUpdater
    {
        EntityBase _Entity;

        #region Constructeur

        public EntityUpdater(EntityBase entity)
        {
            _Entity = entity;
        }


        #endregion

        #region Méthodes utilitaire

        /// <summary>
        /// Permet de déterminer l'existence d'un table dans la base de données
        /// </summary>
        /// <returns>Vrai si la table existe, faux sinon</returns>
        private DataTable FindEntitySchema()
        {            
            string[] restrictions = new string[4];
            restrictions[0] = _Entity.Server.DbConnection.Database;
            restrictions[1] = "SYSDBA";
            restrictions[2] = _Entity.Name.ToUpper();
            restrictions[3] = "BASE TABLE";

            DataTable table = _Entity.Server.DbConnection.GetSchema("Tables", restrictions);

            if (table.Rows.Count == 0)
                return null;
            else return table;
        }

        #endregion

        /// <summary>
        /// 
        /// </summary>
        public void Check()
        {
            _Entity.Server.SgbdDDL.CreateDatabase(_Entity.Server);
            if (FindEntitySchema() == null) _Entity.Create();
        }


    }
}
