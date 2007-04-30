using System;
using System.Collections.Generic;
using System.Text;
using System.Data;
using System.Reflection;

namespace Sofia.Data.Common
{
    public class EntityUpdater
    {

        #region private fields

        EntityBase _entity;

        #endregion

        #region Constructeur

        public EntityUpdater(EntityBase entity)
        {
            _entity = entity;
        }


        #endregion

        #region Méthodes utilitaire

        /// <summary>
        /// Permet de déterminer l'existence d'un table dans la base de données
        /// </summary>
        /// <returns>Vrai si la table existe, faux sinon</returns>
        private DataTable FindEntitySchema()
        {
            _entity.Server.OpenConnection();
            try
            {
                string[] restrictions = new string[4];
                restrictions[0] = _entity.Server.DbConnection.Database;
                restrictions[1] = "SYSDBA";
                restrictions[2] = _entity.Name.ToUpper();
                restrictions[3] = "BASE TABLE";

                DataTable table = _entity.Server.DbConnection.GetSchema("Tables", restrictions);

                if (table.Rows.Count == 0)
                    return null;
                else return table;

            }
            finally
            {
                _entity.Server.CloseConnexion();
            }
        }

        #endregion

        /// <summary>
        /// 
        /// </summary>
        public void Check()
        {
            _entity.Server.SgbdDDL.CreateDatabase(_entity.Server);
            if (FindEntitySchema() == null) _entity.Create();
        }


    }
}
