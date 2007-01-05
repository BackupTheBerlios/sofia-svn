using System;
using System.Collections.Generic;
using System.Text;
using System.Data;
using System.Reflection;

namespace Sofia.Data.Common
{
    class EntityUpdater
    {
        EntityBase _Entity;

        #region Constructeur

        public EntityUpdater(EntityBase entity)
        {
            _Entity = entity;
        }


        #endregion

        #region M�thodes utilitaire

        /// <summary>
        /// Permet de d�terminer l'existence d'un table dans la base de donn�es
        /// </summary>
        /// <returns>Vrai si la table existe, faux sinon</returns>
        private DataTable FindEntitySchema()
        {
            string[] restrictions = new string[4];
            restrictions[0] = _Entity.Server.DbConnection.Database;
            restrictions[1] = "SYSDBA";
            restrictions[2] = _Entity.Name;
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
        private void Check()
        {
            if (FindEntitySchema() == null) CreateEntity();
        }

        /// <summary>
        /// Cr�ation de l'entit� physique dans la base de donn�es
        /// </summary>
        private void CreateEntity()
        {
            /*
            CREATE TABLE "Person" (
            "id" INTEGER
            NOT NULL
            )
            
            alter table "Person"
            add constraint "PK_Person"
            primary key ("id")
            
            */

            

            //Build strings for each command. I could probably have just done one big command, but
            //doing them individually helps me to debug easier.
            /*
            StringBuilder loSQL = new StringBuilder("CREATE TABLE \"Person\" (");

            loSQL.Append("\"id\" INTEGER NOT NULL,");
            loSQL.Append("\"namefirst\" VARCHAR(255),");
            loSQL.Append("\"namemiddle\" VARCHAR(255),");
            loSQL.Append("\"namelast\" VARCHAR(255))");

            string lsSQLPrimaryKeyCreate = "ALTER TABLE \"Person\" ADD CONSTRAINT \"PK_Person\" PRIMARY KEY (\"id\")";

            Query query = new Query(_DbServer);
            //query.CommandText
            */

        }

    }
}
