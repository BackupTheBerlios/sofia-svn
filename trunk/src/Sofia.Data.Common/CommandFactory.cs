using System;
using System.Collections.Generic;
using System.Text;
using System.Data.Common;
using System.Data;

namespace Sofia.Data.Common
{
    internal class CommandFactory : ICommandFactory
    {
        #region Membres priv�s

        /// <summary>
        /// Liste g�n�rique de param�tres Firebird.NET 
        /// </summary>
        private List<DbParameter> _Parameters;

        /// <summary>
        /// Objet connexion abstraite
        /// </summary>
        private Server _DbServer;

        private string _SqlText;

        public string CommandText
        {
            get { return _SqlText; }
            set { _SqlText = value; }
        }

        #endregion

        #region M�thodes publiques

        /// <summary>
        /// Constructeur
        /// </summary>
        /// <param name="dbConnection"></param>
        public CommandFactory(Server server)
        {
            _DbServer = server;
            _Parameters = new List<DbParameter>();
        }

        /// <summary>
        /// Efface la liste des param�tres
        /// </summary>
        public void FlushParameters()
        {
            _Parameters = new List<DbParameter>();
        }

        /// <summary>
        /// Ajoute un param�tre dans la liste des parametres
        /// </summary>
        /// <param name="name">Nom du param�tre</param>
        /// <param name="type">Type du param�tre</param>
        /// <param name="value">Valeur du param�tre</param>
        public void AddParameter(string name, DbType type, object value, ParameterDirection direction)
        {
            DbParameter parameter = _DbServer.DbProviderFactory.CreateParameter();
            parameter.ParameterName = "@" + name;
            parameter.DbType = type;
            if (value != null)
                parameter.Value = value;
            parameter.Direction = direction;
            _Parameters.Add(parameter);
        }

        /// <summary>
        /// Obtient la valeur d'un param�tre
        /// </summary>
        /// <param name="parameterName">Nom du param�tre</param>
        /// <returns>La valeur du param�tre, null si le param�tre est introuvable</returns>
        public object GetParameterValue(string parameterName)
        {
            DbParameter parameter = _Parameters.Find(
                delegate(DbParameter dbParameter) { return dbParameter.ParameterName.EndsWith(parameterName); });

            if (parameter != null)
                return parameter.Value;
            else
                return null;
        }

        /// <summary>
        /// Construit une commande SQL en initialisant les param�tres
        /// </summary>
        /// <returns>Une commande SQL</returns>
        public DbCommand CreateCommand(CommandType commandType)
        {
            DbCommand dbCommand = _DbServer.DbConnection.CreateCommand();
            dbCommand.CommandText = _SqlText;
            dbCommand.CommandType = commandType;

            foreach (DbParameter parameter in _Parameters)
                dbCommand.Parameters.Add(parameter);

            return dbCommand;
        }

        public DbCommand CreateCommand()
        {
            return CreateCommand(CommandType.Text);
        }


        #endregion


    }
}
