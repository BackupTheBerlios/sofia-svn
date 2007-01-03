using System;
using System.Collections.Generic;
using System.Text;
using System.Data.Common;
using System.Data;

namespace Sofia.Data.Common
{
    public class Query
    {
        #region Membres privés

        /// <summary>
        /// Objet connexion abstraite
        /// </summary>
        private Server _DbServer;

        /// <summary>
        /// Composition avec une fabrique de commandes SQL
        /// </summary>
        private ICommandFactory _CommandFactory;

        #endregion

        #region Méthodes privées

        private void ProcessException(Exception e)
        {
            //_DbServer.Log.WriteLine(InformationLevel.Error, e.Message);
        }

        #endregion

        #region Propriétés

        /// <summary>
        /// Obtient ou définit texte de la requête SQL ou le nom de la procédure stockée
        /// </summary>
        public string CommandText
        {
            get { return _CommandFactory.CommandText; }
            set { _CommandFactory.CommandText = value; }
        }

         #endregion

        #region Méthodes publiques

        /// <summary>
        /// Constructeur
        /// </summary>
        /// <param name="dbConnection"></param>
        public Query(Server server)
        {
            _DbServer = server;            
            _CommandFactory = new CommandFactory(_DbServer);
        }

        /// <summary>
        /// Execute une requête SQL de type INSERT, UPDATE ou DELETE
        /// </summary>
        public bool ExecuteNonQuery()
        {
            DbCommand dbCommand = _CommandFactory.CreateCommand(CommandType.Text);
            try
            {                
                dbCommand.ExecuteNonQuery();
                return true;
            }
            catch (Exception e)
            {
                ProcessException(e);
                return false;
            }
        }

        /// <summary>
        /// Execute une procédure stockée
        /// </summary>
        public bool ExecuteStoredProc()
        {
            DbCommand dbCommand = _CommandFactory.CreateCommand(CommandType.StoredProcedure);
            try
            {
                dbCommand.ExecuteNonQuery();
                return true;
            }
            catch (Exception e)
            {
                ProcessException(e);
                return false;
            }
        }        


        /// <summary>
        /// Execute une commande SQL de type SELECT
        /// </summary>
        /// <returns></returns>
        public DbDataReader ExecuteReader()
        {
            DbCommand dbCommand = _CommandFactory.CreateCommand(CommandType.Text);

            try
            {
                return dbCommand.ExecuteReader();
            }
            catch (Exception e)
            {
                ProcessException(e);
                return null;
            }

        }

        /// <summary>
        /// Ajoute un paramètre dans la liste des parametres
        /// </summary>
        /// <param name="name">Nom du paramètre</param>
        /// <param name="type">Type du paramètre</param>
        /// <param name="value">Valeur du paramètre</param>
        public void AddParameter(string name, DbType type, object value, ParameterDirection direction)
        {
            _CommandFactory.AddParameter(name, type, value, direction);
        }

        public void AddParameter(string name, DbType type, object value)
        {
            AddParameter(name, type, value, ParameterDirection.Input);
        }

        public void AddReturnParameter(string name, DbType type)
        {
            AddParameter(name, type, null, ParameterDirection.ReturnValue);
        }

        /// <summary>
        /// Obtient la valeur d'un paramètre
        /// </summary>
        /// <param name="parameterName">Nom du paramètre</param>
        /// <returns>La valeur du paramètre, null si le paramètre est introuvable</returns>
        public object GetParameterValue(string parameterName)
        {
            return _CommandFactory.GetParameterValue(parameterName);
        }

        /// <summary>
        /// Efface la liste des paramètres
        /// </summary>
        public void FlushParameters()
        {
            _CommandFactory.FlushParameters();
        }

        #endregion



    }
}

