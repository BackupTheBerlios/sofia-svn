using System;
using System.Collections.Generic;
using System.Text;
using System.Data.Common;
using System.Data;

namespace Sofia.Data.Common
{
    public class Query
    {
        #region Membres priv�s

        /// <summary>
        /// Objet connexion abstraite
        /// </summary>
        private Server _DbServer;

        /// <summary>
        /// Composition avec une fabrique de commandes SQL
        /// </summary>
        private ICommandFactory _CommandFactory;

        #endregion

        #region M�thodes priv�es

        private void ProcessException(Exception e)
        {
            //_DbServer.Log.WriteLine(InformationLevel.Error, e.Message);
        }

        #endregion

        #region Propri�t�s

        /// <summary>
        /// Obtient ou d�finit texte de la requ�te SQL ou le nom de la proc�dure stock�e
        /// </summary>
        public string CommandText
        {
            get { return _CommandFactory.CommandText; }
            set { _CommandFactory.CommandText = value; }
        }

         #endregion

        #region M�thodes publiques

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
        /// Execute une requ�te SQL de type INSERT, UPDATE ou DELETE
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
        /// Execute une proc�dure stock�e
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
        /// Ajoute un param�tre dans la liste des parametres
        /// </summary>
        /// <param name="name">Nom du param�tre</param>
        /// <param name="type">Type du param�tre</param>
        /// <param name="value">Valeur du param�tre</param>
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
        /// Obtient la valeur d'un param�tre
        /// </summary>
        /// <param name="parameterName">Nom du param�tre</param>
        /// <returns>La valeur du param�tre, null si le param�tre est introuvable</returns>
        public object GetParameterValue(string parameterName)
        {
            return _CommandFactory.GetParameterValue(parameterName);
        }

        /// <summary>
        /// Efface la liste des param�tres
        /// </summary>
        public void FlushParameters()
        {
            _CommandFactory.FlushParameters();
        }

        #endregion



    }
}

