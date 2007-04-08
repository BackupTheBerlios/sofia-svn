using System;
using System.Data.Common;
using System.Collections;

namespace Sofia.Data.Common
{
    /// <summary>
    /// Classe représentant une connexion à la base de données 
    /// </summary>
    public class Server
    {
        #region Membres privés

        private DbConnection _dbConnection;
        private DbProviderFactory _dbProviderFactory;
        private ISgbdDDL _sgbdDdl;

        #endregion

        #region Propriétés

        /// <summary> Constantes spécifique à un SGBD. 
        /// Permet la contruction de chaînes SQL en conformité avec le SGBD.
        /// </summary>
        public ISgbdDDL SgbdDDL
        {
            get
            {
                return _sgbdDdl;
            }
        }

        #endregion

        #region Connexion/Déconnexion

        private string _providerName;
        private string _databaseName;
        private int _port;
        private string _lastError;

        public string LastError
        {
            get { return _lastError; }
            set { _lastError = value; }
        }

        private string _connectionString;

        /// <summary> Chaine de connexion
        /// </summary>
        public string ConnectionString
        {
            get { return _connectionString; }
            set { _connectionString = value; }
        }

        /// <summary> Nom de la base de données
        /// </summary>
        public string DatabaseName
        {
            get { return _databaseName; }
        }

        /// <summary> Initialisation d'une connexion
        /// </summary>
        public bool OpenConnection()
        {
            string connectionString = String.Format("initial catalog={0};user id=sysdba;password=masterkey;character set=NONE;port={1};", _databaseName, _port);
            //Initialisation de la connexion
            try
            {
                _dbProviderFactory = DbProviderFactories.GetFactory(_providerName);
                _dbConnection = _dbProviderFactory.CreateConnection();
                _dbConnection.ConnectionString = _connectionString;
            }
            catch (Exception e)
            {
                _lastError = e.Message;
                return false;
            }

            //Ouverture de la connexion
            try
            {
                _dbConnection.Open();
            }
            catch (Exception e)
            {
                _lastError = e.Message;
                return false;
            }
            return true;
        }

        /// <summary>
        /// Fermeture de la connexion
        /// </summary>
        public void CloseConnexion()
        {
            _dbConnection.Close();
        }


        #endregion

        #region Propriétés

        /// <summary> Fabrique de DbProvider
        /// </summary>
        /// 
        public DbProviderFactory DbProviderFactory
        {
            get { return _dbProviderFactory; }
            set { _dbProviderFactory = value; }
        }

        /// <summary> Objet connexion abstraite
        /// </summary>
        public DbConnection DbConnection
        {
            get { return _dbConnection; }
        }

        #endregion

        #region Constructeur

        /// <summary> Constructeur
        /// </summary>
        public Server(string providerName, string databaseName, ISgbdDDL sgbdConsts, int port)
        {
            _providerName = providerName;
            _databaseName = databaseName;
            _port = port;
            _connectionString = String.Format("initial catalog={0};user id=sysdba;password=masterkey;character set=NONE;data source=localhost;port={1};", _databaseName, _port);
            _sgbdDdl = sgbdConsts;
        }

        public Server(string providerName, string databaseName, ISgbdDDL sgbdConsts)
            : this(providerName, databaseName, sgbdConsts, 3050)
        {
        }

        #endregion
    }

}
