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

        private DbConnection _DbConnection;
        private DbProviderFactory _DbProviderFactory;
        private ISgbdDDL _SgbdDDL;

        #endregion

        #region Propriétés

        /// <summary>
        /// Constantes spécifique à un SGBD. Permet la contruction de chaînes SQL en conformité
        /// avec le SGBD.
        /// </summary>
        public ISgbdDDL SgbdDDL
        {
            get
            {
                return _SgbdDDL;
            }
        }

        #endregion

        #region Connexion/Déconnexion

        private string _ProviderName;
        private string _DatabaseName;
        private int _Port;
        private string _lastError;

        public string LastError
        {
            get { return _lastError; }
            set { _lastError = value; }
        }
        private string _ConnectionString;

        /// <summary>
        /// Chaine de connexion
        /// </summary>
        public string ConnectionString
        {
            get { return _ConnectionString; }
            set { _ConnectionString = value; }
        }

        /// <summary>
        /// Nom de la base de données
        /// </summary>
        public string DatabaseName
        {
            get { return _DatabaseName; }
        }

        /// <summary>
        /// Initialisation d'une connexion à partir d'un fichier de configuration au format XML
        /// </summary>
        /// <param name="configFilePath">
        /// Nom du fichier de configuration. Il doit contenir le chemin 
        /// de l'application dont le fichier ini contient le chemin de la base de donnée
        /// </param>
        ///<seealso cref="Format du fichier ini des logiciels Axilog"/>
        public bool OpenConnection()
        {
            string connectionString = String.Format("initial catalog={0};user id=sysdba;password=masterkey;character set=NONE;port={1};", _DatabaseName, _Port);
            //Initialisation de la connexion
            try
            {
                _DbProviderFactory = DbProviderFactories.GetFactory(_ProviderName);
                _DbConnection = _DbProviderFactory.CreateConnection();
                _DbConnection.ConnectionString = _ConnectionString;
            }
            catch (Exception e)
            {
                _lastError = e.Message;
                return false;
            }

            //Ouverture de la connexion
            try
            {
                _DbConnection.Open();
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
            _DbConnection.Close();
        }


        #endregion

        #region Propriétés

        /// <summary>
        /// Fabrique de DbProvider
        /// </summary>
        /// 
        public DbProviderFactory DbProviderFactory
        {
            get { return _DbProviderFactory; }
            set { _DbProviderFactory = value; }
        }

        /// <summary>
        /// Objet connexion abstraite
        /// </summary>
        public DbConnection DbConnection
        {
            get { return _DbConnection; }
        }

        #endregion

        #region Constructeur

        /// <summary>
        /// Constructeur
        /// </summary>
        public Server(string providerName, string databaseName, ISgbdDDL sgbdConsts, int port)
        {
            _ProviderName = providerName;
            _DatabaseName = databaseName;
            _Port = port;
            _ConnectionString = String.Format("initial catalog={0};user id=sysdba;password=masterkey;character set=NONE;data source=localhost;port={1};", _DatabaseName, _Port);
            _SgbdDDL = sgbdConsts;
        }

        public Server(string providerName, string databaseName, ISgbdDDL sgbdConsts)
            : this(providerName, databaseName, sgbdConsts, 3050)
        {
        }

        #endregion
    }

}
