using System;
using System.Data.Common;
using System.Collections;

namespace Sofia.Data.Common
{
    /// <summary>
    /// Classe repr�sentant une connexion � la base de donn�es 
    /// </summary>
    public class Server
    {
        #region Membres priv�s

        private DbConnection _DbConnection;
        private DbProviderFactory _DbProviderFactory;

        #endregion

        #region Connexion/D�connexion

        private string _ProviderName;
        private string _DatabaseName;
        private int _Port;

        /// <summary>
        /// Initialisation d'une connexion � partir d'un fichier de configuration au format XML
        /// </summary>
        /// <param name="configFilePath">
        /// Nom du fichier de configuration. Il doit contenir le chemin 
        /// de l'application dont le fichier ini contient le chemin de la base de donn�e
        /// </param>
        ///<seealso cref="Format du fichier ini des logiciels Axilog"/>
        public bool CreateConnection()
        {
            string connectionString = String.Format("initial catalog={0};user id=sysdba;password=masterkey;character set=NONE;data source=localhost;port={1};", _DatabaseName, _Port);

            //Initialisation de la connexion
            try
            {
                _DbProviderFactory = DbProviderFactories.GetFactory(_ProviderName);
                _DbConnection = _DbProviderFactory.CreateConnection();
                _DbConnection.ConnectionString = connectionString;
            }
            catch (Exception e)
            {                
                return false;
            }

            //Ouverture de la connexion
            try
            {
                _DbConnection.Open();
            }
            catch (Exception)
            {
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

        #region Propri�t�s

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
        public Server(string providerName, string databaseName, int port)
        {
            _ProviderName = providerName;
            _DatabaseName = databaseName;
            _Port = port;
        }

        public Server(string providerName, string databaseName) : this(providerName, databaseName, 3050) { }

        #endregion
    }

}
