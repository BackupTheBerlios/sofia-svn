using System;
using System.Collections.Generic;
using System.Text;

using System.Data;

using Sofia.Data.Common;
using Sofia.Data.Firebird;
using Sofia.DesignPatterns;

using Sofia.Mvc.Properties;

namespace Sofia.Mvc
{
    public class Model : IObservable, IModel
    {

        #region Champs privés

        Server _Server;

        #endregion

        /// <summary>
        /// Constructeur
        /// </summary>
        public Model()
        {
            _Observers = new List<IObserver>();
            string databasePath = AppDomain.CurrentDomain.BaseDirectory;
            _Server = new Server("FirebirdSql.Data.FirebirdClient", databasePath + "Sofia.Data.Database.fdb", new FirebirdDDL(), Settings.Default.FirebirdPort);
        }

        #region Implémentation de l'interface IObservable

        List<IObserver> _Observers;

        public void Register(IObserver o)
        {
            if (!_Observers.Contains(o))
                _Observers.Add(o);
        }

        public void Unregister(IObserver o)
        {
            _Observers.Remove(o);
        }

        public void Notify()
        {
            foreach (IObserver o in _Observers)
            {
                o.Update();
            }
        }

        #endregion

        #region Implémentation de l'interface IModel

        public void UpdateDocument(string contentId, string contentXml, bool isMasterDocument)
        {
            Documents documents = new Documents(_Server);
            documents.DocId.Value = contentId;            
            documents.DocCaption.Value = "test";
            documents.DocContent.Value = contentXml;
            documents.Update();
        }

        #endregion

        #region Classes métier

        public class Documents : EntityBase
        {
            public Documents(Server server) : base(server) { }

            [PrimaryKey, FieldType(DbType.String, 32)]
            public DbField DocId;

            [Obsolete("Utilisation des tags", true)]
            [FieldType(DbType.String, 32)]
            public DbField FldId;

            [FieldType(DbType.String, 128)]
            public DbField DocCaption;

            [FieldType(DbType.String, -1)]
            public DbField DocContent;
        }

        #endregion

    }
}
