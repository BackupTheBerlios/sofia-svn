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
    /// <summary>
    /// Classe représentant le modèle
    /// </summary>
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

        /// <summary>
        /// <see cref="IObserver"/>
        /// </summary>
        public void Register(IObserver o)
        {
            if (!_Observers.Contains(o))
                _Observers.Add(o);
        }

        /// <summary>
        /// <see cref="IObserver"/>
        /// </summary>
         public void Unregister(IObserver o)
        {
            _Observers.Remove(o);
        }

        /// <summary>
        /// <see cref="IObserver"/>
        /// </summary>
         public void Notify()
        {
            foreach (IObserver o in _Observers)
            {
                o.Update();
            }
        }

        #endregion

        #region Implémentation de l'interface IModel

        /// <summary>
        /// <see cref="IModel"/>
        /// </summary>
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

        public partial class Documents : EntityBase
        {
            public Documents(Server server) : base(server) { }

            [PrimaryKey, FieldType(DbType.String, 32)]
            public DbField DocId;

            [FieldType(DbType.String, 128)]
            public DbField DocCaption;

            [FieldType(DbType.String)]
            public DbField DocContent;

        }

        public partial class Tags : EntityBase
        {
            public Tags(Server server) : base(server) { }

            [PrimaryKey, FieldType(DbType.String, 15)]
            public DbField TagId;

            [FieldType(DbType.String, 32)]
            public DbField TagCaption;

            [FieldType(DbType.Binary)]
            public DbField TagThumb;

            [FieldType(DbType.String, 128)]
            public DbField TagDescription;

        }

        public partial class TaggedDocs : EntityBase
        {
            public TaggedDocs(Server server) : base(server) { }

            [PrimaryKey, FieldType(DbType.String, 15)]
            public DbField TagId;

            [PrimaryKey, FieldType(DbType.String, 32)]
            public DbField DocId;
        }

        /* 
           Exemples de mises à jour de la base
           -----------------------------------
         
        [Obsolete("Commentaire par exemple : Suprimée", true/false)] 
        [Updated(new string[] { "ClasseA, ClasseB" }, Renamed/Removed)]
        public partial ClassC Documents : EntityBase
        {
            public ClassC(Server server) : base(server) { }

            [PrimaryKey, FieldType(DbType.String, 32)]
            public DbField Id;
            
            [Obsolete("Commentaire", true/false)]
            [Updated(new string[] { "ChampA", "ChampB", ... }, Renamed/Deleted)]
            [Updated(new DbType[] {DbType.Integer, DbType.Binary, ...}]
            [FieldType(DbType.String, 128)]
            public DbField ChampC;

            [FieldType(DbType.String, -1)]
            public DbField DocContent;

        }
 
        Exemples de champs calculés
        ---------------------------
      
        public partial class ClasseA
        {
            public DbField ChampCalcule
            {
                get
                {
                    return ChampA.Value + ChampB.Value;
                }
            }
        }
        */

        #endregion

    }
}
