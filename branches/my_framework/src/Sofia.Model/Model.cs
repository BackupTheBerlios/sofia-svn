using System;
using System.Collections.Generic;
using System.Collections;
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
        public void UpdateDocument(string contentId, string contentSummary, string contentXml, bool isMasterDocument, string[] tags)
        {
            Content content = new Content(_Server);
            content.ContentId.Value = contentId;
            content.ContentSummary.Value = contentSummary;
            content.ContentXml.Value = contentXml;

            //Si c'est un nouveau document et que c'est un document maître alors on crée le dossier
            //et on rattache le document à ce dossier
            if (!content.Exists() && isMasterDocument)
            {
                Folder folder = new Folder(_Server);
                folder.FldId.Value = Guid.NewGuid().ToString("N");
                folder.FldCaption.Value = content.ContentSummary.Value;
                content.FldId.Value = folder.FldId.Value;
                folder.Update();
            }

            //Mise à jour des tags auto du document
            foreach (string s in tags)
            {
                TaggedContent taggedContent = new TaggedContent(_Server);
                taggedContent.ContentId.Value = content.ContentId.Value;
                taggedContent.TagId.Value = s.GetHashCode();

                if (!taggedContent.Exists())
                {
                    Tag tag = new Tag(_Server);
                    tag.TagId.Value = taggedContent.TagId.Value;
                    tag.TagCaption.Value = s;
                    tag.Update();
                }

                taggedContent.Update();

            }

            content.Update();
        }

        #endregion

        #region Classes métier

        public partial class Content : EntityBase
        {
            public Content(Server server) : base(server) { }

            [PrimaryKey, FieldType(DbType.String, 32)]
            public DbField ContentId;

            [FieldType(DbType.String, 32)]
            public DbField FldId;

            [FieldType(DbType.String, 128)]
            public DbField ContentSummary;

            [FieldType(DbType.String)]
            public DbField ContentXml;

        }

        public partial class Folder : EntityBase
        {
            public Folder(Server server) : base(server) { }

            [PrimaryKey, FieldType(DbType.String, 32)]
            public DbField FldId;

            [FieldType(DbType.String, 32)]
            public DbField FldCaption;

            [FieldType(DbType.Binary)]
            public DbField FldThumb;
        }

        public partial class Tag : EntityBase
        {
            public Tag(Server server) : base(server) { }

            [PrimaryKey, FieldType(DbType.Int32)]
            public DbField TagId;

            [FieldType(DbType.String, 32)]
            public DbField TagCaption;

            [FieldType(DbType.Binary)]
            public DbField TagThumb;
        }

        public partial class TaggedContent : EntityBase
        {
            public TaggedContent(Server server) : base(server) { }

            [PrimaryKey, FieldType(DbType.Int32)]
            public DbField TagId;

            [PrimaryKey, FieldType(DbType.String, 32)]
            public DbField ContentId;

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
