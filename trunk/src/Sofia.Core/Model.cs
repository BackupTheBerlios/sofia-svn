using System;
using System.Collections.Generic;
using System.Collections;
using System.Text;

using System.Data;

using Sofia.Data.Common;
using Sofia.Data.Firebird;
using Sofia.DesignPatterns;

using Sofia.Properties;

namespace Sofia.Mvc
{
    /// <summary>
    /// Classe représentant le modèle
    /// </summary>
    public class Model : IObservable, IModel
    {

        #region Champs privés

        Server _Server;
        List<IObserver> _Observers;

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

            [PrimaryKey, FieldSize(32)]
            public DbStringField ContentId;

            [FieldSize(32)]
            public DbStringField FldId;

            [FieldSize(128)]
            public DbStringField ContentSummary;

            public DbTextField ContentXml;
        }
        public partial class Folder : EntityBase
        {
            public Folder(Server server) : base(server) { }

            [PrimaryKey, FieldSize(32)]
            public DbStringField FldId;

            [FieldSize(32)]
            public DbStringField FldCaption;

            public DbBinaryField FldThumb;
        }
        public partial class Tag : EntityBase
        {
            public Tag(Server server) : base(server) { }

            [PrimaryKey]
            public DbInt64Field TagId;

            [FieldSize(32)]
            public DbStringField TagCaption;

            public DbBinaryField TagThumb;
        }
        public partial class TaggedContent : EntityBase
        {
            public TaggedContent(Server server) : base(server) { }

            [PrimaryKey]
            public DbInt64Field TagId;

            [PrimaryKey, FieldSize(32)]
            public DbStringField ContentId;
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

        #region IObservable Members

        /// <summary>
        /// <see cref="IObserver"/>
        /// </summary>
        public void RegisterObserver(IObserver o)
        {
            if (!_Observers.Contains(o))
                _Observers.Add(o);
        }

        /// <summary>
        /// <see cref="IObserver"/>
        /// </summary>
        public void UnregisterObserver(IObserver o)
        {
            _Observers.Remove(o);
        }

        public void NotifyObservers(object notification)
        {
            foreach (IObserver o in _Observers)
            {
                o.Update(this, notification);
            }
        }

        #endregion
    }
}
