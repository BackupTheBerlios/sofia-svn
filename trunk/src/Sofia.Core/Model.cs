using System;
using System.Collections.Generic;
using System.Text;

using System.Data;

using Sofia.Data.Common;
using Sofia.Core.Observable;
using Sofia.Core.Properties;

namespace Sofia.Core.Plugins
{
    public class Model : IObservable, IModel
    {

        /// <summary>
        /// Constructeur
        /// </summary>
        public Model()
        {
            _Observers = new List<IObserver>();

            //Tests
            Server server = new Server("FirebirdSql.Data.FirebirdClient", Settings.Default.DatabaseName);
            server.CreateConnection();
            Documents docs = new Documents(server);
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

        public void UpdateDocument(string id, string rawXml)
        {
            throw new NotImplementedException();
        }

        #endregion

        #region Classes métier

        public class Documents : EntityBase
        {
            public Documents(Server server) : base(server) { }

            [PrimaryKey, FieldType(DbType.Guid)]
            public DbField DocId;

            [FieldType(DbType.Guid)]
            public DbField FldId;

            [FieldType(DbType.String)]
            public DbField DocCaption;
        }

        #endregion

    }
}
