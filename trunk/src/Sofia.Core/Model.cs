using System;
using System.Collections.Generic;
using System.Text;

using Sofia.Core.Observable;

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

        #region Méthodes métier



        #endregion

    }
}
