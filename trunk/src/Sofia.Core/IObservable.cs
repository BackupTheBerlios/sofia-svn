using System;
using System.Collections.Generic;
using System.Text;

namespace Sofia.Core.Observable
{
    interface IObservable
    {
        /// <summary>
        /// Enregistre un observateur
        /// </summary>
        /// <param name="o">Un observateur</param>
        void Register(IObserver o);

        /// <summary>
        /// Retire un observateur de la liste des observateurs enregistrés
        /// </summary>
        /// <param name="o">Un observateur</param>
        void Unregister(IObserver o);

        /// <summary>
        /// Notifie les observateurs
        /// </summary>
        void Notify();
    }
}
