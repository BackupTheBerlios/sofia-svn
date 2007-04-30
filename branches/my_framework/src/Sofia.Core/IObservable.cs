using System;
using System.Collections.Generic;
using System.Text;

namespace Sofia.DesignPatterns
{
    public interface IObservable
    {
        /// <summary>
        /// Enregistre un observateur
        /// </summary>
        /// <param name="o">Un observateur</param>
        void RegisterObserver(IObserver o);

        /// <summary>
        /// Retire un observateur de la liste des observateurs enregistrés
        /// </summary>
        /// <param name="o">Un observateur</param>
        void UnregisterObserver(IObserver o);

        /// <summary>
        /// Notifie les observateurs
        /// </summary>
        void NotifyObservers(object notification);
    }
}
