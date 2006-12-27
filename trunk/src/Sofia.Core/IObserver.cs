using System;
using System.Collections.Generic;
using System.Text;

namespace Sofia.Core.Observable
{
    public interface IObserver
    {
        /// <summary>
        /// M�thode permettant de recevoir la notification d'un sujet
        /// </summary>
        void Update();
    }
}
