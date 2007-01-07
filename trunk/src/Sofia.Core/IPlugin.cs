using System;
using System.Collections.Generic;
using System.Text;

using Sofia.Mvc;

namespace Sofia.Plugins
{
    public interface IPlugin
    {
        IView View { get; set; }

        IController Controller { get; set; }

        IModel Model { get; set; }

        /// <summary>
        /// Description
        /// </summary>
        string Description { get; }

    }
}
