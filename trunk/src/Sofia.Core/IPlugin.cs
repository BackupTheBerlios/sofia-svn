using System;
using System.Collections.Generic;
using System.Text;

using Sofia.Mvc;

namespace Sofia.Plugins
{
    public interface IPlugin
    {
        IView CreateView();
        IController Controller { get; }

        /// <summary>
        /// Description
        /// </summary>
        string Description { get; }

    }
}
