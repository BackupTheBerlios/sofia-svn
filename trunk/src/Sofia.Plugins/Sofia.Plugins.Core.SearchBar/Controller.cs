using System;
using System.Collections.Generic;
using System.Text;

using Sofia.Mvc;

namespace Sofia.Plugins.Core.Search
{
    public class Controller: ControllerBase
    {
        public Controller(IModel model) : base(model) { }

        public override object Toolbar
        {
            get
            {
                IView view = Find(0);
                if (view != null)
                    return view;
                else
                    return null;

            }
        }
    }
}
