using System;
using System.Collections.Generic;
using System.Text;
using System.Windows.Forms;

using Sofia.Core.Plugins;

namespace Sofia.Core.Plugins.WindowsForms
{
    public class ViewBase: UserControl, IView
    {
        #region Constructeur

        public ViewBase() : base() { }

        public ViewBase(IController controller): base()
        {
            _Controller = controller;
        }

        #endregion

        #region Implémentation de l'interface IView

        IController _Controller;

        public IController Controller
        {
            get
            {
                return _Controller;
            }
        }

        public void LoadFromXml(string rawXml)
        {
            throw new NotSupportedException();
        }

        public string SaveToXml()
        {
            throw new NotSupportedException();
        }

        #endregion


    }
}
