
using System;
using System.Collections;

using Sofia.Commands;

namespace Sofia.Mvc
{

    public class ControllerBase : IController
    {
        IModel _Model;
        IView _View;

        //Gestionnaire de commandes	
        CommandManager _CommandManager;

        public ControllerBase()
        {
            _CommandManager = new CommandManager();
        }

        #region implémentation de l'interface

        public IModel Model
        {
            get
            {
                return _Model;
            }
            set
            {
                _Model = value;
            }
        }

        public IView View
        {
            get
            {
                return _View;
            }
            set
            {
                _View = value;
            }
        }

        public virtual void ExecuteCommand(string ident, string parameters)
        {
            _CommandManager.CommandByName(ident).Execute(parameters);
        }

        public void Save()
        {
            _Model.UpdateDocument(_View.ContentId.ToString("N"), _View.ContentSummary, _View.SaveToXml(), _View.IsMasterView, _View.Tags);
        }

        #endregion
    }

}
