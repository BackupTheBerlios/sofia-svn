
using System;
using System.Collections;

using Sofia.Commands;

namespace Sofia.Mvc
{

    public class ControllerBase : IController
    {
        IModel _Model;

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

        public virtual void ExecuteCommand(string ident, string parameters)
        {
            _CommandManager.CommandByName(ident).Execute(parameters);
        }

        #endregion
    }

}
