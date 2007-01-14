
using System;
using System.Collections;
using System.Collections.Generic;

using Sofia.Commands;

namespace Sofia.Mvc
{

    public class ControllerBase : IController
    {
        IModel _Model;
        List<IView> _Views;

        //Gestionnaire de commandes	
        CommandManager _CommandManager;

        public ControllerBase(IModel model)
        {
            _Views = new List<IView>();
            _CommandManager = new CommandManager();
            _Model = model;            
        }

        #region implémentation de l'interface

        public virtual void ExecuteCommand(string ident, string parameters)
        {
            _CommandManager.CommandByName(ident).Execute(parameters);
        }

        public IView Find(string contentId)
        {
            return _Views.Find(delegate(IView view) { return view.ContentId.ToString("N") == contentId; });
        }

        public IView Find(int index)
        {
            return _Views[index];
        }

        public void Add(IView view)
        {
            _Views.Add(view);
            view.Index = _Views.IndexOf(view);
        }
        #endregion
    }

}
