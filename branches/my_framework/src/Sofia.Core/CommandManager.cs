
using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;

namespace Sofia.Commands
{

    public class CommandManager : KeyedCollection<string, ICommand>
    {

        /// <summary>
        /// Register a new command
        /// </summary>
        /// <param name="cmd">A command instance</param>
        public void RegisterCommand(ICommand command)
        {
            this.Add(command);            
            command.CommandManager = this;
        }

        #region protected methods

        protected override string GetKeyForItem(ICommand item)
        {
            return item.Identifier;
        }

        #endregion
    }

}
