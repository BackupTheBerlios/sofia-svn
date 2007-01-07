using System;
using System.Collections.Generic;
using System.Text;

namespace Sofia.Commands
{
    public interface ICommandReceiver
    {
        void Action(ICommand command);
    }
}
