using System;
using System.Collections.Generic;
using System.Text;

namespace Sofia.Views.MainView
{
    public class ViewGuiFactory
    {
        public static IViewGui NewViewGui()
        {
            return (IViewGui) new ViewGui();
        }
    }
}
