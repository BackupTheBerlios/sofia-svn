using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

using Microsoft.Practices.CompositeUI;
using Microsoft.Practices.CompositeUI.SmartParts;

namespace Sofia.Cab.Modules.HomeChief
{
    public class HomeChiefWorkItem : WorkItem
    {
        #region fields

        private IWorkspace _contentWorkspace;

        #endregion

        public void Show(IWorkspace sideBar, IWorkspace content)
        {
            _contentWorkspace = content;

            //Needs to be named because it will be used in a placeholder
            this.Items.AddNew<UserInfoView>("UserInfo");
            SideBarView __sideBarView = this.Items.AddNew<SideBarView>();

            //AddMenuItems();

            sideBar.Show(__sideBarView);
            this.Activate();
        }
    }
}
