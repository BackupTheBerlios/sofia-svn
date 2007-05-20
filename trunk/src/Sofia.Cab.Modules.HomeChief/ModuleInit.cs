using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

using Microsoft.Practices.CompositeUI;
using Microsoft.Practices.ObjectBuilder;
using Microsoft.Practices.CompositeUI.SmartParts;

using SCAB_Common = Sofia.Cab.Common;
using System.Windows.Controls;
using Sofia.Cab.Common;

namespace Sofia.Cab.Modules.HomeChief
{
    public class ModuleInit : Microsoft.Practices.CompositeUI.ModuleInit
    {
        #region fields

        private HomeChiefWorkItem _workItem;

        #endregion

        [InjectionConstructor]
        public ModuleInit([ServiceDependency] HomeChiefWorkItem workItem)
        {
            _workItem = workItem;
        }

        /// <summary>
        /// <see cref="Microsoft.Practices.CompositeUI.ModuleInit.Load()"/> for more information.
        /// </summary>
        public override void Load()
        {            
            AddCustomerMenuItem();

            //Retrieve well known workspaces
            IWorkspace sideBarWorkspace = _workItem.Workspaces[SCAB_Common.WorkspacesConstants.SHELL_SIDEBAR];
            IWorkspace contentWorkspace = _workItem.Workspaces[SCAB_Common.WorkspacesConstants.SHELL_CONTENT];

            HomeChiefWorkItem __workItem = _workItem.WorkItems.AddNew<HomeChiefWorkItem>();
            __workItem.Show(sideBarWorkspace, contentWorkspace);
        }

        private void AddCustomerMenuItem()
        {
            MenuItem __customerItem = new MenuItem();
            __customerItem.Header = "Customer";
            __customerItem.Tag = 8;
            _workItem.UIExtensionSites[UIExtensionConstants.FILE].Add(__customerItem);
            _workItem.UIExtensionSites.RegisterSite(Properties.Resources.CustomerMenuExtensionSite, __customerItem);
        }

    }
}
