//===============================================================================
// Copyright © Microsoft Corporation.  All rights reserved.
// THIS CODE AND INFORMATION IS PROVIDED "AS IS" WITHOUT WARRANTY
// OF ANY KIND, EITHER EXPRESSED OR IMPLIED, INCLUDING BUT NOT
// LIMITED TO THE IMPLIED WARRANTIES OF MERCHANTABILITY AND
// FITNESS FOR A PARTICULAR PURPOSE.
//===============================================================================

using System.Collections.Generic;
using System.Configuration;

namespace Sofia.Cab.Shell
{
	class ShellItemsSection : ConfigurationSection
	{
		[ConfigurationProperty("menuItems", IsDefaultCollection = true)]
		public MenuItemElementCollection MenuItems
		{
			get
			{
				return (MenuItemElementCollection)(this["menuItems"]);
			}
		}
	}
}
