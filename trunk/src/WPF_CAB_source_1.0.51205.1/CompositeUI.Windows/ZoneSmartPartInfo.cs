using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Drawing;
using System.Text;
using Microsoft.Practices.CompositeUI;
using Microsoft.Practices.CompositeUI.SmartParts;

namespace Microsoft.Practices.CompositeUI.Windows
{
	/// <summary>
	/// A <see cref="SmartPartInfo"/> that describes how a specific smartpart will be shown in a zone workspace.
	/// </summary>
	[ToolboxItem(true)]
	[ToolboxBitmap(typeof(ZoneSmartPartInfo), "ZoneSmartPartInfo")]
	public class ZoneSmartPartInfo : SmartPartInfo
	{
		private string _zoneName;

		public ZoneSmartPartInfo()
		{
		}

		public ZoneSmartPartInfo(string zoneName)
		{
			_zoneName = zoneName;
		}

		public ZoneSmartPartInfo(string title, string zoneName)
		{
			Title = title;
			_zoneName = zoneName;
		}

		[Category("Layout")]
		[DefaultValue(null)]
		public string ZoneName
		{
			get
			{
				return _zoneName;
			}
			set
			{
				_zoneName = value;
			}
		}
	}
}
