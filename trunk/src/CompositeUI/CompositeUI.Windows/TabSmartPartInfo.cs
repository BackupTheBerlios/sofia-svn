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
	/// A <see cref="SmartPartInfo"/> that describes how a specific smartpart will be shown in a tab workspace.
	/// </summary>
	[ToolboxBitmap(typeof(TabSmartPartInfo), "TabSmartPartInfo")]
	public class TabSmartPartInfo : SmartPartInfo
	{
		private TabPosition _position = TabPosition.End;
		private bool _activateTab = true;

		/// <summary>
		/// Specifies whether the tab will get focus when shown.
		/// </summary>
		[Category("Layout")]
		[DefaultValue(true)]
		public bool ActivateTab
		{
			get
			{
				return _activateTab;
			}
			set
			{
				_activateTab = value;
			}
		}

		/// <summary>
		/// Specifies the position of the tab page.
		/// </summary>
		[Category("Layout")]
		[DefaultValue(TabPosition.End)]
		public TabPosition Position
		{
			get
			{
				return _position;
			}
			set
			{
				_position = value;
			}
		}
	}

	/// <summary>
	/// Specifies the position of the tab page on a <see cref="TabWorkspace"/>.
	/// </summary>
	public enum TabPosition
	{
		/// <summary>
		/// Place tab page at begining.
		/// </summary>
		Beginning,
		/// <summary>
		/// Place tab page at end.
		/// </summary>
		End
	}
}
