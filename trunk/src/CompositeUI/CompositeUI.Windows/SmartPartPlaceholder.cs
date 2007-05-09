using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Text;
using System.Windows;
using System.Windows.Controls;
using Microsoft.Practices.CompositeUI.SmartParts;
using Microsoft.Practices.CompositeUI.Utility;

namespace Microsoft.Practices.CompositeUI.Windows
{
	/// <summary>
	/// Implements a placeholder for smart parts in a WPF application.
	/// </summary>
	[DesignerCategory("Code")]
	public class SmartPartPlaceholder : UserControl, ISmartPartPlaceholder
	{
		private string smartPartName;
		private object smartPart;

		/// <summary>
		/// Fires when a smartpart is shown in the placeholder.
		/// </summary>
		public event EventHandler<SmartPartPlaceHolderEventArgs> SmartPartShown;

		/// <summary>
		/// Gets or sets the name of SmartPart that will be placed in the placeholder.
		/// </summary>
		public string SmartPartName
		{
			get
			{
				return smartPartName;
			}
			set
			{
				smartPartName = value;
			}
		}

		/// <summary>
		/// Gets or sets a reference to the smartpart after it has been added.
		/// </summary>
		[Browsable(false)]
		[DesignerSerializationVisibility(DesignerSerializationVisibility.Hidden)]
		public object SmartPart
		{
			get
			{
				return smartPart;
			}
			set
			{
				Guard.ArgumentNotNull(value, "value");
				smartPart = value;
				Content = smartPart;
				OnSmartPartShown(smartPart);
			}
		}

		private void OnSmartPartShown(object smartPartShown)
		{
			if (this.SmartPartShown != null)
			{
				this.SmartPartShown(this, new SmartPartPlaceHolderEventArgs(smartPartShown));
			}
		}
	}
}
