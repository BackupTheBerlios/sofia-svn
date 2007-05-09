using System;
using System.Collections.Generic;
using System.Text;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Controls.Primitives;
using Microsoft.Practices.CompositeUI.UIElements;
using Microsoft.Practices.CompositeUI.Utility;

namespace Microsoft.Practices.CompositeUI.Windows.UIElements
{
	/// <summary>
	/// Implements a <see cref="UIElementAdapter"/> for <see cref="FrameworkElement"/>s.
	/// </summary>
	public class ItemsControlUIAdapter : UIElementAdapter<FrameworkElement>
	{
		private readonly ItemsControl _itemsControl;

		public ItemsControlUIAdapter(ItemsControl itemsControl)
		{
			Guard.ArgumentNotNull(itemsControl, "itemsControl");
			_itemsControl = itemsControl;
		}

		protected override FrameworkElement Add(FrameworkElement uiElement)
		{
			_itemsControl.Items.Insert(GetInsertingIndex(uiElement), uiElement);
			return uiElement;
		}

		protected override void Remove(FrameworkElement uiElement)
		{
			//wpf gets confused if the application is gone
			if (Application.Current != null)
			{
				_itemsControl.Items.Remove(uiElement);
			}
		}

		protected virtual int GetInsertingIndex(FrameworkElement uiElement)
		{
			int mergeIndex = 0;

			//a dirty hack to allow menu items to be placed in specific positions amongst
			//other menu items. i'm heading to confession on Sunday
			//TODO: change this to an attached property?

			if (uiElement.Tag is int)
			{
				mergeIndex = (int) uiElement.Tag;
			}
			else
			{
				return _itemsControl.Items.Count;
			}

			int index = 0;

			for (int i = 0; i < _itemsControl.Items.Count; ++i)
			{
				FrameworkElement frameworkElement = _itemsControl.Items[i] as FrameworkElement;

				if (frameworkElement != null)
				{
					int itemMergeIndex = 0;

					if (frameworkElement.Tag is int)
					{
						itemMergeIndex = (int) frameworkElement.Tag;
					}

					if (mergeIndex > itemMergeIndex)
					{
						index = i + 1;
					}
					else
					{
						break;
					}
				}
			}

			return index;
		}
	}
}
