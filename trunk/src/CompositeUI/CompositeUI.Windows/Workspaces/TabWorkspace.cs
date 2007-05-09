using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Diagnostics;
using System.Drawing;
using System.Text;
using System.Windows;
using System.Windows.Controls;
using Microsoft.Practices.CompositeUI;
using Microsoft.Practices.CompositeUI.SmartParts;
using Microsoft.Practices.CompositeUI.Utility;

namespace Microsoft.Practices.CompositeUI.Windows.Workspaces
{
	/// <summary>
	/// Implements a workspace that shows smartparts in a <see cref="TabControl"/>.
	/// </summary>
	[DesignerCategory("Code")]
	[ToolboxBitmap(typeof(TabWorkspace), "TabWorkspace")]
	public class TabWorkspace : TabControl, IComposableWorkspace<UIElement, TabSmartPartInfo>
	{
		private readonly IDictionary<UIElement, TabItem> _tabItems;
		private WorkspaceComposer<UIElement, TabSmartPartInfo> _composer;
		private bool _callComposerActivateOnIndexChange = true;
		private bool _populatingPages;

		[ServiceDependency]
		public WorkItem WorkItem
		{
			set
			{
				_composer.WorkItem = value;
			}
		}

		public ReadOnlyDictionary<UIElement, TabItem> TabItems
		{
			get
			{
				return new ReadOnlyDictionary<UIElement, TabItem>(_tabItems);
			}
		}

		[Browsable(false)]
		[DesignerSerializationVisibility(DesignerSerializationVisibility.Hidden)]
		public System.Collections.ObjectModel.ReadOnlyCollection<object> SmartParts
		{
			get
			{
				return _composer.SmartParts;
			}
		}

		[Browsable(false)]
		[DesignerSerializationVisibility(DesignerSerializationVisibility.Hidden)]
		public object ActiveSmartPart
		{
			get
			{
				return _composer.ActiveSmartPart;
			}
		}

		public event EventHandler<WorkspaceCancelEventArgs> SmartPartClosing;

		public event EventHandler<WorkspaceCancelEventArgs> SmartPartActivating;

		public event EventHandler<WorkspaceEventArgs> SmartPartActivated;

		public TabWorkspace()
		{
			_tabItems = new Dictionary<UIElement, TabItem>();
			_composer = new WorkspaceComposer<UIElement, TabSmartPartInfo>(this);
		}

		public void OnActivate(UIElement smartPart)
		{
			PopulateTabItems();

			try
			{
				_callComposerActivateOnIndexChange = false;
				SelectedItem = GetTabItemForSmartPart(smartPart);
			}
			finally
			{
				_callComposerActivateOnIndexChange = true;
			}
		}

		public void OnApplySmartPartInfo(UIElement smartPart, TabSmartPartInfo smartPartInfo)
		{
			PopulateTabItems();
			TabItem tabItem = GetTabItemForSmartPart(smartPart);
			SetTabItemProperties(tabItem, smartPartInfo);

			if (smartPartInfo.ActivateTab)
			{
				Activate(tabItem);
			}
		}

		public void OnShow(UIElement smartPart, TabSmartPartInfo smartPartInfo)
		{
			PopulateTabItems();
			ResetSelectedIndexIfNoTabItems();

			TabItem tabItem = GetOrCreateTabItem(smartPart);
			SetTabItemProperties(tabItem, smartPartInfo);

			if (smartPartInfo.ActivateTab)
			{
				Activate(smartPart);
			}
		}

		public void OnHide(UIElement smartPart)
		{
			if (smartPart.Visibility == Visibility.Visible)
			{
				PopulateTabItems();
				TabItem tabItem = GetTabItemForSmartPart(smartPart);
				tabItem.Visibility = Visibility.Hidden;
//				ActivateSiblingTabItem();
			}
		}

		public void OnClose(UIElement smartPart)
		{
			PopulateTabItems();
			_tabItems.Remove(GetTabItemForSmartPart(smartPart));
			_tabItems.Remove(smartPart);
		}

		void IComposableWorkspace<UIElement, TabSmartPartInfo>.RaiseSmartPartActivating(WorkspaceCancelEventArgs e)
		{
			OnSmartPartActivating(e);
		}

		void IComposableWorkspace<UIElement, TabSmartPartInfo>.RaiseSmartPartActivated(WorkspaceEventArgs e)
		{
			OnSmartPartActivated(e);
		}

		void IComposableWorkspace<UIElement, TabSmartPartInfo>.RaiseSmartPartClosing(WorkspaceCancelEventArgs e)
		{
			OnSmartPartClosing(e);
		}

		public TabSmartPartInfo ConvertFrom(ISmartPartInfo source)
		{
			return SmartPartInfo.ConvertTo<TabSmartPartInfo>(source);
		}

		public bool Activate(object smartPart)
		{
			return _composer.Activate(smartPart);
		}

		public void ApplySmartPartInfo(object smartPart, ISmartPartInfo smartPartInfo)
		{
			_composer.ApplySmartPartInfo(smartPart, smartPartInfo);
		}

		public void Close(object smartPart)
		{
			_composer.Close(smartPart);
		}

		public void Hide(object smartPart)
		{
			_composer.Hide(smartPart);
		}

		public bool Show(object smartPart, ISmartPartInfo smartPartInfo)
		{
			return _composer.Show(smartPart, smartPartInfo);
		}

		public bool Show(object smartPart)
		{
			return _composer.Show(smartPart);
		}

		private void PopulateTabItems()
		{
			// If the page count matches don't waste the time repopulating the pages collection
			if (!_populatingPages && _tabItems.Count != Items.Count)
			{
				foreach (TabItem tabItem in Items)
				{
					if (!_tabItems.Values.Contains(tabItem))
					{
						UIElement uiElement = GetUIElementFromTabItem(tabItem);

						if (uiElement != null && !_composer.SmartParts.Contains(uiElement))
						{
							TabSmartPartInfo tabinfo = new TabSmartPartInfo();
							tabinfo.ActivateTab = false;
							//avoid circular calls to this method.
							_populatingPages = true;

							try
							{
								Show(uiElement, tabinfo);
							}
							finally
							{
								_populatingPages = false;
							}
						}
					}
				}
			}
		}

		private UIElement GetUIElementFromTabItem(TabItem tabItem)
		{
			return tabItem.Content as UIElement;
		}

		private TabItem GetTabItemForSmartPart(UIElement smartPart)
		{
			foreach (TabItem tabItem in Items)
			{
				if (object.ReferenceEquals(tabItem.Content, smartPart))
				{
					return tabItem;
				}
			}

			return null;
		}

		private void SetTabItemProperties(TabItem tabItem, TabSmartPartInfo smartPartInfo)
		{
			tabItem.Header = string.IsNullOrEmpty(smartPartInfo.Title) ? tabItem.Header : smartPartInfo.Title;

			try
			{
				TabItem selectedTabItem = SelectedItem as TabItem;
				_callComposerActivateOnIndexChange = false;

				if (smartPartInfo.Position == TabPosition.Beginning)
				{
					Items.Insert(0, tabItem);
				}
				else if (!Items.Contains(tabItem))
				{
					Items.Add(tabItem);
				}

				//preserve selection through the operation.
				SelectedItem = selectedTabItem;
			}
			finally
			{
				_callComposerActivateOnIndexChange = true;
			}
		}

		private void ResetSelectedIndexIfNoTabItems()
		{
			// First control to come in is special. We need to 
			// set the selected index to a non-zero index so we 
			// get the appropriate behavior for activation.
			if (Items.Count == 0)
			{
				try
				{
					_callComposerActivateOnIndexChange = false;
					SelectedIndex = -1;
				}
				finally
				{
					_callComposerActivateOnIndexChange = true;
				}
			}
		}

		private TabItem GetOrCreateTabItem(UIElement smartPart)
		{
			TabItem tabItem = GetTabItemForSmartPart(smartPart);

			if (tabItem == null)
			{
				tabItem = new TabItem();
				tabItem.HorizontalAlignment = HorizontalAlignment.Stretch;
				tabItem.VerticalAlignment = VerticalAlignment.Stretch;
				tabItem.HorizontalContentAlignment = HorizontalAlignment.Stretch;
				tabItem.VerticalContentAlignment = VerticalAlignment.Stretch;
				tabItem.Content = smartPart;
				_tabItems.Add(smartPart, tabItem);
			}
			else if (!_tabItems.ContainsKey(smartPart))
			{
				_tabItems.Add(smartPart, tabItem);
			}

			return tabItem;
		}

		protected virtual void OnSmartPartActivating(WorkspaceCancelEventArgs e)
		{
			if (SmartPartActivating != null)
			{
				SmartPartActivating(this, e);
			}
		}

		protected virtual void OnSmartPartActivated(WorkspaceEventArgs e)
		{
			if (SmartPartActivated != null)
			{
				SmartPartActivated(this, e);
			}
		}

		protected virtual void OnSmartPartClosing(WorkspaceCancelEventArgs e)
		{
			if (SmartPartClosing != null)
			{
				SmartPartClosing(this, e);
			}
		}

		protected override void OnSelectionChanged(SelectionChangedEventArgs e)
		{
			base.OnSelectionChanged(e);

			if (_callComposerActivateOnIndexChange && Items.Count != 0)
			{
				//locate the smart part corresponding to the tab page
				foreach (KeyValuePair<UIElement, TabItem> tabItem in _tabItems)
				{
					if (object.ReferenceEquals(tabItem.Value, SelectedItem))
					{
						Activate(tabItem.Key);
						return;
					}
				}

				// If we got here, we couldn't find a corresponding smart part for the 
				// currently active tab, hence we reset the ActiveSmartPart value.
				_composer.SetActiveSmartPart(null);
			}
		}

		protected override void OnItemsChanged(System.Collections.Specialized.NotifyCollectionChangedEventArgs e)
		{
			base.OnItemsChanged(e);
			PopulateTabItems();
		}
	}
}
