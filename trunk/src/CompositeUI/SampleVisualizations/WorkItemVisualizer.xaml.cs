using System;
using System.Collections.Generic;
using System.Text;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Data;
using System.Windows.Documents;
using System.Windows.Input;
using System.Windows.Media;
using System.Windows.Media.Imaging;
using System.Windows.Navigation;
using System.Windows.Shapes;
using Microsoft.Practices.CompositeUI;
using Microsoft.Practices.CompositeUI.SmartParts;
using Microsoft.Practices.CompositeUI.Utility;
using Microsoft.Practices.ObjectBuilder;

namespace SampleVisualizations
{
	/// <summary>
	/// Visualizer to show CAB work items.
	/// </summary>
	public partial class WorkItemVisualizer : System.Windows.Controls.UserControl
	{
		private readonly IVisualizer _visualizer;

		public WorkItemVisualizer()
		{
			InitializeComponent();
		}

		[InjectionConstructor]
		public WorkItemVisualizer(WorkItem rootWorkItem, IVisualizer visualizer)
			: this()
		{
			_visualizer = visualizer;
			WorkItemAdded(this, new DataEventArgs<WorkItem>(visualizer.CabRootWorkItem));
			rootWorkItem.Workspaces["MainWorkspace"].Show(this, new SmartPartInfo("WorkItems", string.Empty));
		}

		private void AddTreeViewItem(ItemCollection items, WorkItem workItem)
		{
			bool firstChild = (items.Count == 0);
			WorkItemTreeViewItem item = new WorkItemTreeViewItem(workItem);
			items.Add(item);

			if (firstChild && (item.Parent is WorkItemTreeViewItem))
			{
				(item.Parent as WorkItemTreeViewItem).IsExpanded = true;
			}
		}

		private WorkItemTreeViewItem GetTreeViewItem(WorkItem workItem)
		{
			return GetTreeViewItem(workItem, _workItemsTreeView.Items);
		}

		private WorkItemTreeViewItem GetTreeViewItem(WorkItem workItem, ItemCollection items)
		{
			foreach (WorkItemTreeViewItem item in items)
			{
				if (item.WorkItem == workItem)
				{
					return item;
				}

				WorkItemTreeViewItem treeViewItem = GetTreeViewItem(workItem, item.Items);

				if (treeViewItem != null)
				{
					return treeViewItem;
				}
			}

			return null;
		}

		private void WorkItemAdded(object sender, DataEventArgs<WorkItem> e)
		{
			ItemCollection collection = null;
			WorkItem workItem = e.Data;

			if (workItem.Parent == null)
			{
				collection = _workItemsTreeView.Items;
			}
			else
			{
				WorkItemTreeViewItem item = GetTreeViewItem(workItem.Parent);

				if (item != null)
				{
					collection = item.Items;
				}
			}

			if (collection != null && workItem.WorkItems != null)
			{
				workItem.IdChanged += WorkItemIdChanged;
				workItem.Activated += WorkItemActivated;
				workItem.Deactivated += WorkItemDeactivated;
				workItem.Terminated += WorkItemTerminated;
				workItem.WorkItems.Added += WorkItemAdded;
				workItem.WorkItems.Removed += WorkItemRemoved;

				AddTreeViewItem(collection, e.Data);
			}
		}

		private void WorkItemRemoved(object sender, DataEventArgs<WorkItem> e)
		{
			WorkItem workItem = e.Data;

			workItem.IdChanged -= WorkItemIdChanged;
			workItem.Activated -= WorkItemActivated;
			workItem.Deactivated -= WorkItemDeactivated;
			workItem.Terminated -= WorkItemTerminated;
			workItem.WorkItems.Added -= WorkItemAdded;
			workItem.WorkItems.Removed -= WorkItemRemoved;

			WorkItemTreeViewItem item = GetTreeViewItem(e.Data);

			if (item != null)
			{
				(item.Parent as ItemsControl).Items.Remove(item);
			}
		}

		private void WorkItemActivated(object sender, EventArgs e)
		{
			Update((WorkItem) sender);
		}

		private void WorkItemDeactivated(object sender, EventArgs e)
		{
			Update((WorkItem) sender);
		}

		private void WorkItemTerminated(object sender, EventArgs e)
		{
			Update((WorkItem) sender);
		}

		private void WorkItemIdChanged(object sender, DataEventArgs<string> e)
		{
			Update((WorkItem) sender);
		}

		private void Update(WorkItem workItem)
		{
			WorkItemTreeViewItem item = GetTreeViewItem(workItem);

			if (item != null)
			{
				item.Update();
			}
		}

		private void _workItemsTreeView_SelectedItemChanged(object sender, RoutedPropertyChangedEventArgs<object> e)
		{
			WorkItemTreeViewItem item = e.NewValue as WorkItemTreeViewItem;

			if (item.WorkItem.Status == WorkItemStatus.Terminated)
			{
				_propertyGrid.SelectedObject = null;
				MessageBox.Show("This work item has been terminated.");
			}
			else
			{
				_propertyGrid.SelectedObject = item.WorkItem;
			}
		}

		private sealed class WorkItemTreeViewItem : TreeViewItem
		{
			private readonly WorkItem _workItem;

			public WorkItem WorkItem
			{
				get
				{
					return _workItem;
				}
			}

			public WorkItemTreeViewItem(WorkItem workItem)
			{
				_workItem = workItem;
				Update();
			}

			public void Update()
			{
				Header = _workItem.GetType().Name + " - " + _workItem.ID;

				if (_workItem.Status == WorkItemStatus.Active)
				{
					Background = Brushes.Yellow;
				}
				else if (_workItem.Status == WorkItemStatus.Inactive)
				{
					Background = Brushes.White;
				}
				else
				{
					Background = Brushes.LightGray;
				}
			}
		}
	}
}