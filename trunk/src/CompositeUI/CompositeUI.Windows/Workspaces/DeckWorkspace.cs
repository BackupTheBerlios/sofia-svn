using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.ComponentModel;
using System.Diagnostics;
using System.Drawing;
using System.Text;
using System.Windows;
using System.Windows.Controls;
using Microsoft.Practices.CompositeUI;
using Microsoft.Practices.CompositeUI.SmartParts;

namespace Microsoft.Practices.CompositeUI.Windows.Workspaces
{
	/// <summary>
	/// Implements a workspace that displays smart parts in a "deck".
	/// </summary>
	[DesignerCategory("Code")]
	[ToolboxBitmap(typeof(DeckWorkspace), "DeckWorkspace")]
	public class DeckWorkspace : UserControl, IComposableWorkspace<UIElement, SmartPartInfo>
	{
		private readonly Stack<UIElement> _stack;
		private readonly WorkspaceComposer<UIElement, SmartPartInfo> _composer;

		[Browsable(false)]
		[DesignerSerializationVisibility(DesignerSerializationVisibility.Hidden)]
		public ReadOnlyCollection<UIElement> SmartParts
		{
			get
			{
				UIElement[] uiElements = new UIElement[_composer.SmartParts.Count];
				_composer.SmartParts.CopyTo(uiElements, 0);
				return new ReadOnlyCollection<UIElement>(uiElements);
			}
		}

		[Browsable(false)]
		[DesignerSerializationVisibility(DesignerSerializationVisibility.Hidden)]
		public UIElement ActiveSmartPart
		{
			get
			{
				return _composer.ActiveSmartPart as UIElement;
			}
		}

		[ServiceDependency]
		public WorkItem WorkItem
		{
			set
			{
				_composer.WorkItem = value;
			}
		}

		ReadOnlyCollection<object> IWorkspace.SmartParts
		{
			get
			{
				return _composer.SmartParts;
			}
		}

		object IWorkspace.ActiveSmartPart
		{
			get
			{
				return _composer.ActiveSmartPart;
			}
		}

		public event EventHandler<WorkspaceCancelEventArgs> SmartPartClosing;

		public event EventHandler<WorkspaceCancelEventArgs> SmartPartActivating;

		public event EventHandler<WorkspaceEventArgs> SmartPartActivated;

		public DeckWorkspace()
		{
			_composer = new WorkspaceComposer<UIElement, SmartPartInfo>(this);
			_stack = new Stack<UIElement>();
			HorizontalAlignment = HorizontalAlignment.Stretch;
			VerticalAlignment = VerticalAlignment.Stretch;
		}

		public void OnActivate(UIElement smartPart)
		{
			if (ActiveSmartPart != null)
			{
				_composer.Hide(ActiveSmartPart);
			}

			_stack.Push(smartPart);
			smartPart.Visibility = Visibility.Visible;
			Content = smartPart;
		}

		public void OnApplySmartPartInfo(UIElement smartPart, SmartPartInfo smartPartInfo)
		{
			//nothing to do
		}

		public void OnShow(UIElement smartPart, SmartPartInfo smartPartInfo)
		{
			_composer.Activate(smartPart);
		}

		public void OnHide(UIElement smartPart)
		{
			Debug.Assert(object.ReferenceEquals(smartPart, _stack.Peek()));
			smartPart.Visibility = Visibility.Collapsed;
			_stack.Pop();

			//show the next view on the stack
			if (_stack.Count > 0)
			{
				_composer.Activate(_stack.Peek());
			}
		}

		public void OnClose(UIElement smartPart)
		{
			Debug.Assert(object.ReferenceEquals(smartPart, _stack.Peek()));
			Debug.Assert(object.ReferenceEquals(smartPart, Content));
			Content = null;
			_stack.Pop();

			//show the next view on the stack
			if (_stack.Count > 0)
			{
				_composer.Activate(_stack.Peek());
			}
		}

		void IComposableWorkspace<UIElement, SmartPartInfo>.RaiseSmartPartActivating(WorkspaceCancelEventArgs e)
		{
			OnSmartPartActivating(e);
		}

		void IComposableWorkspace<UIElement, SmartPartInfo>.RaiseSmartPartActivated(WorkspaceEventArgs e)
		{
			OnSmartPartActivated(e);
		}

		void IComposableWorkspace<UIElement, SmartPartInfo>.RaiseSmartPartClosing(WorkspaceCancelEventArgs e)
		{
			OnSmartPartClosing(e);
		}

		SmartPartInfo IComposableWorkspace<UIElement, SmartPartInfo>.ConvertFrom(ISmartPartInfo source)
		{
			return OnConvertFrom(source);
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

		protected void OnSmartPartClosing(WorkspaceCancelEventArgs e)
		{
			if (SmartPartClosing != null)
			{
				SmartPartClosing(this, e);
			}
		}

		protected virtual SmartPartInfo OnConvertFrom(ISmartPartInfo source)
		{
			return SmartPartInfo.ConvertTo<SmartPartInfo>(source);
		}
	}
}
