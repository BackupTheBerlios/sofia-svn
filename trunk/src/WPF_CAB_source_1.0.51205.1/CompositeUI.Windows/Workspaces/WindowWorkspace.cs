using System;
using System.Collections.Generic;
using System.Text;
using System.Windows;
using Microsoft.Practices.CompositeUI.SmartParts;

namespace Microsoft.Practices.CompositeUI.Windows.Workspaces
{
	/// <summary>
	/// Implements a Workspace that shows smartparts in windows.
	/// </summary>
	public class WindowWorkspace : Workspace<UIElement, WindowSmartPartInfo>
	{
		private readonly IDictionary<UIElement, Window> _windows;

		public WindowWorkspace()
		{
			_windows = new Dictionary<UIElement, Window>();
		}

		protected override void OnActivate(UIElement smartPart)
		{
			Window window = _windows[smartPart];
			window.BringIntoView();
			window.Activate();
		}

		protected override void OnApplySmartPartInfo(UIElement smartPart, WindowSmartPartInfo smartPartInfo)
		{
			Window window = _windows[smartPart];
			SetWindowProperties(window, smartPartInfo);
			SetWindowLocation(window, smartPartInfo);
		}

		protected override void OnShow(UIElement smartPart, WindowSmartPartInfo smartPartInfo)
		{
			Window window = GetOrCreateWindow(smartPart);
			ShowWindow(window, smartPartInfo);
		}

		protected override void OnHide(UIElement smartPart)
		{
			Window window = GetOrCreateWindow(smartPart);
			window.Hide();
		}

		protected override void OnClose(UIElement smartPart)
		{
			Window window = GetOrCreateWindow(smartPart);
			window.Close();
			_windows.Remove(smartPart);
		}

		private Window GetOrCreateWindow(UIElement smartPart)
		{
			Window window;

			if (!_windows.TryGetValue(smartPart, out window))
			{
				window = new Window();
				_windows[smartPart] = window;
				window.Content = smartPart;

				window.Closing += delegate(object sender, System.ComponentModel.CancelEventArgs e)
				{
					RaiseSmartPartClosing(e);
				};

				window.Closed += delegate(object sender, EventArgs e)
				{
					_windows.Remove(smartPart);
					InnerSmartParts.Remove(smartPart);
				};
			}

			return window;
		}

		private void ShowWindow(Window window, WindowSmartPartInfo smartPartInfo)
		{
			SetWindowProperties(window, smartPartInfo);
			SetWindowLocation(window, smartPartInfo);

			if (smartPartInfo.Modal)
			{
				window.ShowDialog();
			}
			else
			{
				window.Show();
				window.BringIntoView();
			}
		}

		private void SetWindowProperties(Window window, WindowSmartPartInfo smartPartInfo)
		{
			window.Title = smartPartInfo.Title;
			window.Icon = smartPartInfo.Icon;
			window.Width = smartPartInfo.Width;
			window.Height = smartPartInfo.Height;
			window.Owner = smartPartInfo.Owner;
			
			if (smartPartInfo.MinWidth.HasValue)
			{
				window.MinWidth = smartPartInfo.MinWidth.Value;
			}

			if (smartPartInfo.MaxWidth.HasValue)
			{
				window.MaxWidth = smartPartInfo.MaxWidth.Value;
			}

			if (smartPartInfo.MinHeight.HasValue)
			{
				window.MinHeight = smartPartInfo.MinHeight.Value;
			}

			if (smartPartInfo.MaxHeight.HasValue)
			{
				window.MaxHeight = smartPartInfo.MaxHeight.Value;
			}

			if (smartPartInfo.ResizeMode.HasValue)
			{
				window.ResizeMode = smartPartInfo.ResizeMode.Value;
			}

			if (smartPartInfo.ShowInTaskbar.HasValue)
			{
				window.ShowInTaskbar = smartPartInfo.ShowInTaskbar.Value;
			}

			if (smartPartInfo.SizeToContent.HasValue)
			{
				window.SizeToContent = smartPartInfo.SizeToContent.Value;
			}

			if (smartPartInfo.Topmost.HasValue)
			{
				window.Topmost = smartPartInfo.Topmost.Value;
			}
		}

		private void SetWindowLocation(Window window, WindowSmartPartInfo smartPartInfo)
		{
			if (smartPartInfo.Left.HasValue)
			{
				window.Left = smartPartInfo.Left.Value;
			}

			if (smartPartInfo.Top.HasValue)
			{
				window.Top = smartPartInfo.Top.Value;
			}

			if (smartPartInfo.WindowStartupLocation.HasValue)
			{
				window.WindowStartupLocation = smartPartInfo.WindowStartupLocation.Value;
			}
		}
	}
}
