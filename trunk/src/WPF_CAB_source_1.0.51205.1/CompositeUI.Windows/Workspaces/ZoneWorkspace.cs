using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.ComponentModel;
using System.Text;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Markup;
using Microsoft.Practices.CompositeUI;
using Microsoft.Practices.CompositeUI.SmartParts;
using Microsoft.Practices.CompositeUI.Utility;

namespace Microsoft.Practices.CompositeUI.Windows.Workspaces
{
	/// <summary>
	/// Implements a workspace that displays smart parts in specific zones.
	/// </summary>
	public class ZoneWorkspace : DockPanel, IComposableWorkspace<FrameworkElement, ZoneSmartPartInfo>, ISupportInitialize
	{
		private readonly IDictionary<ContentControl, string> _zoneNamesByContentControl;
		private readonly IDictionary<string, ContentControl> _contentControlsByZoneName;
		private readonly IDictionary<object, string> _zoneNamesBySmartPart;
		private readonly WorkspaceComposer<FrameworkElement, ZoneSmartPartInfo> _composer;
		private ContentControl _defaultZone;

		[ServiceDependency]
		public WorkItem WorkItem
		{
			set
			{
				_composer.WorkItem = value;
			}
		}

		[Browsable(false)]
		public ReadOnlyDictionary<string, ContentControl> Zones
		{
			get
			{
				return new ReadOnlyDictionary<string, ContentControl>(_contentControlsByZoneName);
			}
		}

		public ReadOnlyCollection<object> SmartParts
		{
			get
			{
				return _composer.SmartParts;
			}
		}

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

		public ZoneWorkspace()
		{
			_zoneNamesByContentControl = new Dictionary<ContentControl, string>();
			_contentControlsByZoneName = new Dictionary<string, ContentControl>();
			_zoneNamesBySmartPart = new Dictionary<object, string>();
			_composer = new WorkspaceComposer<FrameworkElement, ZoneSmartPartInfo>(this);
		}

		public void OnActivate(FrameworkElement smartPart)
		{
			smartPart.GotFocus -= SmartPartGotFocus;

			try
			{
				AddFrameworkElementToZone(smartPart, _zoneNamesBySmartPart[smartPart]);
			}
			finally
			{
				smartPart.GotFocus += SmartPartGotFocus;
			}
		}

		public void OnApplySmartPartInfo(FrameworkElement smartPart, ZoneSmartPartInfo smartPartInfo)
		{
			AddFrameworkElementToZone(smartPart, smartPartInfo == null ? null : smartPartInfo.ZoneName);
		}

		public void OnShow(FrameworkElement smartPart, ZoneSmartPartInfo smartPartInfo)
		{
			if (_contentControlsByZoneName.Count == 0)
			{
				throw new InvalidOperationException("Properties.Resources.NoZonesInZoneWorkspace");
			}

			AddFrameworkElementToZone(smartPart, smartPartInfo == null ? null : smartPartInfo.ZoneName);
			Activate(smartPart);
		}

		public void OnHide(FrameworkElement smartPart)
		{
			smartPart.Visibility = Visibility.Hidden;
		}

		public void OnClose(FrameworkElement smartPart)
		{
			smartPart.GotFocus -= SmartPartGotFocus;

			if (smartPart.Parent != null)
			{
				(smartPart.Parent as ContentControl).Content = null;
			}
		}

		void IComposableWorkspace<FrameworkElement, ZoneSmartPartInfo>.RaiseSmartPartActivating(WorkspaceCancelEventArgs e)
		{
			OnSmartPartActivating(e);
		}

		void IComposableWorkspace<FrameworkElement, ZoneSmartPartInfo>.RaiseSmartPartActivated(WorkspaceEventArgs e)
		{
			OnSmartPartActivated(e);
		}

		void IComposableWorkspace<FrameworkElement, ZoneSmartPartInfo>.RaiseSmartPartClosing(WorkspaceCancelEventArgs e)
		{
			OnSmartPartClosing(e);
		}

		ZoneSmartPartInfo IComposableWorkspace<FrameworkElement, ZoneSmartPartInfo>.ConvertFrom(ISmartPartInfo source)
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

		[Category("Layout")]
		[DefaultValue(null)]
		[DisplayName("ZoneName")]
		[Description("Zone name assigned to the content control so that smart parts can be shown on it programmatically.")]
		public string GetZoneName(ContentControl target)
		{
			if (!_zoneNamesByContentControl.ContainsKey(target))
			{
				return null;
			}
			else
			{
				return _zoneNamesByContentControl[target];
			}
		}

		[DisplayName("ZoneName")]
		public void SetZoneName(ContentControl target, string name)
		{
			if (IsDescendant(target))
			{
				if (_zoneNamesByContentControl.ContainsKey(target))
				{
					string oldname = _zoneNamesByContentControl[target];
					_contentControlsByZoneName.Remove(oldname);
					_zoneNamesByContentControl.Remove(target);
				}
				else if (!string.IsNullOrEmpty(name))
				{
//					target.ParentChanged += OnZoneParentChanged;
				}

				if (!string.IsNullOrEmpty(name))
				{
					_zoneNamesByContentControl[target] = name;
					_contentControlsByZoneName[name] = target;
				}
			}
		}

		[Category("Layout")]
		[DefaultValue(false)]
		[DisplayName("IsDefaultZone")]
		[Description("Specifies whether the zone is the default one for showing smart parts.")]
		public bool GetIsDefaultZone(FrameworkElement target)
		{
			return object.ReferenceEquals(_defaultZone, target);
		}

		[DisplayName("IsDefaultZone")]
		public void SetIsDefaultZone(ContentControl target, bool isDefault)
		{
			if (isDefault)
			{
				_defaultZone = target;
			}
			else
			{
				_defaultZone = null;
			}
		}

		protected override void OnVisualChildrenChanged(DependencyObject visualAdded, DependencyObject visualRemoved)
		{
			base.OnVisualChildrenChanged(visualAdded, visualRemoved);

			if (visualAdded != null)
			{
//				RefreshRecursive(visualAdded);
			}
			else if (visualRemoved != null && visualRemoved is ContentControl)
			{
				RemoveContentControlFromDictionaries(visualRemoved as ContentControl);
			}
		}

		private void RemoveContentControlFromDictionaries(ContentControl contentControl)
		{
			if (_zoneNamesByContentControl.ContainsKey(contentControl))
			{
				_contentControlsByZoneName.Remove(GetZoneName(contentControl));
				_zoneNamesByContentControl.Remove(contentControl);
			}
			else
			{
				ContentControl[] keys = new ContentControl[_zoneNamesByContentControl.Keys.Count];
				_zoneNamesByContentControl.Keys.CopyTo(keys, 0);

				foreach (ContentControl key in keys)
				{
					if (IsDescendant(contentControl, key))
					{
						_contentControlsByZoneName.Remove(GetZoneName(key));
						_zoneNamesByContentControl.Remove(key);
//						key.ParentChanged -= OnZoneParentChanged;

						Close(key.Content);
					}
				}

			}
		}

		void ISupportInitialize.BeginInit()
		{
			OnBeginInit();
		}

		void ISupportInitialize.EndInit()
		{
			OnEndInit();
		}

		protected virtual void OnBeginInit()
		{
		}

		protected virtual void OnEndInit()
		{
			//treat top-level controls inside named zones as smart parts too.
			foreach (KeyValuePair<string, ContentControl> zone in _contentControlsByZoneName)
			{
				// Only set the zone name, so that the remaining information stays the same
				// as specified at design-time. ZoneName is required so that the Show is 
				// performed on the proper zone (should be a no-op).
				ZoneSmartPartInfo zoneInfo = new ZoneSmartPartInfo(zone.Key);
				Show((zone.Value as ContentControl).Content, zoneInfo);
			}
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

		protected virtual ZoneSmartPartInfo OnConvertFrom(ISmartPartInfo source)
		{
			return SmartPartInfo.ConvertTo<ZoneSmartPartInfo>(source);
		}

		private void SmartPartGotFocus(object sender, EventArgs e)
		{
			FrameworkElement FrameworkElement = sender as FrameworkElement;

			if (FrameworkElement != null)
			{
				Activate(FrameworkElement);
			}
			else
			{
				_composer.SetActiveSmartPart(null);
			}
		}

		private void AddFrameworkElementToZone(FrameworkElement frameworkElement, string zoneName)
		{
			ContentControl zone = GetTargetZone(zoneName);
			zone.Content = frameworkElement;
			_zoneNamesBySmartPart[frameworkElement] = zoneName;
		}

		private ContentControl GetTargetZone(string zoneName)
		{
			ContentControl zone = _defaultZone;

			if (zoneName != null)
			{
				if (!_contentControlsByZoneName.ContainsKey(zoneName))
				{
					throw new ArgumentOutOfRangeException("ZoneName");//, String.Format(CultureInfo.CurrentCulture, Properties.Resources.NoZoneWithName, info.ZoneName));
				}

				zone = _contentControlsByZoneName[zoneName];
			}

			if (zone == null)
			{
				throw new InvalidOperationException(string.Format("Target zone with name '{0}' not found and no default zone is set up.", zoneName));
			}

			return zone;
		}

		private bool IsDescendant(FrameworkElement target)
		{
			return IsDescendant(this, target);
		}

		private bool IsDescendant(object parent, FrameworkElement child)
		{
			DependencyObject childParent = child.Parent;

			while (childParent != null && childParent != parent && childParent is FrameworkElement)
			{
				childParent = (childParent as FrameworkElement).Parent;
			}

			return object.ReferenceEquals(childParent, parent);
		}
	}
}
