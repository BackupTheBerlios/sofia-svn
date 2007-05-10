using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Text;
using System.Windows;
using System.Windows.Media;
using Microsoft.Practices.CompositeUI.SmartParts;

namespace Microsoft.Practices.CompositeUI.Windows
{
	/// <summary>
	/// A <see cref="SmartPartInfo"/> that describes how a specific smartpart will be shown in a window workspace.
	/// </summary>
	public class WindowSmartPartInfo : SmartPartInfo
	{
		private bool _modal;
		private double _width;
		private double _height;
		private ImageSource _icon;
		private double? _left;
		private double? _top;
		private double? _minWidth;
		private double? _maxWidth;
		private double? _minHeight;
		private double? _maxHeight;
		private ResizeMode? _resizeMode;
		private bool? _showInTaskbar;
		private SizeToContent? _sizeToContent;
		private bool? _topmost;
		private WindowStartupLocation? _windowStartupLocation;
		private WindowState? _windowState;
		private WindowStyle? _windowStyle;
		private Window _owner;

		/// <summary>
		/// Whether the window should be shown modally.
		/// </summary>
		[DefaultValue(false)]
		[Category("Layout")]
		public bool Modal
		{
			get
			{
				return _modal;
			}
			set
			{
				_modal = value;
			}
		}

		[TypeConverter(typeof(LengthConverter))]
		public double Width
		{
			get
			{
				return _width;
			}
			set
			{
				_width = value;
			}
		}

		[TypeConverter(typeof(LengthConverter))]
		public double Height
		{
			get
			{
				return _height;
			}
			set
			{
				_height = value;
			}
		}

		public ImageSource Icon
		{
			get
			{
				return _icon;
			}
			set
			{
				_icon = value;
			}
		}

		[TypeConverter(typeof(LengthConverter))]
		public double? Left
		{
			get
			{
				return _left;
			}
			set
			{
				_left = value;
			}
		}

		[TypeConverter(typeof(LengthConverter))]
		public double? Top
		{
			get
			{
				return _top;
			}
			set
			{
				_top = value;
			}
		}

		[TypeConverter(typeof(LengthConverter))]
		public double? MinWidth
		{
			get
			{
				return _minWidth;
			}
			set
			{
				_minWidth = value;
			}
		}

		[TypeConverter(typeof(LengthConverter))]
		public double? MaxWidth
		{
			get
			{
				return _maxWidth;
			}
			set
			{
				_maxWidth = value;
			}
		}

		[TypeConverter(typeof(LengthConverter))]
		public double? MinHeight
		{
			get
			{
				return _minHeight;
			}
			set
			{
				_minHeight = value;
			}
		}

		[TypeConverter(typeof(LengthConverter))]
		public double? MaxHeight
		{
			get
			{
				return _maxHeight;
			}
			set
			{
				_maxHeight = value;
			}
		}

		public ResizeMode? ResizeMode
		{
			get
			{
				return _resizeMode;
			}
			set
			{
				_resizeMode = value;
			}
		}

		public bool? ShowInTaskbar
		{
			get
			{
				return _showInTaskbar;
			}
			set
			{
				_showInTaskbar = value;
			}
		}

		public SizeToContent? SizeToContent
		{
			get
			{
				return _sizeToContent;
			}
			set
			{
				_sizeToContent = value;
			}
		}

		public bool? Topmost
		{
			get
			{
				return _topmost;
			}
			set
			{
				_topmost = value;
			}
		}

		public WindowStartupLocation? WindowStartupLocation
		{
			get
			{
				return _windowStartupLocation;
			}
			set
			{
				_windowStartupLocation = value;
			}
		}

		public WindowState? WindowState
		{
			get
			{
				return _windowState;
			}
			set
			{
				_windowState = value;
			}
		}

		public WindowStyle? WindowStyle
		{
			get
			{
				return _windowStyle;
			}
			set
			{
				_windowStyle = value;
			}
		}

		public Window Owner
		{
			get
			{
				return _owner;
			}
			set
			{
				_owner = value;
			}
		}

		public WindowSmartPartInfo()
		{
			_width = double.NaN;
			_height = double.NaN;
		}
	}
}
