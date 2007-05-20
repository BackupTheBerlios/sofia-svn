//===============================================================================
// Copyright © Microsoft Corporation.  All rights reserved.
// THIS CODE AND INFORMATION IS PROVIDED "AS IS" WITHOUT WARRANTY
// OF ANY KIND, EITHER EXPRESSED OR IMPLIED, INCLUDING BUT NOT
// LIMITED TO THE IMPLIED WARRANTIES OF MERCHANTABILITY AND
// FITNESS FOR A PARTICULAR PURPOSE.
//===============================================================================

using System;
using System.Configuration;
using System.Windows.Controls;

namespace Sofia.Cab.Shell
{
	class MenuItemElement : ConfigurationElement
	{
		[ConfigurationProperty("commandName", IsRequired = false)]
		public string CommandName
		{
			get
			{
				return (string)this["commandName"];
			}
			set
			{
				this["commandName"] = value;
			}
		}

		[ConfigurationProperty("mergeOrder", IsRequired = false)]
		public int MergeOrder
		{
			get
			{
				return (int) this["mergeOrder"];
			}
			set
			{
				this["mergeOrder"] = value;
			}
		}

		[ConfigurationProperty("key", IsRequired = false)]
		public string Key
		{
			get
			{
				return (string)this["key"];
			}
			set
			{
				this["key"] = value;
			}
		}

		[ConfigurationProperty("id", IsRequired = false, IsKey = true)]
		public int ID
		{
			get
			{
				return (int)this["id"];
			}
			set
			{
				this["id"] = value;
			}
		}

		[ConfigurationProperty("label", IsRequired = true)]
		public string Label
		{
			get
			{
				return (string)this["label"];
			}
			set
			{
				this["label"] = value;
			}
		}

		[ConfigurationProperty("site", IsRequired = true)]
		public string Site
		{
			get
			{
				return (string)this["site"];
			}
			set
			{
				this["site"] = value;
			}
		}

		[ConfigurationProperty("register", IsRequired = false)]
		public bool Register
		{
			get
			{
				return (bool)this["register"];
			}
			set
			{
				this["register"] = value;
			}
		}

		[ConfigurationProperty("registrationSite", IsRequired = false)]
		public string RegistrationSite
		{
			get
			{
				return (string)this["registrationSite"];
			}
			set
			{
				this["registrationSite"] = value;
			}
		}

		public MenuItem ToMenuItem()
		{
			MenuItem result = new MenuItem();
			result.Header = Label;
			result.Tag = MergeOrder;

			if (!string.IsNullOrEmpty(Key))
			{
//				result.InputGestureText = Key;
			}

			return result;
		}
	}
}
