//===============================================================================
// Microsoft patterns & practices
// CompositeUI Application Block
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

namespace BankShell
{
	class MenuItemElement : ConfigurationElement
	{
		[ConfigurationProperty("commandname", IsRequired = false)]
		public string CommandName
		{
			get
			{
				return (string)this["commandname"];
			}
			set
			{
				this["commandname"] = value;
			}
		}

		[ConfigurationProperty("mergeorder", IsRequired = false)]
		public int MergeOrder
		{
			get
			{
				return (int) this["mergeorder"];
			}
			set
			{
				this["mergeorder"] = value;
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

		[ConfigurationProperty("registrationsite", IsRequired = false)]
		public string RegistrationSite
		{
			get
			{
				return (string)this["registrationsite"];
			}
			set
			{
				this["registrationsite"] = value;
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
