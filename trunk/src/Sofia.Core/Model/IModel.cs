
using System;
using com.db4o;
using System.Collections;


namespace Sofia.Core
{
	public interface IModel
	{
		IList SendRequest (string request);	
	}
	
}
