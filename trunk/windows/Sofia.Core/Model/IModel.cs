
using System;
using com.db4o;

namespace Sofia.Core
{
	public interface IModel
	{
		ObjectSet SendRequest(string request);
	}
	
}
