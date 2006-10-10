
using System;
using System.Reflection;
using com.db4o;

namespace Sofia.Core
{
	
	public class Proxy: MarshalByRefObject
	{		
		public ModelRequest LoadModel(string assemblyFilename, string request)
		{
			Assembly Dynamic = Assembly.LoadFrom(assemblyFilename);
			
			Type TypeofIModel = typeof(IModel);
			foreach (Type Exported in Dynamic.GetExportedTypes())
				if (TypeofIModel.IsAssignableFrom(Exported))
				{
					ConstructorInfo Constructor = Exported.GetConstructor(Type.EmptyTypes);
					if (Constructor != null)
					{
						IModel instance = (IModel) Constructor.Invoke(null);
						ModelRequest modelRequest = new ModelRequest(instance, request);
						return modelRequest;
					}
				}
			// if no matches
			return null;

		}
	}

	[Serializable()]
	public class ModelRequest
	{	
		private ObjectSet result;

		public ModelRequest(IModel model, string request)
		{
			result = model.SendRequest(request);
		}
		
		public ObjectSet Result { get {	return result; } }
		
	}

}
