using System;
using System.Collections.Generic;
using System.Text;
using System.Reflection;

namespace Sofia.Core.Reflection
{
    class InstanceFactory
    {
        /// <summary>
        /// Invoque le constructeur sans param�tres d'un type d'une assembly
        /// </summary>
        /// <param name="assemblyFile">Chemin complet de l'assembly</param>
        /// <param name="type">Type export� par l'assembly</param>
        /// <returns>L'instance de l'objet</returns>
        public static object CreateInstanceFrom(string assemblyFile, Type type)
        {            
            Assembly assembly = Assembly.LoadFrom(assemblyFile);

            foreach (Type exported in assembly.GetExportedTypes())
            {
                //Type[] myInterfaces = exported.FindInterfaces(Module.FilterTypeName, type.Name);
                //if (myInterfaces.Length != 0)
                if (type.IsAssignableFrom(exported))                
                {
                    ConstructorInfo Constructor = exported.GetConstructor(Type.EmptyTypes);
                    if (Constructor != null)
                        return Constructor.Invoke(null);
                }
            }
            return null;

        }
    }
}
