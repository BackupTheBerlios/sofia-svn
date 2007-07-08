using System;
using System.Collections.Generic;
using System.Text;

namespace HyperTreeControl
{
    /// <summary> The HyperbolicTransformation class implements a isometrie transformation in the hyperbolic space.
  /// </summary>
  public class HyperbolicTransformation
  {
    #region Private fields

    private EuclidianVector _p = null; // translation vector
    private EuclidianVector _o = null; // rotation vector 

    #endregion

    #region ctor
    /// <summary> Constructor.
    /// </summary>
    public HyperbolicTransformation()
    {
      _p = new EuclidianVector();
      _o = new EuclidianVector();
    }

    #endregion

    #region Public properties

    public EuclidianVector P
    {
      get { return _p; }
      set { _p = value; }
    }

    public EuclidianVector O
    {
      get { return _o; }
      set { _o = value; }
    }

    #endregion

    #region Composition of 2 translation

    /// <summary> Compose the 2 given vectors translations into one given transformation.
    /// </summary>
    /// <param name="first"></param>
    /// <param name="second"></param>
    public void Composition(EuclidianVector v1, EuclidianVector v2)
    {
      _p.X = v1.X + v2.X;
      _p.Y = v1.Y + v2.Y;

      EuclidianVector __d = new EuclidianVector(v2);
      __d.Y = -__d.Y;
      __d.Multiply(v1);
      __d.X += 1;
      _p.Divide(__d);

      _o.X = v1.X;
      _o.Y = -v1.Y;
      _o.Multiply(v2);
      _o.X += 1;
      _o.Divide(__d);
    }

    #endregion

    #region ToString

    /// <summary> Gets the string representation of the object.
    /// </summary>
    /// <returns></returns>
    public override string ToString()
    {
      string __result = "Transformation : " +
                      " P= " + _p +
                      ";O= " + _o;
      return __result;
    }

    #endregion

  }
}
