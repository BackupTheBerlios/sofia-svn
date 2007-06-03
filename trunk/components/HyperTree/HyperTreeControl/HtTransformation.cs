using System;
using System.Collections.Generic;
using System.Text;

namespace HyperTreeControl
{
  /// <summary> The HTTransformation class implements a isometrie transformation in the hyperbolic space.
  /// </summary>
  class HtTransformation
  {
    #region Private fields

    private HtCoordE _p = null; // translation vector
    private HtCoordE _o = null; // rotation vector 

    #endregion

    #region ctor
    /// <summary> Constructor.
    /// </summary>
    HtTransformation()
    {
      _p = new HtCoordE();
      _o = new HtCoordE();
    }

    #endregion

    #region Public properties

    public HtCoordE P
    {
      get { return _p; }
      set { _p = value; }
    }

    public HtCoordE O
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
    void composition(HtCoordE first, HtCoordE second)
    {
      _p.X = first.X + second.X;
      _p.Y = first.Y + second.Y;

      HtCoordE d = new HtCoordE(second);
      d.Y = -d.Y;
      d.Multiply(first);
      d.X += 1;
      _p.Divide(d);

      _o.X = first.X;
      _o.Y = -first.Y;
      _o.Multiply(second);
      _o.X += 1;
      _o.Divide(d);
    }

    #endregion

    #region ToString

    /// <summary> Gets the string representation of the object.
    /// </summary>
    /// <returns></returns>
    public override string ToString()
    {
      string result = "Transformation : " +
                      "\n\tP = " + _p +
                      "\n\tO = " + _o;
      return result;
    }

    #endregion

  }
}
