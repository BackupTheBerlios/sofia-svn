using System;
using System.Collections.Generic;
using System.Text;

namespace HyperTreeControl
{
  /// <summary> The HTCoordE class implements the coordinates of a point in the Euclidian space.
  /// </summary>
  public class HtCoordE
  {
    #region Private fields

    private double _x = 0.0; // x coord
    private double _y = 0.0; // y coord

    #endregion

    #region ctor

    /// <summary> Constructor.
    /// x = 0 and y = 0.
    /// </summary>
    public HtCoordE() { }

    /// <summary> Constructor copying the given screen point.
    /// </summary>
    /// <param name="z">The screen point to copy.</param>
    public HtCoordE(HtCoordE z)
    {
      this.Copy(z);
    }

    /// <summary> Constructor fixing x and y.
    /// </summary>
    /// <param name="x">The x coord.</param>
    /// <param name="y">The y coord.</param>
    public HtCoordE(double x, double y)
    {
      _x = x;
      _y = y;
    }

    #endregion

    #region Copy

    /// <summary> Copy the given HtCoordE into this HtCoordE.
    /// </summary>
    /// <param name="z">The HtCoordE to copy.</param>
    public void Copy(HtCoordE z)
    {
      _x = z._x;
      _y = z._y;
    }

    #endregion

    #region Public properties

    /// <summary> Gets the x coordinate.
    /// </summary>
    public double X
    {
      get { return _x; }
      set { _x = value; }
    }

    /// <summary> Gets the y coordinate.
    /// </summary>
    public double Y
    {
      get { return _y; }
      set { _y = value; }
    }

    #endregion

    #region Projections

    /// <summary> Projects from Screen to Euclidian.
    /// </summary>
    /// <param name="x">The x screen coordinate.</param>
    /// <param name="y">The y screen coordinate.</param>
    /// <param name="sOrigin">The origin of the screen plane.</param>
    /// <param name="sMax">The (xMax, yMax) point in the screen plane.</param>
    public void ProjectionStoE(int x, int y, HtCoordS sOrigin, HtCoordS sMax)
    {
      _x = (double)(x - sOrigin.X) / (double)sMax.X;
      _y = -((double)(y - sOrigin.Y) / (double)sMax.Y);
    }


    #endregion

    #region Validation

    /// <summary> Is this coordinate in the hyperbolic disc ?
    /// </summary>
    /// <returns><code>true</code> if this point is in; <code>false</code> otherwise</returns>
    public bool IsValid
    {
        get
        {
            return (this.D2() < 1.0);
        }
    }

    #endregion

    #region Transformation

    /*
     * Some complex computing formula :
     *
     * arg(z)  = atan(y / x) if x > 0
     *         = atan(y / x) + Pi if x < 0
     *
     * d(z)    = Math.sqrt((z.x * z.x) + (z.y * z.y)) 
     *
     * conj(z) = | z.x
     *           | - z.y
     *
     * a * b   = | (a.x * b.x) - (a.y * b.y)
     *           | (a.x * b.y) + (a.y * b.x)
     *
     * a / b   = | ((a.x * b.x) + (a.y * b.y)) / d(b)
     *           | ((a.y * b.x) - (a.x * b.y)) / d(b)
     */


    /// <summary> Multiply this coordinate by the given coordinate.      
    /// </summary>
    /// <param name="z">The coord to multiply with.</param>
    public void Multiply(HtCoordE z)
    {
      double __tx = _x;
      double __ty = _y;
      _x = (__tx * z._x) - (__ty * z._y);
      _y = (__tx * z._y) + (__ty * z._x);
    }

    /// <summary> Divide this coordinate by the given coordinate.
    /// </summary>
    /// <param name="z">The coord to divide with.</param>
    public void Divide(HtCoordE z)
    {
      double d = z.D2();
      double tx = _x;
      double ty = _y;
      _x = ((tx * z._x) + (ty * z._y)) / d;
      _y = ((ty * z._x) - (tx * z._y)) / d;
    }

    /// <summary> Substracts the second coord to the first one and put the result in this HTCoorE (this = a - b).
    /// </summary>
    /// <param name="a">The first coord.</param>
    /// <param name="b">The second coord.</param>
    public void Sub(HtCoordE a, HtCoordE b)
    {
      _x = a._x - b._x;
      _y = a._y - b._y;
    }

    /// <summary> Adds the second coord to the first one and put the result in this HTCoorE (this = a - b).
    /// </summary>
    /// <param name="a">The first coord.</param>
    /// <param name="b">The second coord.</param>
    public void Add(HtCoordE a, HtCoordE b)
    {
        _x = a._x + b._x;
        _y = a._y + b._y;
    }

    /// <summary> Returns the angle between the x axis and the line passing throught the origin O and this point.
    /// The angle is given in radians.
    /// </summary>
    /// <returns>The angle, in radians.</returns>
    public double Arg()
    {
      double __a = Math.Atan(_y / _x);
      if (_x < 0)
      {
        __a += Math.PI;
      }
      else if (_y < 0)
      {
        __a += 2 * Math.PI;
      }
      return __a;
    }

    /// <summary> Returns the square of the distance from the origin to this point.
    /// </summary>
    /// <returns>The square of the distance.</returns>
    public double D2()
    {
      return (_x * _x) + (_y * _y);
    }

    /// <summary> Returns the distance from the origin  to this point.
    /// </summary>
    /// <returns>The distance.</returns>
    public double D()
    {
      return Math.Sqrt(D2());
    }

    /// <summary> Returns the distance from this point to the point given in parameter.
    /// </summary>
    /// <param name="p">The other point.</param>
    /// <returns>The distance between the 2 points.</returns>
    public double D(HtCoordE p)
    {
      return Math.Sqrt((p._x - _x) * (p._x - _x) + (p._y - _y) * (p._y - _y));
    }

    /// <summary> Translate this Euclidian point  by the coordinates of the given Euclidian point.
    /// </summary>
    /// <param name="t">The translation coordinates.</param>
    public void Translate(HtCoordE t)
    {
      // z = (z + t) / (1 + z * conj(t))

      // first the denominator
      double __denX = (_x * t._x) + (_y * t._y) + 1;
      double __denY = (_y * t._x) - (_x * t._y);
      double __dd = (__denX * __denX) + (__denY * __denY);

      // and the numerator
      double __numX = _x + t._x;
      double __numY = _y + t._y;

      // then the division (bell)
      _x = ((__numX * __denX) + (__numY * __denY)) / __dd;
      _y = ((__numY * __denX) - (__numX * __denY)) / __dd;
    }

    /// <summary>Translate the given Euclidian point by the coordinates of the given translation vector, and put the results in this point.
    /// </summary>
    /// <param name="s">The source point.</param>
    /// <param name="t">The translation vector.</param>
    public void Translate(HtCoordE s, HtCoordE t)
    {
      this.Copy(s);
      this.Translate(t);
    }

    /// <summary>
    /// Transform this node by the given transformation.
    /// </summary>
    /// <param name="t">The transformation.</param>
    public void Transform(HtTransformation t)
    {

      HtCoordE __z = new HtCoordE(this);
      Multiply(t.O);
      _x += t.P._x;
      _y += t.P._y;

      HtCoordE __d = new HtCoordE(t.P);
      __d._y = -__d._y;
      __d.Multiply(__z);
      __d.Multiply(t.O);
      __d._x += 1;

      this.Divide(__d);
    }

    #endregion

    #region ToString

    public override string ToString()
    {
      return "(" + _x + " : " + _y + ")E";
    }

    #endregion


  }
}
