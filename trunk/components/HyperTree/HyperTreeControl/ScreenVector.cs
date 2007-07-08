using System;
using System.Collections.Generic;
using System.Text;

namespace HyperTreeControl
{
  /// <summary> The HTCoordS class implements the coordinates of a point in the Screen space.
  /// As the screen space is represented with finite pixels, we just use int instead of float or double.
  /// </summary>
  public class ScreenVector
  {
    #region Private fields

    private static readonly int ZONE_LENGTH = 4; // size of the zone
    private int _x = 0; // x coord
    private int _y = 0; // y coord

    #endregion

    #region ctor

    /// <summary> Constructor.
    /// x = 0 and y = 0.
    /// </summary>
    public ScreenVector() { }

    /// <summary> Constructor copying the given screen point.
    /// </summary>
    /// <param name="z">The screen point to copy.</param>
    public ScreenVector(ScreenVector z)
    {
      _x = z._x;
      _y = z._y;
    }

    /// <summary> Constructor fixing x and y.
    /// </summary>
    /// <param name="x">The x coord.</param>
    /// <param name="y">The y coord.</param>
    public ScreenVector(int x, int y)
    {
      _x = x;
      _y = y;
    }

    #endregion

    #region Public properties

    /// <summary> Gets the x coordinate.
    /// </summary>
    public int X
    {
      get { return _x; }
      set { _x = value; }
    }

    /// <summary> Gets the y coordinate.
    /// </summary>
    public int Y
    {
      get { return _y; }
      set { _y = value; }
    }

    #endregion

    #region Projection

    /// <summary> Projects the given Euclidian point on the screen plane.
    /// </summary>
    /// <param name="ze">The euclidian point.</param>
    /// <param name="sOrigin">The origin of the screen plane.</param>
    /// <param name="sMax">The (xMax, yMax) point in the screen plane.</param>
    public void ProjectionEtoS(EuclidianVector ze, ScreenVector sOrigin, ScreenVector sMax)
    {
      _x = (int)Math.Round(ze.X * sMax._x) + sOrigin._x;
      _y = -(int)Math.Round(ze.Y * sMax._y) + sOrigin._y;
    }

    #endregion

    #region Zone containing 

    /// <summary> Is the given HtCoordS within the zone length of this HtCoordS ?
    /// </summary>
    /// <param name="zs"></param>
    /// <returns>true if it is, false otherwise</returns>
    public bool Contains(ScreenVector zs)
    {
      int __length = this.GetDistance(zs);
      return __length <= ZONE_LENGTH;
    }

    #endregion

    #region Distance

    /// <summary> Returns the distance between this point and the given point.
    /// </summary>
    /// <param name="z">The given point.</param>
    /// <returns>The distance.</returns>
    public int GetDistance(ScreenVector z)
    {
      int __d2 = (z._x - _x) * (z._x - _x) + (z._y - _y) * (z._y - _y);
      return (int)Math.Round(Math.Sqrt(__d2));
    }

    #endregion

    #region ToString

    public override string ToString()
    {
      return "(" + _x + " : " + _y + ")S";
    }

    #endregion

  }
}
