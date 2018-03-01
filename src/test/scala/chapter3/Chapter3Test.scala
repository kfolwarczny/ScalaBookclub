package chapter3

import org.scalatest.{FlatSpec, Matchers}

class Chapter3Test extends FlatSpec with Matchers {

  it should "append two lists" in {
    val l = FPList(1, 2, 3, 4)
    val l2 = FPList(5, 6, 7, 8)
    val expected = FPList(1, 2, 3, 4, 5, 6, 7, 8)

    FPList.append(l, l2) should be {
      expected
    }
  }

  it should "map value by 2" in {
    val l = FPList(1, 2, 3, 4)
    val expected = FPList(2, 4, 6, 8)

    assert(FPList.map(l)(_ * 2) == expected)
  }

  it should "filter all values below 3" in {
    val l = FPList(5, 2, 3, 6, 7, -1)
    val expected = FPList(5, 3, 6, 7)
    val rec = FPList.filter(l)(_ >= 3)

    assert(rec == expected)
  }

  it should "flatmap the list" in {
    val expected = FPList(1, 1, 2, 2, 3, 3)
    val rec = FPList.flatMap(FPList(1, 2, 3))(i => FPList(i, i))

    assert(rec == expected)
  }

  it should "filter all values below 3 with flatmap" in {
    val l = FPList(5, 2, 3, 6, 7, -1)
    val expected = FPList(5, 3, 6, 7)
    val rec = FPList.filterFM(l)(_ >= 3)

    assert(rec == expected)
  }

  it should "add els from one list to els from second list" in {
    val l1 = FPList(1, 2, 3, 4)
    val l2 = FPList(3, 4, 5, 6)
    val expected = FPList(4, 6, 8, 10)

    val result = FPList.addLists(l1, l2)

    assert(result == expected)
  }

  it should "add els from one list to els from second list with zipWith" in {
    val l1 = FPList(1, 2, 3, 4)
    val l2 = FPList(3, 4, 5, 6)
    val expected = FPList(4, 6, 8, 10)

    val result = FPList.zipWith(l1, l2)((x, y) => x + y)

    assert(result == expected)
  }

}
