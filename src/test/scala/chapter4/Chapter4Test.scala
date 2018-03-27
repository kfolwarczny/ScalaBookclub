package chapter4

import org.scalatest.{FlatSpec, Matchers}

class Chapter4Test extends FlatSpec with Matchers {

  it should "map option in Some" in {
    val opt: Option[Int] = Some(1)

    opt.map(_ * 2) should be {
      Some(2)
    }
  }

  it should "map option in None" in {
    val opt: Option[Int] = None
    opt.map(_ * 2) should be {
      None
    }
  }

  it should "flatMap option in Some" in {
    val opt: Option[Int] = Some(1)

    opt.flatMap(x => Some(x * 2)) should be {
      Some(2)
    }
  }

  it should "flatMap option in None" in {
    val opt: Option[Int] = None
    opt.flatMap(x => Some(x * 2)) should be {
      None
    }
  }

  it should "get value in Some" in {
    val opt: Option[Int] = Some(1)

    opt.getOrElse(666) should be {
      1
    }
  }

  it should "return else in None" in {
    val opt: Option[Int] = None
    opt.getOrElse(666) should be {
      666
    }
  }

  it should "get Option[value] in Some" in {
    val opt: Option[Int] = Some(1)

    opt.orElse(Some(666)) should be {
      Some(1)
    }
  }

  it should "return Some[else] in None" in {
    val opt: Option[Int] = None
    opt.orElse(Some(666)) should be {
      Some(666)
    }
  }

  it should "return Some for value that match filter" in {
    val opt: Option[Int] = Some(3)

    opt.filter(_ > 2) should be {
      Some(3)
    }
  }

  it should "return None for value that doesn't match filter" in {
    val opt: Option[Int] = Some(1)
    opt.filter(_ > 2) should be {
      None
    }
  }

  it should "return None for None" in {
    val opt: Option[Int] = None
    opt.filter(_ > 2) should be {
      None
    }
  }

  it should "count variance" in {
    val s: Seq[Double] = Range.Double(0.0, 10.0, 1.0)

    Option.variance(s) should be {
      Some(8.25)
    }
  }

  it should "return None for empty seq" in {
    Option.variance(Seq.empty) should be {
      None
    }
  }

  it should "list[Some[a]] => Option[Some[a]]" in {

    val ls: List[Option[Int]] = Some(1) :: Some(2) :: Some(3) :: Nil

    Option.sequence(ls) should be {
      Some(1 :: 2 :: 3 :: Nil)
    }

  }

  it should "list[None[a]] => None" in {

    val ls: List[Option[Int]] = Some(1) :: None :: Some(3) :: Nil

    Option.sequence(ls) should be {
      None
    }

  }

  it should "Nil => None" in {
    Option.sequence(Nil) should be {
      None
    }
  }

  it should " traverse list[Some[a]] => Option[Some[a]]" in {

    val ls: List[Int] = 1 :: 2 :: 3 :: Nil

    Option.traverse(ls)(i => Some(i * 2)) should be {
      Some(2 :: 4 :: 6 :: Nil)
    }
  }

  it should " map  Right" in {
    Right(2).map(_ * 2) should be {
      Right(4)
    }
  }

  it should " map  Left" in {
    Left(2).map(x => x) should be {
      Left(2)
    }
  }
}
