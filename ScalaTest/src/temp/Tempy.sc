object Tempy {



//  def add(list:Seq[Int], carry: Boolean = false): Seq[Int] = {
//
//    list match {
//      case nil if(carry) => List(0)
//      case nil => nil
//      case head::nil if(head < 9) => list.map(_+1)
//      case head::nil => list.map(_+1) +: add (nil, true)
//      case head::tail if(head < 9) => head :: add (tail, false)
//      case head::tail if(carry) => 0 :: add (tail, true)
//      case head::tail => Seq(1) :: add (tail, true)
//    }
//  }

}