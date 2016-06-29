package chapter4


object Exercise extends App{


  def mean(xs:Seq[Double]):Option[Double] = {
    if(xs.isEmpty) None
    else Some(xs.sum / xs.length)
  }

}
