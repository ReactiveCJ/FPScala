package gettingstarted

object MyModule {

  def abs(n:Int):Int = {

    if(n < 0) -n else n
  }

  private def formatResult(name:String,x:Int,f:Int =>Int):String = {
    s"The $name of $x is ${f(x)}"
  }

  @annotation.tailrec
  private def go(n:Int,acc:Int):Int = {
    if(n<=0)
      acc
    else
      go(n-1, n*acc)
  }

  def factorial(n:Int):Int = {
    go(n,1)
  }

  @annotation.tailrec
  def fibGo(n:Int,acc1:Int,acc2:Int):Int = {
    if(n <= 1)
      acc2
    else
      fibGo(n-1,acc2,acc1 + acc2)
  }

  def fib(n:Int):Int = {
    fibGo(n,0,1)
  }

  def main(args: Array[String]) {
    println(formatResult("absolute value", -42, abs))
    println(formatResult("factorial", 7, factorial))
    println(formatResult("fib",6 , fib))
  }



}
