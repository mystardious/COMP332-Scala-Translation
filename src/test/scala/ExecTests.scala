/*
 * This file is part of COMP332 Assignment 3 2019.
 *
 * Lintilla, a simple functional programming language.
 *
 * © 2019, Dominic Verity, Macquarie University, All rights reserved.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Execution tests.
 */

package lintilla

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
  * Tests to check that the execution of SEC machine code translated from
  * Lintilla source code gives the right output.
  */
@RunWith(classOf[JUnitRunner])
class ExecTests extends SemanticTests {

  import SECDTree._

  // Simple constants

  test("printing a constant integer gives the right output") {
    execTestInline("""
       |print(30)""".stripMargin, "30\n")
  }

  test("print constant integer gives the right translation") {
    targetTestInline("""
       |print(30)""".stripMargin, List(IInt(30), IPrint()))
  }

  test("printing a constant boolean gives the right output") {
    execTestInline("""
       |print(true)""".stripMargin, "true\n")
  }

  test("print constant boolean gives the right translation") {
    targetTestInline("""
       |print(true)""".stripMargin, List(IBool(true), IPrint()))
  }

  // Simple arithmetic expressions

  test("print simple addition expression") {
    execTestInline("""
       |print(30+10)""".stripMargin, "40\n")
  }

  test("print simple addition expression gives the right translation") {
    targetTestInline("""
       |print(30+10)""".stripMargin, List(IInt(30), IInt(10), IAdd(), IPrint()))
  }

  test("print simple multiplication expression") {
    execTestInline("""
       |print(6*7)""".stripMargin, "42\n")
  }

  test("print simple multiplication expression gives the right translation") {
    targetTestInline("""
       |print(6*7)""".stripMargin, List(IInt(6), IInt(7), IMul(), IPrint()))
  }

  test("print simple subtraction expression") {
    execTestInline("""
       |print(30-10)""".stripMargin, "20\n")
  }

  test("print simple subtraction expression gives the right translation") {
    targetTestInline("""
       |print(30-10)""".stripMargin, List(IInt(30), IInt(10), ISub(), IPrint()))
  }

  test("print simple division expression") {
    execTestInline("""
       |print(6/2)""".stripMargin, "3\n")
  }

  test("print simple division expression gives the right translation") {
    targetTestInline("""
       |print(6/2)""".stripMargin, List(IInt(6), IInt(2), IDiv(), IPrint()))
  }

  test("print simple negation expression") {
    execTestInline("""
       |print(-22)""".stripMargin, "-22\n")
  }

  test("print simple negation expression gives the right translation") {
    targetTestInline("""
       |print(-22)""".stripMargin, List(IInt(0), IInt(22), ISub(), IPrint()))
  }

  // Simple relation expressions

  test("print simple equality expression") {
    execTestInline("""
       |print(25=5)""".stripMargin, "false\n")
  }

  test("print simple equality expression gives the right translation") {
    targetTestInline("""
       |print(25=5)""".stripMargin, List(IInt(25), IInt(5), IEqual(), IPrint()))
  }

  test("print simple less expression") {
    execTestInline("""
       |print(7<9)""".stripMargin, "true\n")
  }

  test("print simple less expression gives the right translation") {
    targetTestInline("""
       |print(7<9)""".stripMargin, List(IInt(7), IInt(9), ILess(), IPrint()))
  }

  // More complex expressions

  test("print more complex expression") {
    execTestInline("""
       |print(10+5*6/2-21+2*-2)""".stripMargin, "0\n")
  }

  test("print more complex expression gives the right translation") {
    targetTestInline(
      """
       |print(10+5*6/2-21+2*-2)""".stripMargin,
      List(
        IInt(10),
        IInt(5),
        IInt(6),
        IMul(),
        IInt(2),
        IDiv(),
        IAdd(),
        IInt(21),
        ISub(),
        IInt(2),
        IInt(0),
        IInt(2),
        ISub(),
        IMul(),
        IAdd(),
        IPrint()
      )
    )
  }

  test("print more complex relational expression") {
    execTestInline("""
       |print((5 < 10) = (10 < 5))""".stripMargin, "false\n")
  }

  test("print more complex relational expression gives right translation") {
    targetTestInline(
      """
       |print((5 < 10) = (10 < 5))""".stripMargin,
      List(
        IInt(5),
        IInt(10),
        ILess(),
        IInt(10),
        IInt(5),
        ILess(),
        IEqual(),
        IPrint()
      )
    )
  }

  // Simple block translation

  test("block translates correctly") {
    targetTestInline(
      """
       |print 10;
       |{
       |   print 20;
       |   print 30
       |};
       |print 40""".stripMargin,
      List(
        IInt(10),
        IPrint(),
        IInt(20),
        IPrint(),
        IInt(30),
        IPrint(),
        IInt(40),
        IPrint()
      )
    )
  }

  test("nested block translates correctly") {
    targetTestInline(
      """
       |print 10;
       |{
       |   print 20;
       |   {
       |       print 30
       |   };
       |   print 40
       |};
       |print 50""".stripMargin,
      List(
        IInt(10),
        IPrint(),
        IInt(20),
        IPrint(),
        IInt(30),
        IPrint(),
        IInt(40),
        IPrint(),
        IInt(50),
        IPrint()
      )
    )
  }

  // `let` binding

  test("let binding gives right translation") {
    targetTestInline(
      """
       |let x = 20;
       |print x;
       |print x * x""".stripMargin,
      List(
        IInt(20),
        IClosure(
          None,
          List("x"),
          List(IVar("x"), IPrint(), IVar("x"), IVar("x"), IMul(), IPrint())
        ),
        ICall()
      )
    )
  }

  test("let binding body extends to end of block only") {
    targetTestInline(
      """
       |print 10;
       |{
       |    let x = 20;
       |    print x;
       |    print x * x
       |};
       |print 30""".stripMargin,
      List(
        IInt(10),
        IPrint(),
        IInt(20),
        IClosure(
          None,
          List("x"),
          List(IVar("x"), IPrint(), IVar("x"), IVar("x"), IMul(), IPrint())
        ),
        ICall(),
        IInt(30),
        IPrint()
      )
    )
  }

  test("let binds variable in rest of block") {
    execTestInline("""
       |let x = 10;
       |print x;
       |let y = x * x;
       |print y""".stripMargin, "10\n100\n")
  }

  test("let binding in block correctly shadows outer binding") {
    execTestInline(
      """
       |let x = 10;
       |print x;
       |{
       |    let x = 20;
       |    print x
       |};
       |print x""".stripMargin,
      "10\n20\n10\n"
    )
  }

  // `if` expression

  test("simple `if` expression gives right translation") {
    targetTestInline(
      """
       |if true { print 10 } else { print 20 }""".stripMargin,
      List(
        IBool(true),
        IBranch(
          List(IInt(10), IPrint()),
          List(IInt(20), IPrint())
        )
      )
    )
  }

  test("simple `if` expression evaluation (condition true)") {
    execTestInline("""
       |if (5 < 10) { print 10 } else { print 20 }""".stripMargin, "10\n")
  }

  test("simple `if` expression evaluation (condition false)") {
    execTestInline("""
       |if (5 = 10) { print 10 } else { print 20 }""".stripMargin, "20\n")
  }

  test("`let` binding correctly scoped in then block") {
    execTestInline(
      """
       |let x = 10;
       |if x = 10 { print x; let x = 20; print x }
       |     else { print x; let x = 30; print x };
       |print x""".stripMargin,
      "10\n20\n10\n"
    )
  }

  test("`let` binding correctly scoped in else block") {
    execTestInline(
      """
       |let x = 10;
       |if x = 5 { print x; let x = 20; print x }
       |    else { print x; let x = 30; print x };
       |print x""".stripMargin,
      "10\n30\n10\n"
    )
  }

  // Function binding

  test("`fn` binding gives correct translation") {
    targetTestInline(
      """
       |fn addone(n: int) -> int { n + 1 };
       |print addone;
       |print 10""".stripMargin,
      List(
        IClosure(Some("addone"), List("n"), List(IVar("n"), IInt(1), IAdd())),
        IClosure(
          None,
          List("addone"),
          List(IVar("addone"), IPrint(), IInt(10), IPrint())
        ),
        ICall()
      )
    )
  }

  test("`fn` binding extends to end of block only") {
    targetTestInline(
      """
       |let addone = 20;
       |{
       |    fn addone(n: int) -> int { n + 1 };
       |    print 10;
       |    print addone
       |};
       |print addone""".stripMargin,
      List(
        IInt(20),
        IClosure(
          None,
          List("addone"),
          List(
            IClosure(
              Some("addone"),
              List("n"),
              List(IVar("n"), IInt(1), IAdd())
            ),
            IClosure(
              None,
              List("addone"),
              List(IInt(10), IPrint(), IVar("addone"), IPrint())
            ),
            ICall(),
            IVar("addone"),
            IPrint()
          )
        ),
        ICall()
      )
    )
  }

  test("`fn` binding extends to end of block execution") {
    execTestInline(
      """
       |let addone = 20;
       |{
       |    fn addone(n: int) -> int { n + 1 };
       |    print addone
       |};
       |print addone""".stripMargin,
      "function of arguments (n)\n20\n"
    )
  }

  test("`fn` body with `let` binding translates correctly") {
    targetTestInline(
      """
        |fn local_test() {
        |    print 10;
        |    let x = 20;
        |    print x
        |};
        |print local_test""".stripMargin,
      List(
        IClosure(
          Some("local_test"),
          List(),
          List(
            IInt(10),
            IPrint(),
            IInt(20),
            IClosure(None, List("x"), List(IVar("x"), IPrint())),
            ICall()
          )
        ),
        IClosure(None, List("local_test"), List(IVar("local_test"), IPrint())),
        ICall()
      )
    )
  }

  // Function application
  test("simple function application translation") {
    targetTestInline(
      """
       |fn addone(n: int) -> int { n + 1 };
       |print addone(10)""".stripMargin,
      List(
        IClosure(Some("addone"), List("n"), List(IVar("n"), IInt(1), IAdd())),
        IClosure(
          None,
          List("addone"),
          List(IInt(10), IVar("addone"), ICall(), IPrint())
        ),
        ICall()
      )
    )
  }

  test("simple function application execution") {
    execTestInline("""
       |fn addone(n: int) -> int { n + 1 };
       |print addone(10)""".stripMargin, "11\n")
  }

  test("call a parameterless function") {
    execTestInline("""
       |fn noparam() { print 20 };
       |noparam()""".stripMargin, "20\n")
  }

  test("call a three parameter function") {
    execTestInline(
      """
       |fn threeparam(n: int, m: int, r: int) -> int {
       |    n + 10*m + 100*r
       |};
       |print threeparam(1,2,3)""".stripMargin,
      "321\n"
    )
  }

  test("curried function call") {
    execTestInline(
      """
       |fn curried(n: int) -> (fn(int, int) -> int) {
       |    fn aux(m: int, r: int) -> int {
       |        n + 10*m + 100*r
       |    };
       |    aux
       |};
       |print curried(1)(2,3)""".stripMargin,
      "321\n"
    )
  }

  // And -> Short-Circuit Evaluation
  test("'and' expression evaluates both b1 & b2 if both are true ") {
    execTestInline(
      """
        |let a = { print true; true } && { print true; true }
      """.stripMargin, "true\ntrue\n"
    )
  }
  test("'and' expression evaluates both b1 & b2 if b1 is true and b2 is false ") {
    execTestInline(
      """
        |let a = { print true; true } && { print false; false }
      """.stripMargin, "true\nfalse\n"
    )
  }
  test("'and' expression evaluates only b1 if it is false") {
    execTestInline(
      """
        |let a = { print false; false } && { print false; false }
      """.stripMargin, "false\n"
    )
  }

  // And -> Truth Table
  test("'and' expression returns true if both b1 & b2 are true") {
    execTestInline(
      """
        |print true && true
      """.stripMargin, "true\n"
    )
  }
  test("'and' expression returns false if both b1 & b2 are false") {
    execTestInline(
      """
        |print false && false
      """.stripMargin, "false\n"
    )
  }
  test("'and' expression returns false if b1 is false & b2 is false") {
    execTestInline(
      """
        |print false && true
      """.stripMargin, "false\n"
    )
  }
  test("'and' expression returns false if b1 is true & b2 is false") {
    execTestInline(
      """
        |print true && false
      """.stripMargin, "false\n"
    )
  }

  // Or -> Short-Circuit Evaluation
  test("'or' expression evaluates both b1 & b2 if both are false") {
    execTestInline(
      """
        |let a = { print false; false } || { print false; false }
      """.stripMargin, "false\nfalse\n"
    )
  }
  test("'or' expression evaluates both b1 & b2 if b1 is false and b2 is true") {
    execTestInline(
      """
        |let a = { print false; false } || { print true; true }
      """.stripMargin, "false\ntrue\n"
    )
  }
  test("'or' expression evaluates only b1 if it is true") {
    execTestInline(
      """
        |let a = { print true; true } || { print false; false }
      """.stripMargin, "true\n"
    )
  }

  // Or -> Truth Table
  test("'or' expression returns true if both b1 & b2 are true") {
    execTestInline(
      """
        |print true || true
      """.stripMargin, "true\n"
    )
  }
  test("'or' expression returns false if both b1 & b2 are false") {
    execTestInline(
      """
        |print false || false
      """.stripMargin, "false\n"
    )
  }
  test("'or' expression returns true if b1 is true & b2 is false") {
    execTestInline(
      """
        |print true || false
      """.stripMargin, "true\n"
    )
  }
  test("'or' expression returns true if b1 is false & b2 is true") {
    execTestInline(
      """
        |print false || true
      """.stripMargin, "true\n"
    )
  }

  // Not Expressions
  test("'not' expression return true if used on false") {
    execTestInline(
      """
        |print ~false
      """.stripMargin, "true\n"
    )
  }
  test("'not' expression return false if used on true") {
    execTestInline(
      """
        |print ~true
      """.stripMargin, "false\n"
    )
  }

  // Array -> Initialisation
  test("creating an array results in an empty array") {
    execTestInline(
      """
        |let a = array int;
        |print a
      """.stripMargin, "empty array\n"
    )
  }

  // Array -> Adding items to the array using AppendExp
  test("adding one item to an empty array results in an array with that one item") {
    execTestInline(
      """
        |let v = array int;
        |v += 0;
        |print v
      """.stripMargin, "array containing one entry\n"
    )
  }
  test("adding two items to an empty array results in an array with two items") {
    execTestInline(
      """
        |let v = array int;
        |v += 0;
        |v += 0;
        |print v
      """.stripMargin, "array containing 2 entries\n"
    )
  }
  test("adding three items to an empty array results in an array with three items") {
    execTestInline(
      """
        |let v = array int;
        |v += 0;
        |v += 0;
        |v += 0;
        |print v
      """.stripMargin, "array containing 3 entries\n"
    )
  }
  test("adding four items to an empty array results in an array with four items") {
    execTestInline(
      """
        |let v = array int;
        |v += 0;
        |v += 0;
        |v += 0;
        |v += 0;
        |print v
      """.stripMargin, "array containing 4 entries\n"
    )
  }

  // Array -> Return values in the array using DerefExp
  test("using the dereferencing character followed by an valid index on an array should return the value at that index in the array") {
    execTestInline(
     """
       |let v = array int;
       |v += 0;
       |print v!0
     """.stripMargin, "0\n"
    )
  }

  // Array -> Assign values to indexes in the array using AssignExp
  test("assigning a value to a invalid index should return an error") {
    execTestInline(
      """
        |let v = array int;
        |v!0 := 1;
        |print 1
      """.stripMargin, "FatalError: array index out of bounds\n"
    )
  }
  test("assigning a value to a valid index should update that value in the array") {
    execTestInline(
      """
        |let v = array int;
        |v += 0;
        |v!0 := 1;
        |print v!0
      """.stripMargin, "1\n"
    )
  }

  // Array -> Return the number of entries in the array using LengthExp
  test("the length of an array with no items should be 0") {
    execTestInline(
      """
        |let v = array int;
        |print length(v)
      """.stripMargin, "0\n"
    )
  }
  test("the length of an array with n items should be n") {
    execTestInline(
      """
        |let v = array int;
        |
        |for i = 1 to 100 do {
        |    v += i;
        |    print length(v)
        |}
      """.stripMargin, "1\n2\n3\n4\n5\n6\n7\n8\n9\n10\n11\n12\n13\n14\n15\n16\n17\n18\n19\n20\n21\n22\n23\n24\n25\n26\n27\n28\n29\n30\n31\n32\n33\n34\n35\n36\n37\n38\n39\n40\n41\n42\n43\n44\n45\n46\n47\n48\n49\n50\n51\n52\n53\n54\n55\n56\n57\n58\n59\n60\n61\n62\n63\n64\n65\n66\n67\n68\n69\n70\n71\n72\n73\n74\n75\n76\n77\n78\n79\n80\n81\n82\n83\n84\n85\n86\n87\n88\n89\n90\n91\n92\n93\n94\n95\n96\n97\n98\n99\n100\n"
    )
  }

  // For -> Step Values
  test("'for' expression simple count") {
    execTestInline(
      """
        |for i = 1 to 5 do {
        |    print i
        |}
      """.stripMargin, "1\n2\n3\n4\n5\n"
    )
  }
  test("'for' expression simple count down using step_value") {
    execTestInline(
      """
        |for i = 5 to 1 step -1 do {
        |    print i
        |}
      """.stripMargin, "5\n4\n3\n2\n1\n"
    )
  }
  test("'for' expression simple count using larger step value") {
    execTestInline(
      """
        |for i = 1 to 10 step 2 do {
        |    print i
        |}
      """.stripMargin, "1\n3\n5\n7\n9\n"
    )
  }
  test("'for' expression simple count down using larger step value") {
    execTestInline(
      """
        |for i = 10 to 1 step -2 do {
        |    print i
        |}
      """.stripMargin, "10\n8\n6\n4\n2\n"
    )
  }

  // For -> Loop
  test("'loop' at the begining of a loop will not execute anything further within that iteration") {
    execTestInline(
      """
        |for i = 0 to 4 do {
        |    loop;
        |    print i
        |}
      """.stripMargin, ""
    )
  }
  test("using 'loop' at the begining of a for loop will not execute anything further within that iteration") {
    execTestInline(
      """
        |for i = 0 to 4 do {
        |    loop;
        |    print i
        |}
      """.stripMargin, ""
    )
  }
  test("using 'loop' at any point in a for loop will not execute anything else for that particular iteration") {
    execTestInline(
      """
        |for i = 0 to 4 do {
        |    print i;
        |    loop;
        |    print i + 1
        |  }
      """.stripMargin, "0\n1\n2\n3\n4\n"
    )
  }

  // For -> Break
  test("using 'break' will exit a for loop completely") {
    execTestInline(
      """
        |for i = 0 to 4 do {
        |    print i;
        |    break
        |}
      """.stripMargin, "0\n"
    )
  }

  // FIXME: Tests of short-circuited evaluation of '&&', '||' and '~'.

  // FIXME: Tests of execution of array operations

  // FIXME: Tests of execution of 'for' loops, 'break' and 'loop' constructs.

  // Bigger examples.

  test("factorial example") {
    execTestFile("src/test/resources/factorial.lin", "120\n")
  }

  test("fibonacci example") {
    execTestFile("src/test/resources/fibonacci.lin", "55\n")
  }

  test("higher order example") {
    execTestFile("src/test/resources/iterate.lin", "135\n50\n27\n")

  }

  test("while loop example") {
    execTestFile(
      "src/test/resources/while.lin",
      "2\n7\n22\n67\n202\n607\n1822\n5467\n16402\n"
    )
  }

  test("snippets example") {
    execTestFile(
      "src/test/resources/snippets.lin",
      "1\n1\n2\n1\n3\n43\n6\nfunction of arguments (b)\n10\n10\n5\n15\n"
    )
  }

}
