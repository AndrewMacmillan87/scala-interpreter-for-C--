# scala-interpreter-for-C--

This is an interpreter for the ficticious language C--, written in Scala

In C-- you can:

 - declare variables
 - perform calculations using an aritrary number of brackets
 - print a list of statements using the 'print' keyword
 - declare if/else statements
 - declare while loops
 
C-- syntax corresponds to that of C++<br />
C-- only has one type, <b>integer</b>, used for numerical and boolean (0 or 1) values

The available operators are:

<pre>
+     =     (
-     ==    )
/     !=    {
*     <     }
%     >     ,
      <=
      >=
      &&
      ||
</pre>

This sample C-- program demonstrates the syntax of the language:

<pre>
    <code>
        val = 104
	while (val >= 2) {
		if (val % 2 == 0) {
			next = val / 2
		} else {
			next = 3 * val + 1
		}
		print val, next
		val = next
        }
    </code>
 </pre>
 
This was a project during the 3rd year of my Computing BSc 
I plan to extend it further 
