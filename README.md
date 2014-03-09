Type promotion in haskell
=========================

Like in Haskell, the arithmetic operations of C++ always act on identical types (ex: <tt>2 + 2</tt> or <tt>2.0 + 2.0</tt>, but not <tt>2 + 2.0</tt>).

Unlike Haskell, C++ automatically promotes the arguments of arithmetic operations to the most general type. Lest it be thought that C++ is superior to Haskell in this regard, here is an implementation of type promotion in Haskell.


Implementation
--------------

I use type families to compute the resulting type, e.g. <tt>Promote Int Double = Double</tt> because the result of <tt>2 + 2.0</tt> is a <tt>Double</tt>.

One downside of this implementation strategy is that we need to define a quadratic number of instances, one for each pair of compatible types. To avoid doing this, I use Template Haskell to generate all the instances.
