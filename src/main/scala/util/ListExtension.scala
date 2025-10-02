package util

// Helper extension for traversing lists with Options

extension [A](list: List[A])
    /** 
     * Transforms `List[A]` into `List[B]` wrapped in an Option. 
     * 
     * During iteration, the method calls `f` on each element of the list.
     * If all applications succeed (return `Some` value), it returns `Some`
     * containing the list of all results. If any application fails (returns `None`),
     * the entire traversal short-circuits and returns `None`.
     * 
     * Extension method for generic List[A]. 
     * 
     * Inspired by monadic traverse operation from the cats library: 
     * https://www.javadoc.io/doc/org.typelevel/cats-docs_2.13/latest/cats/Traverse.html
     *
     * @param B the result type of the transformation
     * @param f the function to apply to each element, which may fail, returning `None`
     * @return `Some(List[B])` if all transformations succeed, `None` if any fails
     */
    def traverse[B](f: A => Option[B]): Option[List[B]] =
        list.foldRight(Some(Nil): Option[List[B]]) { (a, acc) =>
            for
                bs <- acc
                b <- f(a)
            yield 
                b :: bs
        }
