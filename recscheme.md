# Recursion Schemes

So recursion schemes are pretty cool. Basically, they're a way of abstracting away the common recursion patterns you often find in functional code. Take, for instance, an `Expression` type, which can be evaluated:

    data UExpr = Const Integer
               | Neg UExpr
               | Prod UExpr UExpr
               | Sum UExpr UExpr
               deriving (Eq, Ord)
              
    eval :: UExpr -> Integer
    eval (Const n)  = n
    eval (Neg a)    = negate (eval a)
    eval (Sum a b)  = eval a + eval b
    eval (Prod a b) = eval a * eval b
    
The common pattern here is that you recur over your expression until you reach a `Const` term. Here's the equivalent using a recursion scheme:

    data ExprF r = CstF Integer
                 | NegF r
                 | PrdF r r
                 | SumF r r
                 deriving (Functor, Foldable, Eq, Ord, Show)
             
    newtype Expr = Expr { getExpr :: Fix ExprF }

    eval :: Expr -> Integer
    eval = cata alg . getExpr where
      alg (CstF d)   = d
      alg (NegF a)   = negate a
      alg (SumF a b) = a + b
      alg (PrdF a b) = a * b
      
The benefit isn't immediately clear. However, you'll notice that there's no explicit recursion in the second example: all that's described in the `eval` function is the *shape* of the recursion, not its explicit semantics.

So far, so boring. The recursion-scheme version is, if anything, *worse* than the original. It gets a little more interesting if we write a `Show` instance for both:

    instance Show UExpr where
      show = show . ppr where
        ppr e = case e of
          Const n  -> integer n
          Neg a    -> char '-' <> parens (ppr a)
          Prod a b -> parens (ppr a) <+> char '*' <+> parens (ppr b)
          Sum  a b -> parens (ppr a) <+> char '+' <+> parens (ppr b)
          
    instance Show Expr where
      show = show . cata ppr . getExpr where
        ppr e = case e of
          CstF n   -> integer n
          NegF a   -> char '-' <> parens a
          PrdF a b -> parens a <+> char '*' <+> parens b
          SumF a b -> parens a <+> char '+' <+> parens b
          
Here, the recursion-scheme version looks a little better: once there's more than one recursive call per case, it starts to look worthwhile.

However, this pretty-printer isn't very pretty:

    Sum (Const 2) (Const 3)
    -- (2) + (3)
    
It's over-parenthesising the expressions. If the sub-expression has higher precedence than the outer expression, it doesn't need any parens. So we need to add some sense of "precedence" to our expressions:

    prec :: UExpr -> Int
    prec e = case e of
      Const _  -> 4
      Neg  _   -> 3
      Prod _ _ -> 2
      Sum  _ _ -> 1
      
    instance Show UExpr where
      show = show . ppr where
        ppr e = case e of
          Const n  -> integer n
          Neg a    -> char '-' <> par a
          Prod a b -> par a <+> char '*' <+> par b
          Sum  a b -> par a <+> char '+' <+> par b
          where par a = if prec e > prec a then parens (ppr a) else ppr a
          
    Prod (Sum (Const 2) (Const 3)) (Const 3)
    -- (2 + 3) * 3
          
This works fine, but I don't like having a `prec` function. It's error-prone, and seems ugly. In reality, I'd like to use the `deriving Ord` on the expression type, so that the precedence of the operations was encoded in the order they were written, i.e:

    let p = Prod (Const 2) (Const 3)
    let s = Sum (Const 2) (Const 3)
    p < s == True
    
However, this only works if the top-level expressions being compared are of different types. It doesn't, say, give the same precedence for two `Prod` expressions.

What I really want is the basic expression with its contents removed: just a kind of shell. `ExprF ()` fits that description quite well: what's more, I ca generate an `ExprF ()` from any `ExprF a` with the `void` function.

Expressing this in a recursion-scheme is a little more complicated than the example above. Some recursive functions look something like this:

    rec :: Type -> Something
    rec (One a b) = f (rec a) (rec b)
    rec (Two a b) = g (rec a) (rec b)
    
In other words, all that's used on the right-hand-side of the equation are the results of recursive calls on the contents of the left-hand-side.

However, sometimes the pattern is a little different. Sometimes you use the contents *without* making recursive calls on them. In other words:

    rec :: Type -> Something
    rec (One a b) = f a (rec b)
    rec (Two a b) = g a (rec b)
    
Or, sometimes you use the whole constructor itself:

    rec :: Type -> Something
    rec c@(One _ b) = f c (rec b)
    rec c@(Two _ b) = g c (rec b)
    
This pattern is called a paramorphism, rather than catamorphism. The pretty-printing follows this pattern, because you need to use the outer expression to find its precedence.

Actually, we're not using the *whole* outer expression. We call `prec` on it every time. This pattern, where you use the result of calling a function on something on the left-hand-side of the equation is called a zygomorphism. That's the recursion scheme I'll use. The function that will get called on the outer expression is `void`, to give the shell expression which I'll use for precedence.

    instance Show Expr where 
      show = show . zygo void alg . getExpr where
        alg e = case e of
          CstF i   -> integer i
          NegF a   -> char '-' <> par a
          SumF a b -> par a <+> char '+' <+> par b
          PrdF a b -> par a <+> char '*' <+> par b
          where par (c,p) = if void e < c then parens p else p
          
