data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show)

freeTree :: Tree Char  
freeTree =   
    Node 'P'  
        (Node 'O'  
            (Node 'L'  
                (Node 'N' Empty Empty)  
                (Node 'T' Empty Empty)  
            )  
            (Node 'Y'  
                (Node 'S' Empty Empty)  
                (Node 'A' Empty Empty)  
            )  
        )  
        (Node 'L'  
            (Node 'W'  
                (Node 'C' Empty Empty)  
                (Node 'R' Empty Empty)  
            )  
            (Node 'A'  
                (Node 'A' Empty Empty)  
                (Node 'C' Empty Empty)  
            )  
        )  
        
exprTree :: Tree String  
exprTree =   
    Node "*"  
        (Node "+"  
            (Node "10" Empty Empty)  
            (Node "2" Empty Empty)  
        )  
        (Node "-"  
            (Node "3" Empty Empty)  
            (Node "2" Empty Empty)  
        )
        
exprTree2 :: Tree String  
exprTree2 =
    Node "*"   
        (Node "-"  
            (Node "+"  
                (Node "10" Empty Empty)  
                (Node "3" Empty Empty)  
            )  
            (Node "1" Empty Empty) 
        ) 
        (Node "2" Empty Empty) 
      

data Crumb a = LeftCrumb a (Tree a) | RightCrumb a (Tree a) deriving (Show) 

type Breadcrumbs a = [Crumb a]  

type Zipper a = (Tree a, Breadcrumbs a)  

x |> f = f x


goLeft :: Zipper a -> Zipper a  
goLeft (Node x l r, bs) = (l, LeftCrumb x r:bs)  

goRight :: Zipper a -> Zipper a  
goRight (Node x l r, bs) = (r, RightCrumb x l:bs)

goUp :: Zipper a -> Zipper a  
goUp (t, LeftCrumb x r:bs) = (Node x t r, bs)  
goUp (t, RightCrumb x l:bs) = (Node x l t, bs) 

 