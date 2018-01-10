{-# language DeriveFunctor #-}
{-# language FlexibleInstances #-}
{-# language RankNTypes #-}
module Data.Zipper.TwoD where

import Control.Comonad
import Control.Lens
import Data.Maybe
import Data.Profunctor

data Loc a
  = At
  { l_it :: a
  , l_left :: Maybe (Loc a)
  , l_right :: Maybe (Loc a)
  , l_up :: Maybe (Loc a)
  , l_down :: Maybe (Loc a)
  }
  deriving Functor

instance Comonad Loc where
  extract = l_it
  duplicate a@(At _ l r u d) = here
    where
      here =
        At
          a
          (fromRight here <$> l)
          (fromLeft here <$> r)
          (fromBottom here <$> u)
          (fromTop here <$> d)

      fromBottom below a'@(At _ l' r' u' _) = here'
        where
          here' =
            At
              a'
              (fromRight here' <$> l')
              (fromLeft here' <$> r')
              (fromBottom here' <$> u')
              (Just below)
      fromRight right a'@(At _ l' _ u' d') = here'
        where
          here' =
            At
              a'
              (fromRight here' <$> l')
              (Just right)
              (fromBottom here' <$> u')
              (fromTop here' <$> d')
      fromLeft left a'@(At _ _ r' u' d') = here'
        where
          here' =
            At
              a'
              (Just left)
              (fromLeft here' <$> r')
              (fromBottom here' <$> u')
              (fromTop here' <$> d')
      fromTop above a'@(At _ l' r' _ d') = here'
        where
          here' =
            At
              a'
              (fromRight here' <$> l')
              (fromLeft here' <$> r')
              (Just above)
              (fromTop here' <$> d')

weaveRow
  :: Maybe (Loc a) -- Thing to left
  -> [Maybe (Loc a)] -- things above
  -> [a] -- things to right
  -> [Maybe (Loc a)] -- things below
  -> [Loc a]
weaveRow l _ [] _ = []
weaveRow l ys (x:xs) zs = here : rights
  where
    (y',ys') = case ys of
      [] -> (Nothing, ys)
      a:as -> (a, as)
    (z',zs') = case zs of
      [] -> (Nothing, zs)
      a:as -> (a, as)
    rights = weaveRow (Just here) ys' xs zs'
    here = At x l (listToMaybe rights) y' z'

weaveColumn
  :: [Maybe (Loc a)] -- Things above
  -> [[a]] -- things below
  -> [[Loc a]]
weaveColumn above [] = []
weaveColumn above (x:xs) = here : belows
  where
    belows = weaveColumn (Just <$> here) xs
    b = case belows of
      [] -> []
      a:_ -> a
    here = weaveRow Nothing above x (Just <$> b)

zipGrid :: [[a]] -> Maybe (Loc a)
zipGrid xs =
  case weaveColumn [] xs of
    ((x:_):_) -> Just x
    _ -> Nothing

data Loc' b a
  = At'
  { l_it' :: a
  , l_left' :: Maybe (b -> Loc' b a)
  , l_right' :: Maybe (b -> Loc' b a)
  , l_up' :: Maybe (b -> Loc' b a)
  , l_down' :: Maybe (b -> Loc' b a)
  }
  deriving Functor

instance Profunctor Loc' where
  rmap = fmap
  lmap f (At' a l r u d) =
    At'
      a
      (fmap (dimap f (lmap f)) l)
      (fmap (dimap f (lmap f)) r)
      (fmap (dimap f (lmap f)) u)
      (fmap (dimap f (lmap f)) d)

focus :: Loc' b a -> a
focus l = l_it' l

left :: Loc' a a -> Maybe (Loc' a a)
left l = l_left' l <*> pure (l_it' l)

right :: Loc' a a -> Maybe (Loc' a a)
right l = l_right' l <*> pure (l_it' l)

up :: Loc' a a -> Maybe (Loc' a a)
up l = l_up' l <*> pure (l_it' l)

down :: Loc' a a -> Maybe (Loc' a a)
down l = l_down' l <*> pure (l_it' l)

update :: (a -> a) -> Loc' a a -> Loc' a a
update f l = l { l_it' = f (l_it' l) }

goRight = \loc arg -> loc { l_left' = Just $ goLeft (loc { l_it' = arg}) }
goLeft = \loc arg -> loc { l_right' = Just $ goRight (loc { l_it' = arg}) }
goUp = \loc arg -> loc { l_down' = Just $ goDown (loc { l_it' = arg}) }
goDown = \loc arg -> loc { l_up' = Just $ goUp (loc { l_it' = arg}) }

weaveRow'
  :: Maybe (Loc' a a) -- Left
  -> [Maybe (Loc' a a)] -- Aboves
  -> [a] -- Rights
  -> [Maybe (Loc' a a)] -- Belows
  -> [(Loc' a a)]
weaveRow' leftOf _ [] _ = []
weaveRow' leftOf above (x:xs) below = here : rights
  where
    rights = weaveRow' (Just here) as xs bs

    (a, as) = case above of
      [] -> (Nothing, above)
      a:as -> (a, as)

    (b, bs) = case below of
      [] -> (Nothing, below)
      b:bs -> (b, bs)

    here =
      At'
        x
        (goLeft <$> leftOf)
        (goRight <$> listToMaybe rights)
        (goUp <$> a)
        (goDown <$> b)

weaveColumn'
  :: [Maybe (Loc' a a)] -- Aboves
  -> [[a]] -- Belows
  -> [[Loc' a a]]
weaveColumn' _ [] = []
weaveColumn' aboves (x:xs) = here : belows
  where
    belows = weaveColumn' (Just <$> here) xs
    here = weaveRow' Nothing aboves x (Just <$> foldr const [] belows)

zipGrid' :: [[a]] -> Maybe (Loc' a a)
zipGrid' g =
  case weaveColumn' [] g of
    (l:_):_ -> Just l
    _ -> Nothing

(<->) :: Iso' a b -> Loc' a a -> Loc' b b
(<->) f = dimap (view $ from f) (view f)
