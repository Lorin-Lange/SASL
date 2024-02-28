{-|
Module      : Utilities.GraphVisualisation
Description : GraphVisualisation
Copyright   : (c) Lorin Lange, 2022
                  Simon Klingler, 2022
Maintainer  : lorin.lange@student.uni-tuebingen.de, 
              simon.klingler@student.uni-tuebingen.de
Stability   : experimental

TODO() : Longer description
-}

{-# LANGUAGE OverloadedStrings #-}

module Utilities.GraphVisualisation where

import Data.Graph.Inductive ( Graph(mkGraph), Gr, LEdge, LNode )
import Data.GraphViz.Types.Generalised as G ()
import Data.GraphViz.Types.Monadic ()
import Data.Text.Lazy as L ( Text )
import Utilities.Types as T
 (Graph, Pointer (deref),
 SASL (Builtin, (:@:), Def, Pair))
import Data.Map (toList)
import qualified Control.Monad.Cont as M

import qualified Data.Text.Lazy as L
import Data.GraphViz
 (GraphvizParams (globalAttributes, fmtNode, fmtEdge),
 GlobalAttributes (GraphAttrs, NodeAttrs),
 textLabel,
 X11Color (White),
 filled,
 style,
 shape,
 nonClusteredParams,
 GraphvizOutput (Pdf),
 runGraphviz,
 graphToDot)
import Data.GraphViz.Attributes.Colors
    ( ColorList, Color(RGB), toColorList, toWColor )
import Data.GraphViz.Attributes (dashed, solid)
import Data.GraphViz.Attributes.Complete
    ( Attribute(FillColor, RankDir, BgColor),
      Shape(BoxShape),
      RankDir(FromTop) )

run :: FilePath -> T.Graph -> IO ()
run path g = M.void $ runGraphviz d Pdf path
  where
    lst = toList g
    d = graphToDot ex1Params (mkGraph (labels lst) 
              (edges' lst) :: Gr Text (Bool, Text))


labels :: [(Pointer, SASL)] -> [LNode Text]
labels []         = []
labels ((p,t):xs) = node ++ labels xs
  where
    node = case t of
             Builtin b -> [(fromIntegral (deref p), L.pack $ show b)]
             _ :@: _   -> [(fromIntegral (deref p), "@")]
             Def {}    -> [(fromIntegral (deref p), "Def")]
             Pair {}   -> [(fromIntegral (deref p), "Pair")]


edges' :: [(Pointer, SASL)] -> [LEdge (Bool, Text)]
edges' []         = []
edges' ((p,t):xs) = s ++ edges' xs
  where
    s = case t of
          Builtin _  -> []
          l :@: r    -> [(fromIntegral $ deref p, 
                          fromIntegral $ deref l, 
                          (True, L.pack $ show $ deref l)), 
                         (fromIntegral $ deref p, 
                          fromIntegral $ deref r, 
                          (False, L.pack $ show $ deref r))]
          Def ds e _ -> (fromIntegral $ deref p, 
                         fromIntegral $ deref e, 
                         (False, L.pack $ show $ deref e)) : defList p ds
          Pair e1 e2 -> [(fromIntegral $ deref p, 
                          fromIntegral $ deref e1, 
                          (False, L.pack $ show $ deref e1)),
                         (fromIntegral $ deref p, 
                          fromIntegral $ deref e2,
                          (False, L.pack $ show $ deref e2))
                        ]


defList :: Pointer -> [([String], Pointer)] -> [LEdge (Bool, Text)]
defList _ []             = []
defList s ((idfrs,p):xs) = (fromIntegral (deref s), 
                            fromIntegral (deref p), 
                            (False, label)) : defList s xs
  where
    label = L.pack $ Prelude.unwords idfrs


ex1Params :: GraphvizParams n L.Text (Bool, L.Text) () L.Text
ex1Params = nonClusteredParams { globalAttributes = ga
                               , fmtNode          = fn
                               , fmtEdge          = fe
                               }
  where fn (_,l)   = [textLabel l]
        fe (_,_,(b, l)) = [textLabel l, style $ if b then dashed else solid]
        ga = [ GraphAttrs [ RankDir   FromTop
                          , BgColor   [toWColor White]
                          ]
             , NodeAttrs  [ shape     BoxShape
                          , FillColor color
                          , style     filled
                          ]
             ]

salmonColor :: ColorList
salmonColor = toColorList [RGB 250 128 114]

color :: ColorList
color = toColorList [RGB 204 230 255]