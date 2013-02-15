{-# LANGUAGE RecordWildCards #-}
module Data.GraphViz.Graphml where

import Control.Monad.State
import Data.Maybe
import Data.Functor
import Control.Arrow (first)
import Data.GraphViz.Types
import Data.GraphViz.Types.Canonical
import Data.GraphViz.Attributes.Complete
import qualified Data.Text.Lazy as T
import qualified Data.GraphViz.Attributes.Colors as AC
import Data.Monoid
import Text.XML.Generator
import qualified Data.ByteString as B
import qualified Data.Colour.SRGB as SRGB
import qualified Data.Colour as C
import qualified Data.GraphViz.Parsing as DP
import qualified Data.Text.Lazy.IO as TIO
import qualified Data.Map as M


convert :: DotGraph String -> Xml Doc
convert dg = doc defaultDocInfo $ xelem "graphml" $
             xattr "xmlns" "http://graphml.graphdrawing.org/xmlns" <>
             xattr "xmlns:y" "http://www.yworks.com/xml/graphml" <#>
             xelem "key" (xattr "for" "node" <>
                          xattr "id" "d6" <>
                          xattr "yfiles.type" "nodegraphics") <>
             xelem "key" (xattr "for" "edge" <>
                          xattr "id" "d10" <>
                          xattr "yfiles.type" "edgegraphics") <>
             handleStmts (graphStatements dg)

handleStmts :: DotStatements String -> Xml Elem
handleStmts DotStmts{..} =
    xelem "graph" (xattr "edgedefault" "directed" <>
                   xattr "id" "G" <#>
                   mconcat (map (handleNode nmap) nodeStmts) <>
                   mconcat (map handleOrphanNode (M.assocs orphans)) <>
                   mconcat (map (handleEdge nmap) $
                            zip [0..] edgeStmts))
  where
    nmap' = M.fromList $ map (first nodeID) $ zip nodeStmts [0..]
    nmap nn = case M.lookup nn nmap' of
                Just nid -> "n" ++ show nid
                Nothing -> "n" ++ show (orphans M.! nn)
    orphans = M.fromList $ flip zip [length nodeStmts ..] $
              filter (not . flip M.member nmap') $
              foldr (\s a -> fromNode s:toNode s:a) [] edgeStmts

handleNode :: (String -> String) -> DotNode String -> Xml Elem
handleNode nodeName DotNode{nodeID=nn,nodeAttributes=attrs} =
    xelem "node" (xattr "id" (nodeName nn) <#>
                  xelem "data" (xattr "key" "d6" <#>
                                yNodeToXml (handleNodeAttrs nn attrs)))

handleOrphanNode :: (String, Int) -> Xml Elem
handleOrphanNode (nn, nid) = xelem "node" (xattr "id" ("n" ++ show nid) <#>
                                           xelem "data" (xattr "key" "d6" <#>
                                                         yNodeToXml oattrs))
  where
    oattrs = defaultYNode { yLabel = nn }

data YNode = YNode { yBgColor :: String
                   , yFgColor :: String
                   , yBorderColor :: String
                   , yBorderWidth :: Double
                   , yBorderStyle :: String
                   , yLabel   :: String
                   , yShape :: String
                   }

defaultYNode :: YNode
defaultYNode = YNode { yBgColor = "#ffffff"
                     , yFgColor = "#000000"
                     , yBorderColor = "#000000"
                     , yBorderWidth = 1
                     , yBorderStyle = "line"
                     , yLabel = ""
                     , yShape = "ellipse"
                     }

yNodeToXml :: YNode -> Xml Elem
yNodeToXml YNode{..} =
  xelem "y:ShapeNode" $ xelems
    [xelem "y:Fill" (xattr "color" yBgColor <> xattr "transparent" "false")
    ,xelem "y:BorderStyle" (xattr "color" yBorderColor <>
                            xattr "type" yBorderStyle <>
                            xattr "width" (show yBorderWidth))
    ,xelem "y:NodeLabel" (xattr "textColor" yFgColor <#>
                          xtext yLabel)
    ,xelem "y:Shape" (xattr "type" yShape)
    ]

color2hex :: AC.Color -> String
color2hex c = fromMaybe "#000000" $
              (SRGB.sRGB24show . (`C.over` C.black)) <$> AC.toColour c

wcolor2hex :: WeightedColor -> String
wcolor2hex (WC c _) = color2hex c

handleNodeAttrs :: String -> Attributes -> YNode
handleNodeAttrs defLbl = foldr go defaultYNode { yLabel = defLbl }
  where go attr s = case attr of
          Color (c:_) -> s { yBorderColor = wcolor2hex c }
          BgColor (c:_) -> s { yBgColor = wcolor2hex c }
          FillColor (c:_) -> s { yBgColor = wcolor2hex c }
          FontColor c -> s { yFgColor = color2hex c }
          Label (StrLabel l) -> s { yLabel = T.unpack l }
          Shape sh -> s { yShape = shapeName sh }
          Style (i:is) -> applyStyle i $ go (Style is) s
          _ -> s

        shapeName sh = case sh of BoxShape      -> "rectangle"
                                  MRecord       -> "roundrectangle"
                                  Ellipse       -> "ellipse"
                                  Hexagon       -> "hexagon"
                                  Octagon       -> "octagon"
                                  DiamondShape  -> "diamond"
                                  Triangle      -> "triangle"
                                  Trapezium     -> "trapezoid"
                                  InvTrapezium  -> "trapezoid2"
                                  Box3D         -> "rectangle3d"
                                  Parallelogram -> "parallelogram"
                                  _ -> "rectangle"

        applyStyle (SItem sname _) s = case sname of
          Dashed -> s { yBorderStyle = "dashed" }
          Dotted -> s { yBorderStyle = "dotted" }
          Solid  -> s { yBorderStyle = "line" }
          Bold   -> s { yBorderWidth = 2 }
          _ -> s
                                          

handleEdge :: (String -> String) -> (Int, DotEdge String) -> Xml Elem
handleEdge nodeName (eid, DotEdge{..}) =
    xelem "edge" $
    xattr "id" ("e" ++ show eid) <>
    xattr "source" (nodeName fromNode) <>
    xattr "target" (nodeName toNode) <#>
    xelem "data" (xattr "key" "d10" <#>
                  yEdgeToXml (handleEdgeAttrs edgeAttributes))


data YEdge = YEdge { yeWidth :: Double
                   , yeStyle :: String
                   , yeColor :: String
                   , yeArrSrc :: Maybe String
                   , yeArrDst :: Maybe String
                   , yeDir    :: DirType
                   , yeLabel :: Maybe String
                   , yeLblColor :: String
                   }

defaultYEdge :: YEdge
defaultYEdge = YEdge { yeWidth = 1
                     , yeStyle = "line"
                     , yeColor = "#000000"
                     , yeArrSrc = Nothing
                     , yeArrDst = Nothing
                     , yeDir    = Forward
                     , yeLabel = Nothing
                     , yeLblColor = "#000000"
                     }

handleEdgeAttrs :: Attributes -> YEdge
handleEdgeAttrs = foldr go defaultYEdge
  where go attr s = case attr of
          Color (c:_) -> s { yeColor = wcolor2hex c }
          ArrowHead (AType ((_,sh):_)) -> s { yeArrDst = Just (arrShape sh) }
          ArrowTail (AType ((_,sh):_)) -> s { yeArrSrc = Just (arrShape sh) }
          Dir dir -> s { yeDir = dir }
          Label (StrLabel l) -> s { yeLabel = Just (T.unpack l) }
          FontColor c -> s { yeLblColor = color2hex c }
          Style (i:is) -> applyStyle i (go (Style is) s)
          _ -> s

        arrShape sh = case sh of Normal  -> "delta"
                                 Tee     -> "dash"
                                 Diamond -> "diamond"
                                 NoArrow -> "none"
                                 Vee     -> "plain"
                                 DotArrow -> "concave"
                                 Crow    -> "crows_foot_many"
                                 Inv     -> "crows_foot_many_mandatory"
                                 Box     -> "convex"   -- MiniHack

        applyStyle (SItem sname _) s = case sname of
          Dashed -> s { yeStyle = "dashed" }
          Dotted -> s { yeStyle = "dotted" }
          Bold   -> s { yeWidth = 2 }
          _ -> s


yEdgeToXml :: YEdge -> Xml Elem
yEdgeToXml YEdge{..} =
    xelem "y:PolyLineEdge" $
    xelem "y:Arrows" (xattr "source" yeArrSrc' <>
                      xattr "target" yeArrDst') <>
    xelem "y:BendStyle" (xattr "smoothed" "false") <>
    xelem "y:LineStyle" (xattr "color" yeColor <>
                         xattr "type" yeStyle <>
                         xattr "width" (show yeWidth)) <>
    case yeLabel of
      Just lbl -> xelem "y:EdgeLabel" $
                  xattr "textColor" yeLblColor <#>
                  xtext lbl
      Nothing -> mempty
  where
    isForward  = yeDir == Forward || yeDir == Both
    isBack     = yeDir == Back || yeDir == Both
    yeArrSrc'  = if isBack then fromMaybe "standard" yeArrSrc else "none"
    yeArrDst'  = if isForward then fromMaybe "standard" yeArrDst else "none"


convertDot :: FilePath -> IO ()
convertDot fn = do
  mm <- TIO.readFile fn
  let dg = DP.parseIt' mm :: DotGraph String
  B.writeFile (fn ++ ".graphml") $ xrender (convert dg)
