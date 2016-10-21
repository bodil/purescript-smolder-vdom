module Text.Smolder.Renderer.VDOM (render) where

import Prelude
import Data.CatList as CatList
import Control.Monad.Eff (Eff)
import Data.Array (fromFoldable)
import Data.List (head)
import Data.Maybe (Maybe)
import Data.VirtualDOM (EventListener(On), with, text, h, VNode)
import Text.Smolder.Markup (EventHandler(EventHandler), MarkupM)
import Text.Smolder.Renderer.Util (Node(Text, Element), renderMarkup)

renderNode :: ∀ e l v. Node (v → Eff e Unit) → VNode e l v
renderNode (Element name props CatList.CatNil children) =
  h name props (renderNode <$> fromFoldable children)
renderNode (Element name props events children) =
  let handlers = convertHandler <$> events
      convertHandler (EventHandler name callback) = On name callback
  in with (h name props (renderNode <$> fromFoldable children)) (fromFoldable handlers)
renderNode (Text t) = text t

render :: ∀ a e l v. MarkupM (v → Eff e Unit) a → Maybe (VNode e l v)
render m = head $ renderNode <$> renderMarkup m
