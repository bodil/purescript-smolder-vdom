module Test.Main where

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Timer (TIMER)
import DOM (DOM)
import DOM.Event.Types (Event)
import DOM.HTML (window)
import DOM.HTML.Types (htmlDocumentToParentNode)
import DOM.HTML.Window (document)
import DOM.Node.ParentNode (querySelector)
import DOM.Node.Types (elementToNode, Node)
import Data.Maybe (Maybe(Nothing), maybe)
import Data.Nullable (toMaybe)
import Data.Tuple (Tuple(Tuple))
import Data.VirtualDOM (patch)
import Data.VirtualDOM.DOM (api)
import Signal (sampleOn, runSignal, (~>), foldp, Signal)
import Signal.Channel (CHANNEL, Channel, subscribe, channel)
import Signal.Channel as Channel
import Signal.DOM (animationFrame)
import Text.Smolder.HTML (button, h1, div)
import Text.Smolder.HTML.Attributes (style)
import Text.Smolder.Markup (Markup, (!), (#!), text, on)
import Text.Smolder.Renderer.VDOM (render)
import Prelude hiding (div)

type State = Int

initialState :: State
initialState = 0

data Action = Noop | Pred | Succ

nat :: Int → String
nat 0 = "Z"
nat n = "S " <> nat (n - 1)

send :: ∀ e. Channel Action → Action → Event → Eff (channel :: CHANNEL | e) Unit
send actions action _ = Channel.send actions action

view :: ∀ e. Channel Action → State → Markup (Event → Eff (channel :: CHANNEL | e) Unit)
view actions state = div do
  h1 ! style ("color: rgb(" <> show (min (state * 8) 256) <> ",0,0)") $ text ("Number " <> nat state)
  button #! on "click" (send actions Pred) $ text "pred"
  button #! on "click" (send actions Succ) $ text "succ"

update :: Action → State → State
update action state = case action of
  Noop → state
  Pred → max (state - 1) 0
  Succ → state + 1

app :: ∀ e. Signal State → Channel Action → Node → Eff (dom :: DOM, channel :: CHANNEL, timer :: TIMER | e) Unit
app stateS actions target = do
  tick ← animationFrame
  runSignal $ (input (sampleOn tick stateS)) ~> write
  where
    input state = foldp go (Tuple Nothing Nothing) state
    go state (Tuple _ prev) = Tuple prev (render $ view actions state)
    write (Tuple prev next) = patch api target prev next

main :: ∀ e. Eff (console :: CONSOLE, dom :: DOM, channel :: CHANNEL, timer :: TIMER | e) Unit
main = do
  doc ← window >>= document >>= htmlDocumentToParentNode >>> pure
  target ← querySelector "#content" doc >>= toMaybe >>> map elementToNode >>> pure
  actions ← channel Noop
  let state = foldp update initialState $ subscribe actions
  maybe (log "No div#content found!") (app state actions) target
