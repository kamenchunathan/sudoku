module App (component) where

import Prelude

import Data.Array (index, mapWithIndex)
import Data.Int (decimal, fromStringAs, toStringAs)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Traversable (sequence)
import Effect (Effect)
import Effect.Aff.Compat (EffectFn1, runEffectFn1)
import Effect.Class (class MonadEffect)
import Effect.Class.Console (log)
import Halogen as H
import Halogen.HTML (ClassName(..))
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties (InputType(..))
import Halogen.HTML.Properties as HP
import Web.Event.Event (Event, target)
import Web.HTML.HTMLInputElement (fromEventTarget, value)

foreign import _consoleLog :: forall a. EffectFn1 a Unit

consoleLog :: forall a. a -> Effect Unit
consoleLog = runEffectFn1 _consoleLog

type State =
  { puzzle :: Array (Array (Maybe Int))
  }

initialState :: forall input. input -> State
initialState _ =
  { puzzle:
      [ [ Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing ]
      , [ Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing ]
      , [ Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing ]
      , [ Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing ]
      , [ Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing ]
      , [ Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing ]
      , [ Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing ]
      , [ Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing ]
      , [ Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing ]
      ]
  }

data Action =
  UpdateCell Int Int Event

handleAction :: forall o m. MonadEffect m => Action -> H.HalogenM State Action () o m Unit
handleAction = case _ of
  UpdateCell i j e -> do
    v <- H.liftEffect $ sequence (value <$> ((target e) >>= fromEventTarget))
    let
      updatePuzzle x row =
        if x == i then
          mapWithIndex (\y prev -> if y == j then (v >>= fromStringAs decimal) else prev) row
        else
          row
    H.modify_ (\s -> s { puzzle = mapWithIndex updatePuzzle s.puzzle })

render :: forall cs m. State -> H.ComponentHTML Action cs m
render { puzzle } =
  HH.div
    [ HP.class_ $ ClassName $ "absolute h-96 w-96 my-auto mx-auto border-2 border-slate-500 flex flex-col "
        <> " inset-1/4"
    ]
    ( mapWithIndex
        ( \i row -> HH.div
            [ HP.id "row"
            , HP.class_ $ ClassName "flex flex-row flex-1"
            ]
            (mapWithIndex (renderCell i) row)

        )
        puzzle
    )
  where
  renderCell i j cell =
    HH.div
      [ HP.class_ $ ClassName "border-2 border-slate-100 text-center flex-1"
      ]
      [ HH.input
          [ HP.class_ $ ClassName "w-full h-full"
          , HP.style ""
          , HP.type_ InputNumber
          , HP.value $ fromMaybe "" $ toStringAs decimal <$> cell
          , HE.onInput $ UpdateCell i j
          ]
      ]

component :: forall q o m. MonadEffect m => H.Component q Unit o m
component = H.mkComponent
  { initialState
  , render
  , eval: H.mkEval H.defaultEval { handleAction = handleAction }
  }
