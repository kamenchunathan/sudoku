module App (component) where

import Prelude

import Data.Int (decimal, toStringAs)
import Data.Maybe (Maybe(..))
import Effect.Class (class MonadEffect)
import Halogen (ClassName(..))
import Halogen as H
import Halogen.HTML (ClassName(..))
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

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

data Action

handleAction :: forall o m. MonadEffect m => Action -> H.HalogenM State Action () o m Unit
handleAction _ = pure unit

render :: forall cs m. State -> H.ComponentHTML Action cs m
render { puzzle } =
  HH.div
    [ HP.class_ $ ClassName $ "absolute h-96 w-96 my-auto mx-auto border-2 border-slate-500 flex flex-col "
        <> " inset-1/4"
    ]
    ( map
        ( \row -> HH.div
            [ HP.id "row"
            , HP.class_ $ ClassName "flex flex-row flex-1"
            ]
            (map renderCell row)

        )
        puzzle
    )
  where
  renderCell cell =
    HH.div
      [ HP.class_ $ ClassName "border-2 border-slate-100"
      ]
      [ case cell of
          Just v -> HH.text $ toStringAs decimal v
          Nothing ->
            ( HH.input
                [ HP.class_ $ ClassName "w-full h-full"
                ]
            )
      ]

component :: forall q o m. MonadEffect m => H.Component q Unit o m
component = H.mkComponent
  { initialState
  , render
  , eval: H.mkEval H.defaultEval { handleAction = handleAction }
  }
