module App (component) where

import Prelude

import DOM.HTML.Indexed.InputType (InputType(..))
import Data.Array (foldMap, mapWithIndex, unsnoc)
import Data.Int (decimal, fromStringAs, toStringAs)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (joinWith)
import Data.String.CodeUnits (singleton, toCharArray)
import Data.Traversable (sequence)
import Effect.Class (class MonadEffect)
import Effect.Class.Console (log)
import Halogen as H
import Halogen.HTML (ClassName(..))
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties (StepValue(..))
import Halogen.HTML.Properties as HP
import Web.Event.Event (Event, target)
import Web.HTML.HTMLInputElement (fromEventTarget, setValue, value)

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

puzzleToString :: Array (Array (Maybe Int)) -> String
puzzleToString arr = joinWith "" $ (fromMaybe "." <<< (map (toStringAs decimal))) <$> (foldMap identity arr)

data Action
  = UpdateCell Int Int Event
  | SubmitPuzzle

handleAction :: forall o m. MonadEffect m => Action -> H.HalogenM State Action () o m Unit
handleAction = case _ of
  UpdateCell i j e -> do
    v <- H.liftEffect $ sequence (value <$> ((target e) >>= fromEventTarget))
    let
      v' = fromMaybe "" $ singleton <$> (_.last <$> (((unsnoc <<< toCharArray) <$> v >>= identity)))
      sanitized_v = (fromStringAs decimal v') >>= (\z -> if z == 0 then Nothing else Just z)
      updatePuzzle x row =
        if x == i then
          mapWithIndex (\y prev -> if y == j then sanitized_v else prev) row
        else
          row
    {-  
      NOTE: Hack because setting value of an input to the current value of the input results in
    duplication of the input instead of using the updated value. So we clear out the value by
    setting it to an empty string and then setting it to the current value. The second part is
    necessary because it clears out the input otherwise

      IMPORTANT: This must never be allowed to go out of sync with model since we're updating input
    value directly
    -}
    _ <- H.liftEffect $ sequence $ (setValue "") <$> ((target e) >>= fromEventTarget)
    _ <- H.liftEffect $ sequence $ (setValue $ fromMaybe "" $ (toStringAs decimal) <$> sanitized_v) <$> ((target e) >>= fromEventTarget)
    H.modify_ (\s -> s { puzzle = mapWithIndex updatePuzzle s.puzzle })

  SubmitPuzzle -> do
    puzzle <- H.gets _.puzzle
    H.liftEffect $ log $ puzzleToString puzzle

render :: forall cs m. State -> H.ComponentHTML Action cs m
render { puzzle } =
  HH.div
    [ HP.class_ $ ClassName $ "absolute h-96 w-96 my-auto mx-auto border-2 border-slate-500  "
        <> " inset-1/4 bg-blue-50"
    ]
    [ HH.div
        [ HP.class_ $ ClassName "relative h-full flex flex-col"
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
    , HH.div
        [ HP.class_ $ ClassName "p-2 flex justify-end" ]
        [ HH.button
            [ HP.class_ $ ClassName "mr-4 my-2 p-2 text-white text-lg bg-slate-500 rounded-md"
            , HE.onClick \_ -> SubmitPuzzle
            ]
            [ HH.text "Get Solution" ]
        ]
    ]
  where
  renderCell i j cell =
    HH.div
      [ HP.class_ $ ClassName "border-2 border-slate-200 text-center flex-1"
      ]
      [ HH.input
          [ HP.class_ $ ClassName "w-full h-full invalid:border-red-400"
          , HP.type_ InputNumber
          -- NOTE: not strictly needed
          , HP.value $ fromMaybe "" $ toStringAs decimal <$> cell
          , HP.max 9.0
          , HP.min 1.0
          , HP.step $ Step 1.0
          , HE.onInput $ UpdateCell i j
          ]
      ]

component :: forall q o m. MonadEffect m => H.Component q Unit o m
component = H.mkComponent
  { initialState
  , render
  , eval: H.mkEval H.defaultEval { handleAction = handleAction }
  }
