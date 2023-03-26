module App (component) where

import Prelude

import Affjax (Error)
import Affjax.RequestBody as Body
import Affjax.ResponseFormat as AXRF
import Affjax.Web (Response)
import Affjax.Web as AX
import Config (apiUri)
import DOM.HTML.Indexed.InputType (InputType(..))
import Data.Argonaut (Json, caseJsonArray, caseJsonBoolean, caseJsonObject, caseJsonString)
import Data.Argonaut.Core as A
import Data.Array (fold, foldMap, foldr, head, length, mapWithIndex, splitAt, unsnoc)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Int (decimal, fromString, fromStringAs, toStringAs)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (Pattern(..), joinWith, split)
import Data.String.CodeUnits (singleton, toCharArray)
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Effect.Class.Console (log)
import Foreign.Object (lookup, toUnfoldable)
import Foreign.Object as Object
import Halogen as H
import Halogen.HTML (ClassName(..))
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties (StepValue(..))
import Halogen.HTML.Properties as HP
import Util (consoleLog, unsafeLog)
import Web.Event.Event (Event, target)
import Web.HTML.HTMLInputElement (fromEventTarget, setValue, value)

type Puzzle = Array (Array (Maybe Int))

data RemoteData e a
  = NotAsked
  | Err e
  | Ok a

type PuzzleResponse =
  { solvable :: Boolean
  , puzzles :: (Array Puzzle)
  }

type State =
  { puzzle :: Puzzle
  , solutions ::
      RemoteData String PuzzleResponse
  }

emptyPuzzle :: Puzzle
emptyPuzzle =
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

initialState :: forall input. input -> State
initialState _ =
  { puzzle: emptyPuzzle
  , solutions: NotAsked

  }

puzzleToString :: Puzzle -> String
puzzleToString arr = joinWith "" $ (fromMaybe "." <<< (map (toStringAs decimal))) <$> (foldMap identity arr)

puzzleStringToArray :: String -> Puzzle
puzzleStringToArray str =
  inner $ split (Pattern "") str
  where
  inner arr =
    if (length $ _.after $ splitAt 9 arr) == 0 then
      [ fromString <$> (_.before $ splitAt 9 arr) ]
    else
      [ fromString <$> (_.before $ splitAt 9 arr) ] <> (inner $ _.after $ splitAt 9 arr)

data Action
  = UpdateCell Int Int Event
  | SubmitPuzzle

handleAction
  :: forall o m
   . MonadEffect m
  => MonadAff m
  => Action
  -> H.HalogenM State Action () o m Unit
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
    res <- H.liftAff $ AX.post AXRF.json apiUri
      ( Just $ Body.json
          ( A.fromObject
              ( Object.fromFoldable
                  [ Tuple "puzzle" (A.fromString $ puzzleToString puzzle)
                  ]
              )
          )
      )
    news <- H.modify (\s -> s { solutions = responseToSolutions res })
    H.liftEffect $ consoleLog news

-- H.liftEffect $ consoleLog st

responseToSolutions :: Either Error (Response Json) -> RemoteData String PuzzleResponse
responseToSolutions (Left _) = Err "Something went wrong, Please try again"
responseToSolutions (Right { body }) =
  let
    responseObject = caseJsonObject
      { solvable: Nothing
      , puzzles: Nothing
      }
      ( \obj ->
          let
            solvable = lookup "solvable" obj >>= (caseJsonBoolean Nothing Just)
            puzzles = lookup "puzzles" obj >>=
              ( caseJsonArray Nothing
                  ( \s ->
                      Just $ foldMap (caseJsonString [] Array.singleton) s
                  )
              )
          in
            { solvable
            , puzzles: (map <<< map) puzzleStringToArray puzzles
            }
      )
      body

    toRemoteData { solvable: Just solvable, puzzles: Just puzzles } = Ok { solvable, puzzles }
    toRemoteData _ = Err "Something went wrong"

  -- _ = unsafeLog $ toRemoteData responseObject
  in
    toRemoteData responseObject

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

component :: forall q o m. MonadEffect m => MonadAff m => H.Component q Unit o m
component = H.mkComponent
  { initialState
  , render
  , eval: H.mkEval H.defaultEval { handleAction = handleAction }
  }
