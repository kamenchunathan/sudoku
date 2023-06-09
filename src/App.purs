module App (component) where

import Prelude

import DOM.HTML.Indexed.InputType (InputType(..))
import Data.Array (foldMap, length, mapWithIndex, splitAt, unsnoc)
import Data.Int (decimal, fromStringAs, toStringAs)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String.CodeUnits (singleton, toCharArray)
import Data.Traversable (sequence)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Effect.Class.Console (log)
import Halogen as H
import Halogen.HTML (ClassName(..))
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties (StepValue(..))
import Halogen.HTML.Properties as HP
import Util (unsafeLog)
import Web.Event.Event (Event, target)
import Web.HTML.HTMLInputElement (fromEventTarget, setValue, value)

foreign import solve :: Array Int -> Array (Array Int)

type Puzzle = Array (Array (Maybe Int))

-- TODO: duplication of state here look into editing the model
type PuzzleSolutionData =
  { solvable :: Boolean
  , puzzles :: (Array Puzzle)
  , currentPuzzle :: Maybe Int
  }

type State =
  { puzzle :: Puzzle
  , solutions :: Maybe PuzzleSolutionData
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
  , solutions: Nothing

  }

fromFlatArray :: Array Int -> Puzzle
fromFlatArray arr =
  if (length $ _.after $ splitAt 9 arr) == 0 then
    [ map Just $ _.before $ splitAt 9 arr ]
  else
    [ map Just $ _.before $ splitAt 9 arr ] <> (fromFlatArray $ _.after $ splitAt 9 arr)

data Action
  = UpdateCell Int Int Event
  | SubmitPuzzle
  | ClearPuzzle
  | PrevSolution
  | NextSolution

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
    H.modify_
      ( \s ->
          let
            sudoku_solutions = solve $ fromMaybe 0 <$> (foldMap identity s.puzzle)
            solvable = length sudoku_solutions > 0
            puzzles = fromFlatArray <$> sudoku_solutions
          in
            s
              { solutions = Just
                  { solvable
                  , puzzles
                  , currentPuzzle: if solvable then Just 0 else Nothing
                  }
              }
      )

  ClearPuzzle -> H.modify_ (\s -> s { puzzle = emptyPuzzle, solutions = Nothing })

  PrevSolution -> H.modify_
    ( \s ->
        case s.solutions of
          (Just (sols@{ currentPuzzle, puzzles })) ->
            s
              { solutions =
                  ( Just
                      ( sols { currentPuzzle = (wrappingSub (length puzzles) 1 <$> currentPuzzle) }
                      )
                  )
              }
          _ -> s
    )

  NextSolution ->
    H.modify_
      ( \s ->
          case s.solutions of
            (Just (sols@{ currentPuzzle, puzzles })) ->
              s
                { solutions =
                    ( Just
                        (sols { currentPuzzle = (wrappingAdd (length puzzles - 1) 1 <$> currentPuzzle) })
                    )
                }
            _ -> s
      )

wrappingAdd :: Int -> Int -> Int -> Int
wrappingAdd max x y =
  let
    res = x + y
    ret = if res > max then res - max else res
  in
    ret

wrappingSub :: Int -> Int -> Int -> Int
wrappingSub max x y =
  let
    res = y - x
    ret = if res < 0 then res + max else res
  in
    ret

render :: forall cs m. State -> H.ComponentHTML Action cs m
render { puzzle, solutions } =
  HH.div
    []
    [ HH.header
        [ HP.class_ $ ClassName "w-4/6 mx-auto py-12" ]
        [ HH.h1
            [ HP.class_ $ ClassName "text-3xl font-semibold text-center" ]
            [ HH.text "Sudoku Solver"
            ]
        , HH.p
            [ HP.class_ $ ClassName "text-center p-2" ]
            [ HH.text "Enter a sudoku and have it solved using the backtracking algorithm" ]
        ]
    , HH.div
        [ HP.class_ $ ClassName "absolute top-1/4 left-[22%]" ]
        [ HH.div
            [ HP.class_ $ ClassName "p-2 flex justify-between" ]
            [ HH.div
                []
                [ HH.div [ HP.class_ $ ClassName "h-8" ] []
                , renderPuzzle puzzle true false
                , renderButtons
                ]
            , HH.div [ HP.class_ $ ClassName "w-36" ] []
            , renderSolutions solutions
            ]
        ]
    ]

renderSolutions :: forall cs m. Maybe PuzzleSolutionData -> H.ComponentHTML Action cs m
renderSolutions Nothing =
  HH.div
    []
    [ HH.div [ HP.class_ $ ClassName "h-8" ] []
    , HH.div
        [ HP.class_ $ ClassName "w-96 h-96 flex flex-col justify-center bg-slate-400 rounded-md" ]
        [ HH.p
            [ HP.class_ $ ClassName "w-full text-center text-white text-xl" ]
            [ HH.text "Press `Get Solution` to get solutions"
            ]
        ]
    ]
renderSolutions (Just { solvable, puzzles, currentPuzzle }) =
  HH.div
    []
    [ HH.div
        [ HP.class_ $ ClassName "h-8" ]
        [ if solvable then
            HH.div
              [ HP.class_ $ ClassName "flex justify-between" ]
              [ HH.p
                  [ HP.class_ $ ClassName "inline-block" ]
                  [ HH.text $ (toStringAs decimal $ length puzzles) <> " solutions found" ]
              , HH.p
                  [ HP.class_ $ ClassName "inline-block underline text-lg font-semibold" ]
                  [ HH.text $ "Solution: " <> (toStringAs decimal $ fromMaybe 0 currentPuzzle + 1) ]
              ]
          else
            HH.div
              []
              [ HH.div [ HP.class_ $ ClassName "h-8" ] []
              , HH.div
                  [ HP.class_ $ ClassName "w-96 h-96 flex flex-col justify-center bg-slate-400 rounded-md" ]
                  [ HH.p
                      [ HP.class_ $ ClassName "w-5/6 mx-auto text-center text-white text-xl" ]
                      [ HH.text "There were no solutions found for that sudoku puzzle" ]
                  ]

              ]

        ]
    , HH.div
        []
        ( mapWithIndex
            (\i p -> renderPuzzle p false ((Just i) /= currentPuzzle))
            puzzles
        )
    , if solvable then renderSolutionButtons else HH.div_ []
    ]

renderSolutionButtons :: forall cs m. H.ComponentHTML Action cs m
renderSolutionButtons =
  HH.div
    [ HP.class_ $ ClassName "p-2 flex justify-end" ]
    [ HH.button
        [ HP.class_ $ ClassName "mr-4 my-2 py-2 px-4 text-white text-lg bg-slate-500 rounded-md"
        , HE.onClick \_ -> PrevSolution
        ]
        [ HH.text "Prev" ]
    , HH.button
        [ HP.class_ $ ClassName "mr-4 my-2 py-2 px-4 text-white text-lg bg-slate-500 rounded-md"
        , HE.onClick \_ -> NextSolution
        ]
        [ HH.text "Next" ]
    ]

renderButtons :: forall cs m. H.ComponentHTML Action cs m
renderButtons = HH.div
  [ HP.class_ $ ClassName "p-2 flex justify-end" ]
  [ HH.button
      [ HP.class_ $ ClassName "px-4 my-2 mr-4 text-white bg-slate-400 rounded-md"
      , HE.onClick \_ -> ClearPuzzle
      ]
      [ HH.text "Clear" ]
  , HH.button
      [ HP.class_ $ ClassName "mr-4 my-2 p-2 text-white text-lg bg-slate-500 rounded-md"
      , HE.onClick \_ -> SubmitPuzzle
      ]
      [ HH.text "Get Solution" ]
  ]

renderPuzzle :: forall cs m. Puzzle -> Boolean -> Boolean -> H.ComponentHTML Action cs m
renderPuzzle puzzle editable hidden =
  HH.div
    [ HP.class_ $ ClassName $ "h-96 w-96 my-auto mx-auto border-2 border-slate-500  "
        <> " bg-blue-50 rounded-md"
        <> display
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
    ]
  where
  display = if hidden then " hidden " else ""
  renderCell i j cell =
    HH.div
      [ HP.class_ $ ClassName "border-2 border-slate-200 text-center flex-1" ]
      [ if editable then HH.input
          [ HP.class_ $ ClassName "w-full h-full invalid:border-red-400"
          , HP.type_ InputNumber
          -- NOTE: not strictly needed
          , HP.value $ fromMaybe "" $ toStringAs decimal <$> cell
          , HP.max 9.0
          , HP.min 1.0
          , HP.step $ Step 1.0
          , HE.onInput $ UpdateCell i j
          ]
        else
          HH.p
            [ HP.class_ $ ClassName "p-2" ]
            [ HH.text $ fromMaybe "" $ toStringAs decimal <$> cell ]
      ]

component :: forall q o m. MonadEffect m => MonadAff m => H.Component q Unit o m
component = H.mkComponent
  { initialState
  , render
  , eval: H.mkEval H.defaultEval { handleAction = handleAction }
  }
