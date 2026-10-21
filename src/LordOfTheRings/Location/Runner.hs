module LordOfTheRings.Location.Runner (module LordOfTheRings.Location.Runner, module X) where

import LordOfTheRings.Card (CardDef (..), toCardDef)
import LordOfTheRings.Classes.HasQueue
import LordOfTheRings.Classes.RunMessage
import LordOfTheRings.Location.Types as X
import LordOfTheRings.Message
import LordOfTheRings.Prelude
import LordOfTheRings.Target
import LordOfTheRings.Token
import LordOfTheRings.Window

instance RunMessage LocationAttrs where
  runMessage msg attrs = case msg of
    PlaceLocation lid placement | lid == attrs.id -> pure $ attrs & placementL .~ placement
    PlaceTokens _ (isTarget attrs -> True) token n -> do
      let attrs' = attrs & tokensL %~ addTokens token n
      -- a location with as much progress as its quest points is explored
      when (token == Progress && attrs'.questPoints > 0 && attrs'.progress >= attrs'.questPoints)
        $ pushAll
          [ CheckWindows [mkWhen $ Explored attrs.id]
          , -- a location with victory points goes to the victory display
            if isJust (cdVictoryPoints $ toCardDef attrs) then AddToVictoryDisplay (toTarget attrs) else Discard (toTarget attrs)
          ]
      pure attrs'
    RemoveTokens _ (isTarget attrs -> True) token n -> do
      pure $ attrs & tokensL %~ subtractTokens token n
    _ -> pure attrs
