module LordOfTheRings.Treachery.Cards.LustForTreasure (lustForTreasure) where

import LordOfTheRings.Scenario.Scenarios.FliesAndSpiders.Helpers (poisonOn)
import LordOfTheRings.Trait (Trait (Condition))
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype LustForTreasure = LustForTreasure TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

lustForTreasure :: TreacheryCard LustForTreasure
lustForTreasure = treachery LustForTreasure Cards.lustForTreasure

instance RunMessage LustForTreasure where
  runMessage msg t@(LustForTreasure a) = case msg of
    WhenRevealed (isTarget a -> True) -> do
      eachPlayer \pid -> do
        characters <- charactersControlledBy pid
        counts <- for characters \c -> do
          ordinary <- getAttachmentsOn c
          conditions <- attachedWithTrait Condition c
          poison <- poisonOn c
          pure $ length (ordNub $ ordinary <> conditions) + poison
        encounterRaisesThreat pid (sum counts)
      pure t
    _ -> LustForTreasure <$> runMessage msg a
