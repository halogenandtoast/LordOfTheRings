module LordOfTheRings.Location.Cards.DoorsOfDurinTheRingGoesSouth (doorsOfDurinTheRingGoesSouth) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Outcome (Outcome (Lost))
import LordOfTheRings.Scenario.Scenarios.TheRingGoesSouth.Helpers (firstPlayerHasRingBearer)

newtype DoorsOfDurinTheRingGoesSouth = DoorsOfDurinTheRingGoesSouth LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

doorsOfDurinTheRingGoesSouth :: LocationCard DoorsOfDurinTheRingGoesSouth
doorsOfDurinTheRingGoesSouth = location DoorsOfDurinTheRingGoesSouth Cards.doorsOfDurinTheRingGoesSouth

{- | Immune to player card effects. Progress must be placed on each other active location
before it can be placed here: questing progress fills the other first, and what is left
comes here once it is explored. Progress cannot be placed here unless the first player
controls the Ring-bearer.
-}
instance HasModifiersFor DoorsOfDurinTheRingGoesSouth where
  getModifiersFor (DoorsOfDurinTheRingGoesSouth a) = do
    ringBearer <- firstPlayerHasRingBearer
    modifySelf a $ [ImmuneToPlayerCardEffects, TakesProgressAfterOtherActiveLocations] <> [ProgressLimit 0 | not ringBearer]

-- | If there are 9 damage tokens here, the players lose the game.
instance RunMessage DoorsOfDurinTheRingGoesSouth where
  runMessage msg l@(DoorsOfDurinTheRingGoesSouth a) = case msg of
    PlaceTokens _ (isTarget a -> True) Progress _ -> do
      others <- select (IsActiveLocation <> not_ (LocationWithId a.id))
      ringBearer <- firstPlayerHasRingBearer
      if notNull others || not ringBearer then pure l else DoorsOfDurinTheRingGoesSouth <$> runMessage msg a
    PlaceTokens _ (isTarget a -> True) Damage _ -> do
      a' <- runMessage msg a
      when (a'.damage >= 9) $ gameOver Lost
      pure $ DoorsOfDurinTheRingGoesSouth a'
    _ -> DoorsOfDurinTheRingGoesSouth <$> runMessage msg a
