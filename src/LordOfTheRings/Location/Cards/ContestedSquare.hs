module LordOfTheRings.Location.Cards.ContestedSquare (contestedSquare) where

import LordOfTheRings.Helpers.ControlledLocation (returnToStaging)
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (InPlayArea, StagingArea))

newtype ContestedSquare = ContestedSquare LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

contestedSquare :: LocationCard ContestedSquare
contestedSquare = location ContestedSquare Cards.contestedSquare

-- | Surge.
-- Forced: After an enemy is defeated, place 1 progress on Contested Square if it is in the
-- staging area.
-- While you control Contested Square, it gains: "Forced: After a character you control is
-- destroyed, if you control Contested Square, return Contested Square to the staging area."
instance HasAbilities ContestedSquare where
  getAbilities (ContestedSquare a) =
    [mkAbility a 1 $ forced $ EnemyDefeatedWindow After | a.placement == StagingArea]
      <> [mkAbility a 2 $ forced $ OwnCharacterDestroyedWindow After pid | InPlayArea pid <- [a.placement]]

instance RunMessage ContestedSquare where
  runMessage msg l@(ContestedSquare a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ | a.placement == StagingArea -> do
      placeProgress a a 1
      pure l
    UseCardAbility _ (isSource a -> True) 2 _ | InPlayArea _ <- a.placement -> do
      returnToStaging a.id
      pure l
    _ -> ContestedSquare <$> runMessage msg a
