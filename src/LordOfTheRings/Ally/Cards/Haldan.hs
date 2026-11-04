module LordOfTheRings.Ally.Cards.Haldan (haldan) where

import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Objective.CardDefs qualified as Cards
import LordOfTheRings.Outcome (Outcome (Lost))
import LordOfTheRings.Placement (Placement (InPlayArea))

{- | Haldan, the objective-ally side of Woodmen Village (Journey Up the Anduin, Lost in
Mirkwood). Guarded, he is the captive of the enemy he is attached to ('guardCardWith');
under a player's control he is an ally.
-}
newtype Haldan = Haldan AllyAttrs
  deriving anyclass IsAlly
  deriving newtype (Show, Eq, ToJSON, Entity)

haldan :: AllyCard Haldan
haldan = ally Haldan Cards.haldan

-- | While there is an active location, Haldan does not exhaust to quest.
instance HasModifiersFor Haldan where
  getModifiersFor (Haldan a) = do
    active <- selectAny IsActiveLocation
    when active $ modifySelf a [DoesNotExhaustToCommit]

-- | If Haldan leaves play, the players lose the game.
instance HasAbilities Haldan where
  getAbilities (Haldan a) = [mkAbility a 1 $ forced $ LeftPlayWindow After (toTarget a)]

-- | The first player gains control of Haldan while he is free of encounters: as his guard
-- leaves play he goes to the first player then, as a guarded objective does, not exhausted.
instance RunMessage Haldan where
  runMessage msg h@(Haldan a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      gameOver Lost
      pure h
    RescueCharacter (isTarget a -> True) -> do
      lead <- getLead
      pure $ Haldan $ a & placementL .~ InPlayArea lead
    _ -> Haldan <$> runMessage msg a
