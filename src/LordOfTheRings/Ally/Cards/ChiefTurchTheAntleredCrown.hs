module LordOfTheRings.Ally.Cards.ChiefTurchTheAntleredCrown (chiefTurchTheAntleredCrown) where

import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Objective.CardDefs qualified as Cards
import LordOfTheRings.Outcome (Outcome (Lost))

newtype ChiefTurchTheAntleredCrown = ChiefTurchTheAntleredCrown AllyAttrs
  deriving anyclass IsAlly
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | The objective ally, under the first player's control from the start.
chiefTurchTheAntleredCrown :: AllyCard ChiefTurchTheAntleredCrown
chiefTurchTheAntleredCrown = ally ChiefTurchTheAntleredCrown Cards.chiefTurchTheAntleredCrown

-- | Immune to player card effects. Chief Turch does not exhaust to defend.
instance HasModifiersFor ChiefTurchTheAntleredCrown where
  getModifiersFor (ChiefTurchTheAntleredCrown a) = modifySelf a [ImmuneToPlayerCardEffects, DoesNotExhaustToDefend]

-- | If Chief Turch leaves play, the players lose the game: read off the window he leaves
-- play in, however he leaves.
instance HasAbilities ChiefTurchTheAntleredCrown where
  getAbilities (ChiefTurchTheAntleredCrown a) = [mkAbility a 2 $ forced $ LeftPlayWindow After (toTarget a)]

instance RunMessage ChiefTurchTheAntleredCrown where
  runMessage msg s@(ChiefTurchTheAntleredCrown a) = case msg of
    UseCardAbility _ (isSource a -> True) 2 _ -> do
      gameOver Lost
      pure s
    _ -> ChiefTurchTheAntleredCrown <$> runMessage msg a
