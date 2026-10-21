module LordOfTheRings.Ally.Cards.Wilyador (wilyador) where

import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Objective.CardDefs qualified as Cards
import LordOfTheRings.Outcome (Outcome (Lost))

newtype Wilyador = Wilyador AllyAttrs
  deriving anyclass (IsAlly)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | The objective ally, under the first player's control from the start.
wilyador :: AllyCard Wilyador
wilyador = ally Wilyador Cards.wilyador

-- | No attachments. Wilyador cannot be healed of more than 5 wounds by a single effect.
-- (Rhosgobel: While Rhosgobel is in the staging area, Wilyador cannot be healed.)
instance HasModifiersFor Wilyador where
  getModifiersFor (Wilyador a) = do
    rhosgobel <- selectAny (LocationInStagingArea <> LocationWithCardCode (toCardCode Locations.rhosgobel))
    modified a a $ [CannotHaveAttachments, HealLimit 5] <> [CannotBeHealed | rhosgobel]

-- | Forced: At the end of each round, Wilyador suffers 2 damage.
instance HasAbilities Wilyador where
  getAbilities (Wilyador a) = [mkAbility a 1 $ forced RoundEndsWindow]

-- | If Wilyador leaves play, the players have lost the game.
instance RunMessage Wilyador where
  runMessage msg s@(Wilyador a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      dealDamage a a 2
      pure s
    Discard (isTarget a -> True) -> do
      gameOver Lost
      pure s
    _ -> Wilyador <$> runMessage msg a
