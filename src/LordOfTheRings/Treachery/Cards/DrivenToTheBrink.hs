module LordOfTheRings.Treachery.Cards.DrivenToTheBrink (drivenToTheBrink) where

import LordOfTheRings.Scenario.Scenarios.TheSiegeOfGondor.Helpers (corsairFleetResources)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype DrivenToTheBrink = DrivenToTheBrink TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Peril.
drivenToTheBrink :: TreacheryCard DrivenToTheBrink
drivenToTheBrink = treachery DrivenToTheBrink Cards.drivenToTheBrink

-- | When Revealed: Either immediately end the quest phase (without resolving the quest), or
-- assign X damage among questing characters you control. X is the number of resources on
-- The Corsair Fleet. The damage is offered only with some to assign and a questing
-- character to take it.
-- Shadow: Exhaust a character you control.
instance RunMessage DrivenToTheBrink where
  runMessage msg t@(DrivenToTheBrink attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      revealer <- getRevealer
      x <- corsairFleetResources
      questing <- selectCharacters (CommittedHero <> HeroControlledBy (PlayerWithId revealer)) (CommittedAlly <> AllyControlledBy (PlayerWithId revealer))
      chooseOrRunOneM revealer do
        questionLabeled "drivenToTheBrink.choose"
        causedBy attrs
        -- the quest phase's own messages are on the game's queue once the choice is made
        labeled "drivenToTheBrink.endQuestPhase" $ doStep 1 msg
        when (x > 0 && notNull questing) $ labeled "drivenToTheBrink.damage" $ dealDamageDividedAmong attrs revealer questing x
      pure t
    DoStep 1 (WhenRevealed (isTarget attrs -> True)) -> do
      endQuestPhaseWithoutResolving
      pure t
    ResolveShadowEffect cid _ pid _ | cid == attrs.cardId -> do
      chooseAndExhaust attrs pid 1
      pure t
    _ -> DrivenToTheBrink <$> runMessage msg attrs
